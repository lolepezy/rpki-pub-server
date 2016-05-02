{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module RRDP.Repo where

import           Control.Concurrent
import qualified Control.Concurrent.AdvSTM  as AS
import           Control.Concurrent.STM     as S
import           Control.Exception.Base
import           Control.Monad
import           Data.Data                  (Typeable)
import           Data.Foldable
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Maybe
import           Data.Time.Clock
import qualified ListT                      as LT
import           Network.URI
import qualified STMContainers.Map          as TMap
import qualified STMContainers.Multimap     as TMMap

import qualified Data.Dequeue               as DQ

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T

import           Data.Acid
import           Data.Acid.Advanced         (query', update')

import           Data.UUID.V4               (nextRandom)

import           System.Directory
import           System.FilePath
import           System.IO.Error

import           Config
import qualified RRDP.XML                   as XS
import qualified Store                      as ST
import           Types
import qualified Util                       as U


type RRDPValue r = Either RRDPError r

type RRDPResponse = RRDPValue L.ByteString

type ChangeSet = [QueryPdu]

type RepoState  = [(URI, (Base64, ClientId))]
type TRepoMap   = TMap.Map URI (Base64, ClientId)
type TClientMap = TMMap.Multimap ClientId URI
type TChangeLog = (TMap.Map Integer ChangeSet, TVar Integer)

data TRepoState = TRepoState {
  rMap      :: TRepoMap,
  cMap      :: TClientMap,
  changeLog :: TChangeLog
}

data AppState = AppState {
  currentState :: TRepoState,
  acidRepo     :: AcidState ST.Repo,
  changeSync   :: TMVar ChangeSet,
  appConfig    :: AppConfig
}

data RollbackException = RollbackException [Action] deriving (Typeable, Show)

instance Exception RollbackException

tInsert :: TRepoState -> Base64 -> ClientId -> URI -> STM ()
tInsert TRepoState{..} base64 clientId uri = do
  TMap.insert (base64, clientId) uri rMap
  TMMap.insert uri clientId cMap

tDelete :: TRepoState -> ClientId -> URI -> STM ()
tDelete TRepoState{..} clientId uri = do
  TMap.delete uri rMap
  TMMap.delete uri clientId cMap


initialTRepo :: [ST.RepoObject] -> STM TRepoState
initialTRepo repoObjects = do
  (rmap, cmap) <- (,) <$> TMap.new <*> TMMap.new
  mapM_ (\(ST.RepoObject cId u b64) -> do
    TMap.insert (b64, cId) u rmap
    TMMap.insert u cId cmap) repoObjects

  logMap  <- TMap.new
  counter <- newTVar 0
  return TRepoState { rMap = rmap, cMap = cmap, changeLog = (logMap, counter) }


initialAppState :: AppConfig -> AcidState ST.Repo -> IO AppState
initialAppState appConf acid = do
  ros       <- query' acid ST.GetAllObjects
  (tRepo, syncV, fsSyncChan) <- atomically $ (,,) <$> initialTRepo ros <*> newEmptyTMVar <*> newTChan
  let appState = AppState {
    appConfig = appConf,
    acidRepo = acid,
    changeSync = syncV,
    currentState = tRepo
  }
  _ <- forkIO $ rrdpSyncThread fsSyncChan appState
  _ <- forkIO $ syncFSThread fsSyncChan appState

  return appState


{-
  TODO Support tag attribute in PDUs
  TODO Add proper logging
-}
processMessage :: AppState -> ClientId -> L.ByteString -> IO (AppState, Reply)
processMessage appState clientId queryXml =
  case XS.parseMessage queryXml of
    Left e                    -> return (appState, Errors [XMLError e])
    Right ListMessage         -> listObjects appState clientId
    Right (PduMessage _ pdus) -> applyActionsToState appState clientId pdus


listObjects :: AppState -> ClientId -> IO (AppState, Reply)
listObjects a @ AppState { currentState = TRepoState{..} } clientId = atomically $ do
  uris <- LT.toList $ TMMap.streamByKey clientId cMap
  objs <- mapM (\u -> (u,) <$> TMap.lookup u rMap) uris
  return (a, ListReply [ ListPdu u h | (u, Just (Base64 _ h, _)) <- objs ])


applyActionsToState :: AppState -> ClientId -> [QueryPdu] -> IO (AppState, Reply)
applyActionsToState appState @ AppState {
    currentState = repoState @ TRepoState{..},
    acidRepo = repo,
    changeSync = chSync }
    clientId pdus = do
  actions <- applyToState `catch` rollbackActions
  return (appState, reply actions)
  where
    applyToState = AS.atomically $ do
      actions <- AS.liftAdv $ tActionSeq clientId pdus repoState
      case [ e | Wrong_ e <- actions ] of
        [] -> do
          AS.liftAdv $ do
            updateChangeLog changeLog
            void $ notifySnapshotWritingThread pdus
          AS.onCommit $ void $ update' repo (ST.ApplyActions clientId actions)
        _ ->
          AS.liftAdv $ throwSTM $ RollbackException actions

      return actions

    rollbackActions (RollbackException as) = return as

    updateChangeLog :: TChangeLog -> STM ()
    updateChangeLog (changeMap, counter) = do
      c <- readTVar counter
      TMap.insert pdus c changeMap
      writeTVar counter (c + 1)

    notifySnapshotWritingThread = tryPutTMVar chSync

    reply actions = case [ e | Wrong_ e <- actions ] of
      [] -> Success
      as -> Errors as


stateSnapshot :: TRepoMap -> STM RepoState
stateSnapshot tmap = LT.toList (TMap.stream tmap)

changeLogSnapshot :: TChangeLog -> STM [(Integer, ChangeSet)]
changeLogSnapshot (chLog, _) = do
  chl <- LT.toList (TMap.stream chLog)
  return $ sortBy (compare `on` fst) chl


{-
  Create actions only until the first error to avoid
  redundant STM modify/rollback overhead.
 -}
tActionSeq :: ClientId -> [QueryPdu] -> TRepoState -> STM [Action]
tActionSeq clientId pdus repoState @ TRepoState{..} = go pdus
 where
  go :: [QueryPdu] -> STM [Action]
  go [] = return []
  go (pdu: otherPdus) = do
    action <- makeAction pdu
    case action of
      w@(Wrong_ _) -> return [w]
      a            -> applyActionToMap a >> (a : ) <$> go otherPdus


  applyActionToMap :: Action -> STM Action
  applyActionToMap a@(AddOrUpdate_ (uri, base64)) = const a <$> tInsert repoState base64 clientId uri
  applyActionToMap a@(Delete_ uri)                = const a <$> tDelete repoState clientId uri
  applyActionToMap a                              = return a

  makeAction :: QueryPdu -> STM Action
  makeAction p =
   let
     withClientIdCheck objUri sClientId f
         | sClientId == clientId = f
         | otherwise = Wrong_ PermissionFailure { oUri = objUri, storedClientId = sClientId, queryClientId = clientId }

     withHashCheck queryHash storedHash f
         | queryHash == storedHash = f
         | otherwise = Wrong_ $ NoObjectMatchingHash storedHash p

     lookupObject :: URI -> STM (Maybe (Base64, ClientId))
     lookupObject u = TMap.lookup u rMap
     in
     case p of
       QP (Publish uri base64 Nothing _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> AddOrUpdate_ (uri, base64)
           Just _  -> Wrong_ $ ObjectAlreadyPresent p

       QP (Publish uri base64 (Just hash) _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ NoObjectPresent uri hash
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck hash h $ AddOrUpdate_ (uri, base64)

       QW (Withdraw uri hash _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ NoObjectPresent uri hash
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck hash h $ Delete_ uri


rrdpSyncThread :: TChan (RepoState, ChangeSet) -> AppState -> IO ()
rrdpSyncThread syncChan AppState {
    currentState  = TRepoState{..},
    appConfig     = AppConfig { snapshotSyncPeriodOpt = syncPeriod },
    changeSync    = chSync
    } = do
      t0 <- getCurrentTime
      waitAndProcess t0
    where
      waitAndProcess :: UTCTime -> IO ()
      waitAndProcess t0 = do
        timestamp " time 0 = "

        -- wait until notified
        _ <- atomically $ takeTMVar chSync

        timestamp " time 1 = "
        t1      <- getCurrentTime
        if longEnough t0 t1 then do
          timestamp " time 2 = "
          doSyncAndWaitAgain t1
        else do
          timestamp " time 3 = "
          threadDelay $ round (1000 * 1000 * toRational (syncMinPeriod - diffUTCTime t1 t0))
          timestamp " time 4 = "
          doSyncAndWaitAgain t1
          timestamp " time 5 = "

      longEnough utc1 utc2 = diffUTCTime utc1 utc2 < syncMinPeriod

      doSyncAndWaitAgain newTime = do
        atomically $ do
          lastState <- stateSnapshot rMap
          clog      <- changeLogSnapshot changeLog
          let (chMap, _) = changeLog
          mapM_ ((`TMap.delete` chMap) . fst) clog
          writeTChan syncChan (lastState, concatMap snd clog)
        waitAndProcess newTime

      syncMinPeriod = fromInteger (toInteger syncPeriod) :: NominalDiffTime




-- temporary poorman logging until the proper one is used
timestamp :: String -> IO ()
timestamp s = do
  t <- getCurrentTime
  print $ s ++ show t


{-
  syncFSThread creates a thread that flushes the changes to FS.
-}

type DeltaDequeue = DQ.BankersDequeue ([QueryPdu], Integer, Hash, Serial)

data SyncFSData = SyncFSData {
  deltaDequeue   :: DeltaDequeue,
  totalDeltaSize :: Integer
}

syncFSThread :: TChan (RepoState, ChangeSet) -> AppState -> IO ()
syncFSThread syncChan appState @ AppState {
  appConfig = AppConfig {
      repositoryPathOpt = repoDir,
      oldDataRetainPeriodOpt = retainPeriod
    }
  } = do
  uuid <- nextRandom
  let currentSessionId = U.uuid2SessionId uuid
  _ <- scheduleFullCleanup currentSessionId
  _ <- scheduleOldSnapshotsCleanup currentSessionId
  go currentSessionId (Serial 1) (SyncFSData DQ.empty 0)

  where
    go sessionId serial syncData = do
      (lastState, pdus) <- atomically $ readTChan syncChan

      let snapshotXml = serializeSnapshot lastState $ SnapshotDef (Version 3) sessionId serial
          (snapshotSize, snapshotHash) = (U.length snapshotXml, U.getHash snapshotXml)

          deltaXml = serializeDelta $ Delta (DeltaDef (Version 3) sessionId serial) pdus
          (deltaSize, deltaHash) = (U.length deltaXml, U.getHash deltaXml)

          updated :: SyncFSData -> (SyncFSData, [Serial])
          updated (SyncFSData dts totalSize) =
            _updated (DQ.pushFront dts (pdus, deltaSize, deltaHash, serial)) (totalSize + deltaSize)
            where
              _updated ds ts
                | ts < snapshotSize = (SyncFSData ds ts, [])
                | otherwise = case DQ.popBack ds of
                  Just ((_, size, _, dSerial), ds') ->
                    let (syncData', toDelete) = _updated ds' (ts - size)
                    in (syncData', dSerial : toDelete)
                  Nothing -> (SyncFSData DQ.empty 0, [])

      let (newSyncData @ (SyncFSData deltas _), deltaSerialsToDelete) = updated syncData

      fs <- syncToFS (sessionId, serial) appState (snapshotXml, snapshotHash) (deltaXml, deltaHash) deltas

      mapM_ (scheduleDeltaRemoval sessionId) deltaSerialsToDelete

      go sessionId (U.nextS serial) newSyncData


    scheduleDeltaRemoval (SessionId sId) (Serial s) = forkIO $ do
      timestamp "scheduleDeltaRemoval 1: "
      threadDelay $ retainPeriod * 1000 * 1000
      timestamp "scheduleDeltaRemoval 2: "
      removeFile $ repoDir </> T.unpack sId </> show s </> "delta.xml"

    scheduleFullCleanup (SessionId sId) = forkIO $ do
      timestamp "scheduleFullCleanup 1: "
      threadDelay $ retainPeriod * 1000 * 1000
      timestamp "scheduleFullCleanup 2: "
      exists <- doesDirectoryExist repoDir
      when exists $ do
        dirs <- getDirectoryContents repoDir
        let filterOut = [".", "..", T.unpack sId, "notification.xml"]
        let sessionsToDelete = [ repoDir </> d | d <- dirs, d `notElem` filterOut]
        timestamp $ "sessionsToDelete = " ++ show sessionsToDelete
        mapM_ removeDirectoryRecursive sessionsToDelete

    scheduleOldSnapshotsCleanup (SessionId sId) = forkIO $ forever $ do
      timestamp "scheduleOldSnapshotsCleanup 1: "
      threadDelay $ retainPeriod * 1000 * 1000
      timestamp "scheduleOldSnapshotsCleanup 2: "
      let sessionDir = repoDir </> T.unpack sId
      exists <- doesDirectoryExist sessionDir
      when exists $ do
        allDirs <- filter (\d -> d `notElem` [".", ".."]) <$> getDirectoryContents sessionDir
        let maxSerial = maximum $ mapMaybe U.maybeInteger allDirs
        mapM_ (\d -> do
                let period = 3600 :: NominalDiffTime
                let snapshotFile = sessionDir </> d </> "snapshot.xml"
                ctime <- getModificationTime snapshotFile
                now   <- getCurrentTime
                -- don't delete the last one
                when (show maxSerial /= d && diffUTCTime now ctime < period) $ removeFile snapshotFile
              ) allDirs


syncToFS :: (SessionId, Serial) -> AppState -> (L.ByteString, Hash) -> (L.ByteString, Hash) -> DeltaDequeue -> IO (Either RRDPError ())
syncToFS (sessionId @ (SessionId sId), Serial s)
  AppState {
    appConfig = AppConfig {
      repositoryPathOpt = repoDir,
      repositoryBaseUrlOpt = repoUrl
    }} (snapshotXml, snapshotHash) (deltaXml, _) deltas = do
      createDirectoryIfMissing True storeDir
      _ <- writeLastSnapshot `catchIOError` \e -> return $ Left $ SnapshotSyncError e
      _ <- writeDelta `catchIOError` \e -> return $ Left $ DeltaSyncError e
      writeNotification `catchIOError` \e -> return $ Left $ NotificationSyncError e
  where
    storeDir = repoDir </> T.unpack sId </> show s
    notification = serializeNotification (sessionId, Serial s) repoUrl snapshotHash deltas

    writeLastSnapshot = write_ $ L.writeFile (storeDir </> "snapshot.xml") snapshotXml
    writeDelta        = write_ $ L.writeFile (storeDir </> "delta.xml") deltaXml

    writeNotification = write_ $ do
      let tmp = repoDir </> "notification.xml.tmp"
      L.writeFile tmp notification
      renameFile tmp (repoDir </> "notification.xml")

    write_ f = f >> return (Right ())



serializeNotification :: (SessionId, Serial) -> String -> Hash -> DeltaDequeue -> L.ByteString
serializeNotification (SessionId sId, Serial serial) _repoUrlBase snapshotHash deltas =
  U.lazy $ XS.format $ XS.notificationElem sd $ snapshotElem ++ deltaElems
  where
    sd = SnapshotDef (Version 3) (SessionId sId) (Serial serial)
    snapshotElem = [ snapshotDefElem sUri snapshotHash | sUri <- maybeToList $ snapshotUri serial ]
    deltaElems   = [ deltaDefElem u hash s | (_, _, hash, Serial s) <- toList deltas, u <- toList $ deltaUri s ]

    snapshotUri s = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/snapshot.xml"
    deltaUri s    = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/delta.xml"

    snapshotDefElem uri (Hash hash) = XS.mkElem "snapshot" [("uri", U.pack $ show uri), ("hash", U.strict hash)] []
    deltaDefElem uri (Hash hash) s  = XS.mkElem "delta"    [("uri", U.pack $ show uri), ("hash", U.strict hash), ("serial", U.pack $ show s)] []


serializeSnapshot :: RepoState -> SnapshotDef -> L.ByteString
serializeSnapshot ros snapshotDef = U.lazy $ XS.format $ XS.snapshotElem snapshotDef publishElements
  where
    publishElements = [ XS.publishElem u b64 Nothing | (u, (b64, _)) <- ros ]


serializeDelta :: Delta -> L.ByteString
serializeDelta (Delta deltaDef pdus) = U.lazy $ XS.format $ XS.deltaElem deltaDef elements
  where
    elements  = [ case pdu of
                    QP (Publish u b64 mHash _) -> XS.publishElem u b64 mHash
                    QW (Withdraw u hash _)     -> XS.withdrawElem u hash
                | pdu <- pdus ]
