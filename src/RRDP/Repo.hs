{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module RRDP.Repo where

import           Control.Concurrent
import qualified Control.Concurrent.AdvSTM  as AS
import           Control.Concurrent.STM     as S
import           Control.Exception.Base
import           Control.Monad
import           Data.Data                  (Typeable)
import           Data.Foldable
import           Data.Maybe
import           Data.Time.Clock
import qualified ListT                      as LT
import           Network.URI
import qualified STMContainers.Map          as TMap

import qualified Data.Dequeue               as DQ

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T

import           Data.Acid
import           Data.Acid.Advanced         (update', query')

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

type RepoState  = [(URI, (Base64, ClientId))]
type TRepoState = TMap.Map URI (Base64, ClientId)

data AppState = AppState {
  currentState  :: TRepoState,
  acidRepo      :: AcidState ST.Repo,
  changeSetSync :: TChan [QueryPdu],
  syncFSVar     :: MVar (RepoState, [QueryPdu]),
  appConfig     :: AppConfig
}

data RollbackException = RollbackException [Action] deriving (Typeable, Show)

instance Exception RollbackException


initAppState :: AppConfig -> AcidState ST.Repo -> IO AppState
initAppState appConf acid = do
  syncFS    <- newEmptyMVar
  ros       <- query' acid ST.GetAllObjects
  (m, chan) <- atomically $ (,) <$> stmMapFromList ros <*> newTChan
  let appState = AppState {
    appConfig = appConf,
    acidRepo = acid,
    changeSetSync = chan,
    syncFSVar = syncFS,
    currentState = m
  }
  _ <- forkIO $ rrdpSyncThread appState
  _ <- forkIO $ syncFSThread appState

  return appState
  where
    stmMapFromList list = do
      m <- TMap.new
      _ <- sequence_ [ TMap.insert (b64, cId) u m | ST.RepoObject cId u b64 <- list ]
      return m


{-
  TODO Implement <list> requests
  TODO Support tag attribute in PDUs
  TODO Adjust it to the latest standart version
-}
processMessage :: AppState -> ClientId -> L.ByteString -> IO (Either RRDPError (AppState, L.ByteString))
processMessage appState clientId queryXml =
  case XS.parseMessage queryXml of
    Left e              -> return $ Left $ BadMessage e
    Right parsedMessage -> applyActionsToState appState clientId parsedMessage


applyActionsToState :: AppState -> ClientId -> QMessage -> IO (Either RRDPError (AppState, L.ByteString))
applyActionsToState appState @ AppState {
    currentState = currentImMemoryState,
    acidRepo = repo,
    changeSetSync = changeQueue }
    clientId (Message _ pdus) = do
  actions <- applyToState `catch` rollbackActions
  return $ Right (appState, XS.createReply $ message actions)
  where
    applyToState = AS.atomically $ do
      actions <- AS.liftAdv $ tActionSeq clientId pdus currentImMemoryState
      let errors = [ ReportError err | Wrong_ err <- actions ]
      case errors of
        -- in case errors are present, rollback the transaction
        -- and return the actions
        (_:_) -> AS.liftAdv $ throwSTM $ RollbackException actions
        []    -> do
          -- notify the snapshot writing thread
          AS.liftAdv $ notifySnapshotWritingThread pdus
          AS.onCommit $ void $ update' repo (ST.ApplyActions clientId actions)

      return actions

    rollbackActions (RollbackException as) = return as

    message actions = Message (Version 1) responsePdus
      where responsePdus = [ case a of
                 AddOrUpdate_ (uri, _) -> PublishR uri
                 Delete_ uri           -> WithdrawR uri
                 Wrong_ err            -> ReportError err
              | a <- actions ]

    notifySnapshotWritingThread :: [QueryPdu] -> STM ()
    notifySnapshotWritingThread = writeTChan changeQueue


stateSnapshot :: TRepoState -> STM RepoState
stateSnapshot tmap = LT.toList (TMap.stream tmap)

{-
  Create actions only until the first error to avoid
  redundant STM modify/rollback overhead.
 -}
tActionSeq :: ClientId -> [QueryPdu] -> TRepoState -> STM [Action]
tActionSeq clientId pdus tReposState = go pdus
 where
  go :: [QueryPdu] -> STM [Action]
  go [] = return []
  go (pdu: otherPdus) = do
    action <- makeAction pdu
    case action of
      w@(Wrong_ _) -> return [w]
      a            -> applyActionToMap a >> (a : ) <$> go otherPdus


  applyActionToMap :: Action -> STM Action
  applyActionToMap a@(AddOrUpdate_ (uri, base64)) = const a <$> TMap.insert (base64, clientId) uri tReposState
  applyActionToMap a@(Delete_ uri)                = const a <$> TMap.delete uri tReposState
  applyActionToMap a@(Wrong_ _)                   = return a

  makeAction :: QueryPdu -> STM Action
  makeAction p =
   let
     withClientIdCheck objUri sClientId f
         | sClientId == clientId = f
         | otherwise = Wrong_ CannotChangeOtherClientObject { oUri = objUri, storedClientId = sClientId, queryClientId = clientId }

     withHashCheck uri queryHash storedHash f
         | queryHash == storedHash = f
         | otherwise = Wrong_ BadHash { passed = queryHash, stored = storedHash, uriW = uri }

     lookupObject :: URI -> STM (Maybe (Base64, ClientId))
     lookupObject u = TMap.lookup u tReposState
     in
     case p of
       QP (Publish uri base64 Nothing) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> AddOrUpdate_ (uri, base64)
           Just _  -> Wrong_ $ CannotInsertExistingObject uri

       QP (Publish uri base64 (Just hash)) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ ObjectNotFound uri
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck uri hash h $ AddOrUpdate_ (uri, base64)

       QW (Withdraw uri hash) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ ObjectNotFound uri
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck uri hash h $ Delete_ uri


rrdpSyncThread :: AppState -> IO ()
rrdpSyncThread AppState {
    currentState  = currentImMemoryState,
    appConfig     = AppConfig { snapshotSyncPeriodOpt = syncPeriod },
    changeSetSync = sync,
    syncFSVar     = syncFS } = do
      t0 <- getCurrentTime
      waitAndProcess t0
    where
      waitAndProcess :: UTCTime -> IO ()
      waitAndProcess t0 = do
        timestamp " time 0 = "
        (changes, lastState) <- atomically $ (,) <$> getCurrentChanContent sync <*> stateSnapshot currentImMemoryState
        timestamp " time 1 = "
        t1      <- getCurrentTime
        let pdus = concat changes
        if longEnough t0 t1 then do
          timestamp " time 2 = "
          doSyncWaitAgain lastState pdus t1
        else do
          timestamp " time 3 = "
          threadDelay $ round (1000 * 1000 * toRational (syncMinPeriod - diffUTCTime t1 t0))
          timestamp " time 4 = "
          doSyncWaitAgain lastState pdus t1
          timestamp " time 5 = "

      longEnough utc1 utc2 = diffUTCTime utc1 utc2 < syncMinPeriod

      doSyncWaitAgain lastState pdus newTime = do
        putMVar syncFS (lastState, pdus)
        waitAndProcess newTime

      getCurrentChanContent ch = do
        x     <- readTChan ch
        empty <- isEmptyTChan ch
        if empty then
          return [x]
        else
          (x :) <$> getCurrentChanContent ch

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

syncFSThread :: AppState -> IO ()
syncFSThread appState @ AppState {
  syncFSVar = syncFS,
  appConfig = AppConfig {
    repositoryPathOpt = repoDir,
    oldDataRetainPeriodOpt = retainPeriod
    }
  } = do
  uuid <- nextRandom
  let currentSessionId = U.uuid2SessionId uuid
  _ <- scheduleFullCleanup currentSessionId
  _ <- scheduleOldSnapshotsCleanup currentSessionId
  go currentSessionId (Serial 1) $ SyncFSData DQ.empty 0
  where
    go sessionId serial syncData = do
      timestamp "syncFSThread 1: "
      (lastState, pdus) <- takeMVar syncFS
      timestamp "syncFSThread 2: "

      let snapshotXml = serializeSnapshot lastState $ SnapshotDef (Version 3) sessionId serial
          (snapshotSize, snapshotHash) = (U.length snapshotXml, U.getHash snapshotXml)

          deltaXml = serializeDelta $ Delta (DeltaDef (Version 3) sessionId serial) pdus
          (deltaSize, deltaHash) = (U.length deltaXml, U.getHash deltaXml)

          updated :: SyncFSData -> (SyncFSData, [Serial])
          updated (SyncFSData deltas totalSize) =
            _updated (DQ.pushFront deltas (pdus, deltaSize, deltaHash, serial)) (totalSize + deltaSize)
            where
              _updated ds ts
                | ts < snapshotSize = (SyncFSData ds ts, [])
                | otherwise = case DQ.popBack ds of
                  Just ((_, size, _, dSerial), newDeltas) ->
                    let (newSyncData, toDelete) = _updated newDeltas (ts - size)
                    in (newSyncData, dSerial : toDelete)
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
        -- don't touch '.', '..' and the current session directory
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

    write_ f = do { _ <- f; return $ Right () }



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
                    QP (Publish u b64 mHash) -> XS.publishElem u b64 mHash
                    QW (Withdraw u hash)     -> XS.withdrawElem u hash
                | pdu <- pdus ]
