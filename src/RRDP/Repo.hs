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
import           Data.Acid.Advanced         (update')

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
  changeSetSync :: Chan (RepoState, [QueryPdu]),
  syncFSVar     :: MVar (RepoState, [QueryPdu]),
  appConfig     :: AppConfig
}

data RollbackException = RollbackException [Action] deriving (Typeable, Show)

instance Exception RollbackException


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
      {- TODO That can have terrible performance as it reads the whole map
         and calls readTVar a lot of times.
      -}
      immutableState <- AS.liftAdv $ stateSnapshot currentImMemoryState
      let errors = [ ReportError err | Wrong_ err <- actions ]
      case errors of
        -- in case errors are present, rollback the transaction
        -- and return the actions
        (_:_) -> AS.liftAdv $ throwSTM $ RollbackException actions
        []    -> AS.onCommit $ do
          -- TODO Handle errors here somehow
          void $ update' repo (ST.ApplyActions clientId actions)
          -- notify the snapshot writing thread
          notifySnapshotWritingThread (immutableState, pdus)

      return actions

    rollbackActions (RollbackException as) = return as

    message actions = Message (Version 1) (errors ++ publishR ++ withdrawR)
      where
        errors       = [ ReportError err | Wrong_ err       <- actions ]
        publishR     = [ PublishR    uri | QP (Publish uri _ _) <- pdus ]
        withdrawR    = [ WithdrawR   uri | QW (Withdraw uri _)  <- pdus ]

    notifySnapshotWritingThread :: (RepoState, [QueryPdu]) -> IO ()
    notifySnapshotWritingThread = writeChan changeQueue


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
    changeSetSync = sync,
    syncFSVar = syncFS } = do
      t0 <- getCurrentTime
      waitAndProcess t0
    where
      waitAndProcess :: UTCTime -> IO ()
      waitAndProcess t0 = do
        timestamp " time 0 = "
        changes <- getCurrentChanContent sync
        timestamp " time 1 = "
        t1      <- getCurrentTime
        let pdus = concatMap snd changes
        let lastState = last $ map fst changes
        if longEnough t0 t1 then do
          timestamp " time 2 = "
          doSyncWaitAgain lastState pdus t1
        else do
          timestamp " time 3 = "
          threadDelay $ round (1000 * 1000 * toRational (syncMinPeriod - diffUTCTime t1 t0))
          timestamp " time 4 = "
          doSyncWaitAgain lastState pdus t1
          timestamp " time 5 = "

      -- TODO Make these 10 seconds configurable
      longEnough utc1 utc2 = diffUTCTime utc1 utc2 < syncMinPeriod

      doSyncWaitAgain lastState pdus newTime = do
        putMVar syncFS (lastState, pdus)
        waitAndProcess newTime

      getCurrentChanContent ch = do
        empty <- isEmptyChan ch
        if empty then
          return []
        else do
          x <- readChan ch
          (x :) <$> getCurrentChanContent ch

      timestamp s = do
        t <- getCurrentTime
        print $ s ++ show t

      syncMinPeriod = 10 :: NominalDiffTime


{-
  syncFSThread creates a thread that flushes the changes to FS.
-}

type DeltaDequeue = DQ.BankersDequeue ([QueryPdu], Integer, Hash, Serial)

data SyncFSData = SyncFSData {
  deltaDequeue   :: DeltaDequeue,
  totalDeltaSize :: Integer
}

syncFSThread :: AppState -> IO ()
syncFSThread appState @ AppState { syncFSVar = syncFS } = do
  uuid <- nextRandom
  go (U.uuid2SessionId uuid) (Serial 1) $ SyncFSData DQ.empty 0
  where
    go sessionId serial syncData = do
      (lastState, pdus) <- readMVar syncFS

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

      mapM_ (forkIO . scheduleDeltaRemoval) deltaSerialsToDelete

      go sessionId (U.nextS serial) newSyncData
      where
        scheduleDeltaRemoval (Serial s) = void $ do
          -- sleep for an hour before removing
          threadDelay $ 3600*1000*1000
          -- delete the delta with this serial
          return ()




syncToFS :: (SessionId, Serial) -> AppState -> (L.ByteString, Hash) -> (L.ByteString, Hash) -> DeltaDequeue -> IO (Either RRDPError ())
syncToFS (sessionId @ (SessionId sId), serial)
  AppState {
    appConfig = AppConfig {
      repositoryPathOpt = repoDir,
      repositoryBaseUrlOpt = repoUrl
    }} (snapshotXml, snapshotHash) (deltaXml, _) deltas = do
      createDirectoryIfMissing False storeDir
      _ <- writeLastSnapshot `catchIOError` \e -> return $ Left $ SnapshotSyncError e
      _ <- writeDelta `catchIOError` \e -> return $ Left $ DeltaSyncError e
      writeNotification `catchIOError` \e -> return $ Left $ NotificationSyncError e
  where
    storeDir = repoDir </> T.unpack sId </> show serial
    notification = serializeNotification (sessionId, serial) repoUrl snapshotHash deltas

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
    deltaElems = map (\(_, _, hash, s) -> deltaDefElem (deltaUri s) hash s) $ toList deltas

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


stmMapFromList :: TMap.Key k => [(k,v)] -> STM (TMap.Map k v)
stmMapFromList list = do
  m <- TMap.new
  _ <- sequence_ [ TMap.insert object url m | (url, object) <- list ]
  return m
