{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Repo.Rrdp where

import           Control.Concurrent
import           Control.Concurrent.STM     as S
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Time.Clock
import           Network.URI
import qualified STMContainers.Map          as TMap

import qualified Data.Dequeue               as DQ

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T

import           Data.UUID.V4               (nextRandom)

import           System.Directory
import           System.FilePath
import           System.IO.Error

import           Data.String.Interpolate

import           Config(AppConfig(..))
import qualified Log                        as LG
import           Repo.State
import           Types
import qualified Util                       as U
import qualified XML                        as XS


rrdpAccumulatorThread :: LG.Logger -> TChan (RepoState, ChangeSet) -> TMVar ChangeSet -> AppState -> IO ()
rrdpAccumulatorThread logger syncChan syncV
    AppState {
      currentState  = TRepoState{..},
      appConfig     = AppConfig { snapshotSyncPeriod = syncPeriod }
    } = do
      t0 <- getCurrentTime
      waitAndProcess t0
    where
      waitAndProcess :: UTCTime -> IO ()
      waitAndProcess t0 = do
        -- wait until notified
        _  <- atomically $ takeTMVar syncV
        t1 <- getCurrentTime
        if longEnough t0 t1 then
          doSyncAndWaitAgain t1
        else do
          threadDelay $ round (1000 * 1000 * toRational (syncMinPeriod - diffUTCTime t1 t0))
          doSyncAndWaitAgain t1

      longEnough utc1 utc2 = diffUTCTime utc1 utc2 < syncMinPeriod

      doSyncAndWaitAgain newTime = do
        LG.info logger $ LG.msg "Getting snapshot..."
        atomically $ do
          lastState <- stateSnapshot rMap
          clog      <- changeLogSnapshot changeLog
          let (chMap, _) = changeLog
          -- TODO That has to be abstracted out, use some "methods"
          mapM_ ((`TMap.delete` chMap) . fst) clog
          writeTChan syncChan (lastState, concatMap snd clog)

        waitAndProcess newTime

      syncMinPeriod = fromInteger (toInteger syncPeriod) :: NominalDiffTime

{-
  syncFSThread creates a thread that flushes the changes to FS.
-}

type DeltaDequeue = DQ.BankersDequeue ([QueryPdu], Integer, Hash, Serial)

data SyncFSData = SyncFSData {
  deltaDequeue   :: DeltaDequeue,
  totalDeltaSize :: Integer
}

syncFSThread :: LG.Logger -> TChan (RepoState, ChangeSet) -> AppState -> IO ()
syncFSThread logger syncChan AppState {
  appConfig = AppConfig {
      repositoryPath = repoDir,
      repositoryBaseUrl = repoUrl,
      oldDataRetainPeriod = retainPeriod
    }
  } = do
  uuid <- nextRandom
  let currentSessionId = U.uuid2SessionId uuid
  _ <- scheduleFullCleanup currentSessionId
  _ <- scheduleOldSnapshotsCleanup currentSessionId
  go currentSessionId (Serial 1) (SyncFSData DQ.empty 0)

  where
    info_ m = LG.info logger $ LG.msg m

    go :: SessionId -> Serial -> SyncFSData -> IO ()
    go sessionId @ (SessionId sId) serial @ (Serial se) syncData = do
      (lastState, pdus) <- atomically $ readTChan syncChan

      let snapshotXml = serializeSnapshot lastState $ SnapshotDef (Version 3) sessionId serial
          (snapshotSize, snapshotHash) = (U.lbslen snapshotXml, U.getHash snapshotXml)

          deltaXml = serializeDelta $ Delta (DeltaDef (Version 3) sessionId serial) pdus
          (deltaSize, deltaHash) = (U.lbslen deltaXml, U.getHash deltaXml)

          updatedDeltaQueue :: SyncFSData -> (SyncFSData, [Serial])
          updatedDeltaQueue (SyncFSData deltaQ totalSize) =
            updated' (DQ.pushFront deltaQ (pdus, deltaSize, deltaHash, serial)) (totalSize + deltaSize)
            where
              updated' ds ts
                | ts < snapshotSize = (SyncFSData ds ts, [])
                | otherwise = case DQ.popBack ds of
                  Just ((_, size, _, dSerial), ds') ->
                    let (syncData', toDelete) = updated' ds' (ts - size)
                    in (syncData', dSerial : toDelete)
                  Nothing -> (SyncFSData DQ.empty 0, [])

      let (newSyncData @ (SyncFSData deltas _), deltaSerialsToDelete) = updatedDeltaQueue syncData

      let
        storeDir = repoDir </> U.cs sId </> show se
        notification = serializeNotification (sessionId, serial) repoUrl snapshotHash deltas

        writeLastSnapshot = write_ $ L.writeFile (storeDir </> "snapshot.xml") snapshotXml
        writeDelta        = write_ $ L.writeFile (storeDir </> "delta.xml") deltaXml

        writeNotification = write_ $ do
          let tmp = repoDir </> "notification.xml.tmp"
          L.writeFile tmp notification
          renameFile tmp (repoDir </> "notification.xml")

        write_ f = f >> return (Right ())
        in void $ do
          createDirectoryIfMissing True storeDir
          void $ writeLastSnapshot `catchIOError` \e -> return $ Left $ SnapshotSyncError e
          unless (null deltas) $
            void $ writeDelta `catchIOError` \e -> return $ Left $ DeltaSyncError e
          writeNotification `catchIOError` \e -> return $ Left $ NotificationSyncError e


      mapM_ (scheduleDeltaRemoval sessionId) deltaSerialsToDelete

      go sessionId (U.nextS serial) newSyncData

    -- TODO Handle errors in these separate threads (log them)

    scheduleDeltaRemoval (SessionId sId) (Serial s) = forkIO $ do
      info_ [i|Scheduled removal of delta #{sId}  #{s} |]
      threadDelay $ retainPeriod * 1000 * 1000
      info_ $ "Removing delta " ++ U.cs sId ++ " " ++ show s
      removeFile $ repoDir </> T.unpack sId </> show s </> "delta.xml"

    scheduleFullCleanup (SessionId sId) = forkIO $ do
      info_ [i|Scheduled full cleanup except for session #{sId} |]
      threadDelay $ retainPeriod * 1000 * 1000
      info_ "Starting full clean up"
      exists <- doesDirectoryExist repoDir
      when exists $ do
        dirs <- getDirectoryContents repoDir
        let filterOut = [".", "..", T.unpack sId, "notification.xml"]
        let sessionsToDelete = [ repoDir </> d | d <- dirs, d `notElem` filterOut]
        info_ $ "Sessions to delete: " ++ show sessionsToDelete
        mapM_ removeDirectoryRecursive sessionsToDelete

    scheduleOldSnapshotsCleanup (SessionId sId) = forkIO $ forever $ do
      info_ [i|Scheduled expired snapshot cleanup in session #{sId}|]
      threadDelay $ retainPeriod * 1000 * 1000
      let sessionDir = repoDir </> T.unpack sId
      exists <- doesDirectoryExist sessionDir
      when exists $ do
        allDirs <- filter (\d -> d `notElem` [".", ".."]) <$> getDirectoryContents sessionDir
        -- don't delete the last one
        let dirsToDelete = case mapMaybe U.maybeInteger allDirs of
             [] -> []
             serials -> let m = show (maximum serials) in filter (/= m) allDirs
        mapM_ (\d -> do
            let snapshotFile = sessionDir </> d </> "snapshot.xml"
            ctime <- getModificationTime snapshotFile
            now   <- getCurrentTime
            when (diffUTCTime now ctime < (fromInteger . toInteger) retainPeriod) $ do
              info_ $ "Deleting snapshot " ++ snapshotFile
              snapshotExists <- doesFileExist snapshotFile
              when snapshotExists $ removeFile snapshotFile
          ) dirsToDelete


serializeNotification :: (SessionId, Serial) -> String -> Hash -> DeltaDequeue -> U.LBS
serializeNotification (SessionId sId, Serial serial) _repoUrlBase snapshotHash deltas =
  U.cs $ XS.format $ XS.notificationElem sd $ snapshotElem ++ deltaElems
  where
    sd = SnapshotDef (Version 3) (SessionId sId) (Serial serial)
    snapshotElem = [ snapshotDefElem sUri snapshotHash | sUri <- maybeToList $ snapshotUri serial ]
    deltaElems   = [ deltaDefElem u hash s | (_, _, hash, Serial s) <- toList deltas, u <- toList $ deltaUri s ]

    snapshotUri s = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/snapshot.xml"
    deltaUri s    = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/delta.xml"

    snapshotDefElem uri (Hash hash) = XS.mkElem "snapshot" [("uri", U.cs $ show uri), ("hash", U.cs hash)] []
    deltaDefElem uri (Hash hash) s  = XS.mkElem "delta"    [("uri", U.cs $ show uri), ("hash", U.cs hash), ("serial", U.cs $ show s)] []


serializeSnapshot :: RepoState -> SnapshotDef -> L.ByteString
serializeSnapshot ros snapshotDef = U.cs $ XS.format $ XS.snapshotElem snapshotDef publishElements
  where
    publishElements = [ XS.publishElem u b64 Nothing | (u, (b64, _)) <- ros ]


serializeDelta :: Delta -> L.ByteString
serializeDelta (Delta deltaDef pdus) = U.cs $ XS.format $ XS.deltaElem deltaDef elements
  where
    elements  = [ case pdu of
                    QP (Publish u b64 mHash _) -> XS.publishElem u b64 mHash
                    QW (Withdraw u hash _)     -> XS.withdrawElem u hash
                | pdu <- pdus ]
