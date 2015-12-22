{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module RRDP.Repo where

import           Control.Monad
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Map                   as M
import           Data.Maybe
import           Network.URI

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T

import           Data.Acid
import           Data.Acid.Advanced         (query', update')


import           System.Directory
import           System.FilePath
import           System.IO.Error

import qualified RRDP.XML                   as XS
import qualified Store                      as ST
import           Types
import           Config
import qualified Util                       as U


type RRDPValue r = Either RRDPError r

type RRDPResponse = RRDPValue L.ByteString

type DeltaMap = M.Map Int Delta

data Repository = Repository {
  snapshost :: Snapshot,
  deltas    :: DeltaMap
} deriving (Show)

-- Serialized representation of the repository
data SerializedRepo = SerializedRepo {
  snapshotS     :: L.ByteString,
  deltaS        :: M.Map Int L.ByteString,
  notificationS :: L.ByteString
} deriving (Show)

data AppState = AppState {
  sessions              :: Map SessionId Repository,
  currentSession        :: Repository,
  repoPath              :: String,
  repoUrlBase           :: String,
  serializedCurrentRepo :: SerializedRepo
} deriving (Show)


actionSeq :: ClientId -> [QueryPdu] -> Map URI ST.RepoObject -> [Action]
actionSeq clientId pdus uriMap = [
  let
    withClientIdCheck objUri sClientId f
        | sClientId == clientId = f
        | otherwise = Wrong_ CannotChangeOtherClientObject { oUri = objUri, storedClientId = sClientId, queryClientId = clientId }

    withHashCheck uri queryHash storedHash f
        | queryHash == storedHash = f
        | otherwise = Wrong_ BadHash { passed = queryHash, stored = storedHash, uriW = uri }
    in
    case p of
      PublishQ uri base64 Nothing ->
        case M.lookup uri uriMap of
          Nothing -> Update_ (uri, base64)
          Just _  -> Wrong_ $ CannotInsertExistingObject uri

      PublishQ uri base64 (Just hash) ->
        case M.lookup uri uriMap of
          Nothing -> Wrong_ $ ObjectNotFound uri
          Just ST.RepoObject { ST.base64 = Base64 _ h, ST.clientId = cId } ->
            withClientIdCheck uri cId $ withHashCheck uri hash h $ Update_ (uri, base64)

      WithdrawQ uri hash ->
        case M.lookup uri uriMap of
          Nothing -> Wrong_ $ ObjectNotFound uri
          Just ST.RepoObject { ST.base64 = Base64 _ h, ST.clientId = cId } ->
            withClientIdCheck uri cId $ withHashCheck uri hash h $ Delete_ uri

  | p <- pdus ]


actions :: AcidState ST.Repo -> [QueryPdu] -> ClientId -> IO [ObjOperation (URI, Base64) (URI, Base64) URI RepoError]
actions repo pdus clientId = do
    ros <- query' repo (ST.GetByURIs uris)
    let uriMap = M.fromList [ (u, ro) | ro @ ST.RepoObject { ST.uri = u } <- ros ]
    return $ actionSeq clientId pdus uriMap
  where
    uris = [ case p of
              PublishQ uri _ _ -> uri
              WithdrawQ uri _  -> uri
            | p <- pdus ]


sessionInfo :: MonadIO m => AcidState (EventState ST.GetInfo) -> m (EventResult ST.GetInfo)
sessionInfo repo = query' repo ST.GetInfo


syncThread :: AcidState ST.Repo -> AppConfig -> SyncFlag -> IO ()
syncThread repo appContext syncFlag = forever $ do
  _ <- readMVar syncFlag
  (sessionId@(SessionId sId), serial, objects, _) <- query' repo ST.GetRepo
  syncS <- syncSnapshot repo appContext
  syncS1 <- syncSnapshot repo appContext
  -- make it configurable
  threadDelay $ 10*1000*1000
  return ()

{-

 main thread:
    if (flagIsEmpty)
      tryPutMVar snapshotIsReady (sessionId, serial)


 sync thread:
   forever:
    (sessionId, serial) <- readMVar snapshotIsReady
    syncSnapshot sessionId serial
    threadDelay N seconds

-}



processMessage :: AcidState ST.Repo -> AppConfig -> ClientId -> SyncFlag -> L.ByteString -> IO (Either RRDPError (AcidState ST.Repo, L.ByteString))
processMessage repo appContext clientId syncFlag queryXml =
    case XS.parseMessage queryXml of
      Left err -> return $ Left $ BadMessage err
      Right (Message _ pdus) -> do
        (sessionId, serial) <- sessionInfo repo
        repoActions         <- actions repo pdus clientId

        let publishes = [ Publish u b h | PublishQ u b h <- pdus ]
        let withdraws = [ Withdraw u h  | WithdrawQ u h <- pdus  ]
        let delta = Delta (DeltaDef (Version 1) sessionId serial clientId) publishes withdraws

        let errors       = [ ReportError err | Wrong_ err <- repoActions ]
        let publishR     = [ PublishR    uri | Publish uri _ _ <- publishes  ]
        let withdrawR    = [ WithdrawR   uri | Withdraw uri _  <- withdraws  ]
        let replyMessage = Message (Version 1) (errors ++ publishR ++ withdrawR)

        -- TODO Process errors
        _ <- update' repo (ST.ApplyActions clientId delta repoActions)
                      -- `catchIOError` \e -> return $ Left $ DeltaSyncError e

        -- write delta file synchronously
        deltaResult <- syncDelta delta appContext

        _ <- notifySnapshotWritingThread

        return $ const (repo, XS.createReply replyMessage) <$> deltaResult
        where
          notifySnapshotWritingThread = liftIO $ tryPutMVar syncFlag True


syncDelta :: Delta -> AppConfig -> IO (Either RRDPError ())
syncDelta d@(Delta (DeltaDef _ (SessionId sId) (Serial s) _) _ _) AppConfig { repositoryPathOpt = repoDir } =
  writeDelta `catchIOError` \e -> return $ Left $ DeltaSyncError e
  where
    storeDir = repoDir </> T.unpack sId </> show s
    writeDelta = do
      createDirectoryIfMissing False storeDir
      L.writeFile (storeDir </> "delta.xml") $ serializeDelta d
      return $ Right ()


syncSnapshot :: AcidState ST.Repo -> AppConfig -> IO (Either RRDPError ())
syncSnapshot repo AppConfig { repositoryPathOpt = repoDir } =
  writeSnapshot `catchIOError` \e -> return $ Left $ SnapshotSyncError e
  where
    writeSnapshot = do
      (sessionId@(SessionId sId), serial, objects, _) <- query' repo ST.GetRepo
      let storeDir = repoDir </> T.unpack sId </> show serial
      createDirectoryIfMissing False storeDir
      L.writeFile (storeDir </> "snapshot.xml") $ serializeSnapshot objects $ SnapshotDef (Version 3) sessionId serial
      return $ Right ()



serializeNotification1 :: ST.StoredData -> String -> L.ByteString
serializeNotification1 (SessionId sId, Serial s, _, _) _repoUrlBase =
  U.lazy $ XS.format $ XS.notificationElem sd elements
  where
    elements = [ snapshotDefElem sUri (U.getHash $ U.lazy sSnapshot)
               | sUri <- maybeToList snapshotUri ] ++
               [ deltaDefElem   dUri (U.getHash $ U.lazy sDelta) serial
               | (serial, _) <- M.toList _deltas,
                  sDelta     <- maybeToList $ M.lookup serial sDeltas,
                  dUri       <- maybeToList $ deltaUri serial ]

    snapshotUri = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/snapshot.xml"
    deltaUri s  = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/delta.xml"

    snapshotDefElem uri (Hash hash)     = XS.mkElem "snapshot" [("uri", U.pack $ show uri), ("hash", U.strict hash)] []
    deltaDefElem uri (Hash hash) serial = XS.mkElem "delta" [("uri", U.pack $ show uri), ("hash", U.strict hash), ("serial", U.pack $ show serial)] []



serializeNotification :: Repository -> String -> L.ByteString -> Map Int L.ByteString -> L.ByteString
serializeNotification (Repository (Snapshot sd@(SnapshotDef _ (SessionId sId) _) _) _deltas) _repoUrlBase sSnapshot sDeltas =
  U.lazy $ XS.format $ XS.notificationElem sd elements
  where
    elements = [ snapshotDefElem sUri (U.getHash $ U.lazy sSnapshot)
               | sUri <- maybeToList snapshotUri ] ++
               [ deltaDefElem   dUri (U.getHash $ U.lazy sDelta) serial
               | (serial, _) <- M.toList _deltas,
                  sDelta     <- maybeToList $ M.lookup serial sDeltas,
                  dUri       <- maybeToList $ deltaUri serial ]

    snapshotUri = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/snapshot.xml"
    deltaUri s  = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/delta.xml"

    snapshotDefElem uri (Hash hash)     = XS.mkElem "snapshot" [("uri", U.pack $ show uri), ("hash", U.strict hash)] []
    deltaDefElem uri (Hash hash) serial = XS.mkElem "delta" [("uri", U.pack $ show uri), ("hash", U.strict hash), ("serial", U.pack $ show serial)] []


serializeSnapshot :: [ST.RepoObject] -> SnapshotDef -> L.ByteString
serializeSnapshot ros snapshotDef = U.lazy $ XS.format $ XS.snapshotElem snapshotDef publishElements
  where
    publishElements = [XS.publishElem u b64 Nothing | ST.RepoObject { ST.uri = u, ST.base64 = b64 } <- ros]


serializeDelta :: Delta -> L.ByteString
serializeDelta (Delta deltaDef ps ws) = U.lazy $ XS.format $ XS.deltaElem deltaDef publishElements withdrawElements
  where
    publishElements  = [ XS.publishElem u b64 mHash | Publish u b64 mHash <- ps ]
    withdrawElements = [ XS.withdrawElem u hash | Withdraw u hash <- ws ]
