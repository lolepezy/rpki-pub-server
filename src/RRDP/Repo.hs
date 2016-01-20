{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module RRDP.Repo where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Exception.Base
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Map                   as M
import           Data.List                  (sortBy)
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


processMessage :: AcidState ST.Repo -> AppConfig -> ClientId -> SyncFlag -> L.ByteString -> IO (Either RRDPError (AcidState ST.Repo, L.ByteString))
processMessage repo _ clientId syncFlag queryXml = runEitherT $ do
  (Message _ pdus)       <- hoistEither $ U.leftmap BadMessage $ XS.parseMessage queryXml
  (sessionId, serial, _) <- sessionInfo repo
  repoActions            <- tryIO DeltaSyncError $ actions repo pdus clientId

  let publishes = [ Publish u b h | PublishQ u b h <- pdus ]
  let withdraws = [ Withdraw u h  | WithdrawQ u h <- pdus  ]
  let delta = Delta (DeltaDef (Version 1) sessionId serial clientId) publishes withdraws

  let errors       = [ ReportError err | Wrong_ err      <- repoActions ]
  let publishR     = [ PublishR    uri | Publish uri _ _ <- publishes  ]
  let withdrawR    = [ WithdrawR   uri | Withdraw uri _  <- withdraws  ]
  let replyMessage = Message (Version 1) (errors ++ publishR ++ withdrawR)

  _ <- tryIO DeltaSyncError $ update' repo (ST.ApplyActions clientId delta repoActions)
  _ <- tryIO DeltaSyncError notifySnapshotWritingThread

  return (repo, XS.createReply replyMessage)
  where
    notifySnapshotWritingThread = tryPutMVar syncFlag True
    tryIO anError io = EitherT $ U.leftmap anError <$> tryIOError io


syncThread :: AcidState ST.Repo -> AppConfig -> SyncFlag -> IO ()
syncThread repo appContext syncFlag = forever $ do
  _ <- readMVar syncFlag
  repoState <- query' repo ST.GetRepo
  syncS <- syncToFS repoState appContext
  let result = case syncS of
        Left e ->
          -- log the message and complain
          return ()
        Right r ->
          void $ update' repo ST.MarkSync

  -- TODO make it configurable
  threadDelay $ 10*1000*1000
  result


syncToFS :: ST.RepoState -> AppConfig -> IO (Either RRDPError ())
syncToFS repoState@(sessionId@(SessionId sId), serial, objects, deltas, latestSerial)
         AppConfig { repositoryPathOpt = repoDir, repositoryBaseUrlOpt = repoUrl } = do
  writeLastSnapshot `catchIOError` \e -> return $ Left $ SnapshotSyncError e
  writeDeltas `catchIOError` \e -> return $ Left $ DeltaSyncError e
  writeNotification `catchIOError` \e -> return $ Left $ NotificationSyncError e
  where
    snapshotDef = SnapshotDef (Version 3) sessionId serial
    snapshotXml = serializeSnapshot objects snapshotDef
    storeDir = repoDir </> T.unpack sId </> show serial

    writeLastSnapshot = do
      createDirectoryIfMissing False storeDir
      L.writeFile (storeDir </> "snapshot.xml") snapshotXml
      return $ Right ()

    writeDeltas = do
        let notSynchedDeltas = sortBy deltaOrder $ Prelude.filter (`notSyncedYet` latestSerial) deltas
        Right <$> mapM_ (\d -> do
                          let deltaDir = repoDir </> T.unpack sId </> show serial
                          createDirectoryIfMissing False deltaDir
                          L.writeFile (storeDir </> "delta.xml") $ serializeDelta d)
                       notSynchedDeltas

    writeNotification = do
      L.writeFile (repoDir </> "notification.xml.tmp") $ serializeNotification repoState snapshotXml repoUrl
      renameFile (repoDir </> "notification.xml.tmp") (repoDir </> "notification.xml")
      return $ Right ()

    notSyncedYet (Delta (DeltaDef _ _ (Serial s) _) _ _) (Serial lastSync) = s > lastSync
    deltaOrder (Delta (DeltaDef _ _ (Serial s1) _) _ _)
               (Delta (DeltaDef _ _ (Serial s2) _) _ _) = compare s1 s2



serializeNotification :: ST.RepoState -> L.ByteString -> String -> L.ByteString
serializeNotification repoState@(sessionId@(SessionId sId), Serial serial, _, deltas, _) snapshotXml _repoUrlBase =
  U.lazy $ XS.format $ XS.notificationElem sd elements
  where
    sd = SnapshotDef (Version 3) sessionId (Serial serial)
    elements = [ snapshotDefElem sUri (U.getHash $ U.lazy snapshotXml)
               | sUri <- maybeToList $ snapshotUri serial ] ++
               [ deltaDefElem (deltaUri s) (U.getHash . U.lazy $ serializeDelta d) s
               | d@(Delta (DeltaDef _ _ s _) _ _) <- deltas ]

    snapshotUri s = parseURI $ _repoUrlBase ++ "/" ++ T.unpack sId ++ "/" ++ show s ++ "/snapshot.xml"
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
