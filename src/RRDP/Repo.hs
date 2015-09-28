{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module RRDP.Repo where

import           Control.Applicative
import           Data.Map                   as M
import           Data.Maybe
import           Data.Set                   as S
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


serializeRepo :: Repository -> String -> SerializedRepo
serializeRepo r@(Repository snapshot _deltas) _repoUrlBase = SerializedRepo {
  snapshotS     = sSnapshot,
  deltaS        = sDeltas,
  notificationS = serializeNotification r _repoUrlBase sSnapshot sDeltas
} where
  sSnapshot = serializeSnapshot snapshot
  sDeltas   = fmap serializeDelta _deltas


serializedDelta :: SerializedRepo -> Int -> Maybe L.ByteString
serializedDelta serializedRepo deltaNumber = M.lookup deltaNumber $ deltaS serializedRepo

{-
Example:

  <notification xmlns="HTTP://www.ripe.net/rpki/rrdp" version="1" session_id="9df4b597-af9e-4dca-bdda-719cce2c4e28" serial="2">
    <snapshot uri="HTTP://rpki.ripe.net/rpki-ca/rrdp/EEEA7F7AD96D85BBD1F7274FA7DA0025984A2AF3D5A0538F77BEC732ECB1B068.xml"
             hash="EEEA7F7AD96D85BBD1F7274FA7DA0025984A2AF3D5A0538F77BEC732ECB1B068"/>
    <delta serial="2" uri="HTTP://rpki.ripe.net/rpki-ca/rrdp/198BD94315E9372D7F15688A5A61C7BA40D318210CDC799B6D3F9F24831CF21B.xml"
           hash="198BD94315E9372D7F15688A5A61C7BA40D318210CDC799B6D3F9F24831CF21B"/>
    <delta serial="1" uri="HTTP://rpki.ripe.net/rpki-ca/rrdp/8DE946FDA8C6A6E431DFE3622E2A3E36B8F477B81FAFCC5E7552CC3350C609CC.xml"
           hash="8DE946FDA8C6A6E431DFE3622E2A3E36B8F477B81FAFCC5E7552CC3350C609CC"/>
  </notification>
-}
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



serializeSnapshot :: Snapshot -> L.ByteString
serializeSnapshot (Snapshot snapshotDef publishes) = U.lazy $ XS.format $ XS.snapshotElem snapshotDef publishElements
  where
    publishElements = [XS.publishElem uri base64 Nothing | Publish uri base64 _ <- publishes]

serializeDelta :: Delta -> L.ByteString
serializeDelta (Delta deltaDef ps ws) = U.lazy $ XS.format $ XS.deltaElem deltaDef pElems wElems
  where
    pElems = [XS.publishElem uri base64 hash | Publish uri base64 hash <- ps]
    wElems = [XS.withdrawElem uri hash       | Withdraw uri hash <- ws]


{- Update repository based on incoming message -}
updateRepo :: Repository -> QMessage -> (Repository, RMessage)
updateRepo repo@(Repository
                  (Snapshot (SnapshotDef version sessionId (Serial serial)) existingPublishes)
                  existingDeltas) (Message (Version mVersion) pdus) =

  case reportErrors of
    [] -> (newRepo, reply)
    _  -> (repo, reply)

  where
    newSerial = serial + 1

    {- TODO That could be quite slow, think on making the map "persistent"
       in the current repository -}
    existingObjects  = M.fromList [ (uri, hash) | Publish uri (Base64 _ hash) _ <- existingPublishes ]
    existingHash uri = M.lookup uri existingObjects

    changes = [
      let
        withHashCheck uri queryHash storedHash f
            | queryHash == storedHash = f
            | otherwise = Wrong_ BadHash { passed = queryHash, stored = storedHash, uriW = uri }
        in
      case p of
        PublishQ uri base64 Nothing ->
          case existingHash uri of
            Nothing -> Add_ (uri, base64)
            Just _  -> Wrong_ $ CannotInsertExistingObject uri

        PublishQ uri base64 (Just hash) ->
          case existingHash uri of
            Nothing -> Wrong_ $ ObjectNotFound uri
            Just h  -> withHashCheck uri hash h $ Update_ (uri, base64, hash)

        WithdrawQ uri hash ->
          case existingHash uri of
            Nothing -> Wrong_ $ ObjectNotFound uri
            Just h  -> withHashCheck uri hash h $ Delete_ (uri, hash)

      | p <- pdus ]


    deltaP = catMaybes [
        case c of
          Add_ (uri, base64)          -> Just $ Publish uri base64 Nothing
          Update_ (uri, base64, hash) -> Just $ Publish uri base64 (Just hash)
          _                          -> Nothing
        | c <- changes ]

    deltaW = [ Withdraw uri hash | Delete_ (uri, hash) <- changes ]

    newUrls  = S.fromList $
                 [ uri | Publish uri _ _ <- deltaP ] ++
                 [ uri | Withdraw uri _  <- deltaW ]
    onlyOldObjects = Prelude.filter oldObject existingPublishes
      where oldObject (Publish uri _ _) = not $ uri `S.member` newUrls

    newSnapshotP = onlyOldObjects ++ deltaP

    newSnapshot  = Snapshot (SnapshotDef version sessionId $ Serial newSerial) newSnapshotP
    newDelta     = Delta (DeltaDef version sessionId (Serial newSerial) (ClientId "")) deltaP deltaW
    newDeltas    = M.insert newSerial newDelta existingDeltas
    newRepo      = Repository newSnapshot newDeltas

     -- generate reply
    reply        = Message (Version mVersion) (publishR ++ withdrawR ++ reportErrors) :: Message ReplyPdu
    publishR     = [ PublishR    uri  | Publish uri _ _ <- deltaP  ]
    withdrawR    = [ WithdrawR   uri  | Withdraw uri _  <- deltaW  ]
    reportErrors = [ ReportError err_ | Wrong_ err_      <- changes ]



-- Check some precoditions of the repository consistency
-- TODO Add check for contigous delta range
verifyRepo :: Repository -> Either RepoError Repository
verifyRepo r@(Repository (Snapshot (SnapshotDef version sessionId _) _) _deltas) =
  U.verify matchingSessionId NonMatchingSessionId r >>=
  U.verify (version == protocolVersion) (BadRRDPVersion version) >>=
  U.verify deltaVersion (BadRRDPVersion version)
  where
    protocolVersion = Version 1
    deltaList = M.elems _deltas
    matchingSessionId = Prelude.null [ sId | Delta (DeltaDef _ sId _ _) _ _ <- deltaList, sId /= sessionId ]
    deltaVersion      = Prelude.null [ v   | Delta (DeltaDef v _ _ _) _ _   <- deltaList, v   /= protocolVersion ]


{- Write one more delta and replace snapshot.xml -}
syncToFS :: Repository -> FilePath -> IO (Either RRDPError ())
syncToFS (Repository s@(Snapshot (SnapshotDef _ (SessionId sId) (Serial serial)) _) _deltas) repoDir = do
  _ <- writeDelta `catchIOError` \e -> return $ Left $ DeltaSyncError e
  writeSnapshot   `catchIOError` \e -> return $ Left $ SnapshotSyncError e
  where
    snapshotTmpName = sessionStoreDir </> "snapshot.xml.tmp"
    snapshotName    = sessionStoreDir </> "snapshot.xml"

    sessionStoreDir = repoDir </> T.unpack sId

    writeSnapshot = do
      L.writeFile snapshotTmpName $ serializeSnapshot s
      renameFile snapshotTmpName snapshotName
      return $ Right ()

    writeDelta = case deltaBytes of
      Just d  -> do
        createDirectoryIfMissing False $ sessionStoreDir </> show serial
        L.writeFile (sessionStoreDir </> show serial </> "delta.xml") d
        return $ Right ()
      Nothing -> return $ Left $ InconsistentSerial serial

    -- TODO: Think about getting rid of Maybe here and how to always
    -- have a delta
    deltaBytes  = serializeDelta <$> M.lookup serial _deltas


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do
  queryMessage               <- mapParseError $ XS.parseMessage queryXml
  let (newRepo, replyMessage) = updateRepo repo queryMessage
  return (newRepo, XS.createReply replyMessage)
  where
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r


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
          Just (ST.RepoObject { ST.base64 = Base64 _ h, ST.clientId = cId }) ->
            withClientIdCheck uri cId $ withHashCheck uri hash h $ Update_ (uri, base64)

      WithdrawQ uri hash ->
        case M.lookup uri uriMap of
          Nothing -> Wrong_ $ ObjectNotFound uri
          Just (ST.RepoObject { ST.base64 = Base64 _ h, ST.clientId = cId }) ->
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


-- TODO Need to handle errors
processMessage2 :: AcidState ST.Repo -> L.ByteString -> ClientId -> Either RRDPError (AcidState ST.Repo, L.ByteString)
processMessage2 repo queryXml clientId = do
    m@(Message version pdus) <- mapParseError $ XS.parseMessage queryXml
    Right (repo, "")
    where
      z = do
        (sessionId, serial) <- query' repo ST.GetInfo
        -- TODO Create real delta
        let delta = Delta (DeltaDef (Version 1) sessionId serial clientId) [] []
        as <- actions repo [] clientId
        update' repo (ST.ApplyActions clientId delta as)
        return (repo, "")

      mapParseError (Left e)  = Left $ BadMessage e
      mapParseError (Right r) = Right r


snapshotXmlAcid :: AcidState ST.Repo -> SnapshotDef -> IO RRDPResponse
snapshotXmlAcid repo snapshotDef = do
  ros <- query' repo ST.GetAllObjects
  let publishElements = [ XS.publishElem u b64 Nothing | ST.RepoObject { ST.uri = u, ST.base64 = b64 } <- ros ]
  return $ Right $ U.lazy $ XS.format $ XS.snapshotElem snapshotDef publishElements


deltaXmlAcid :: AcidState ST.Repo -> DeltaDef -> IO RRDPResponse
deltaXmlAcid repo dd@(DeltaDef _ sessionId serial _) = do
  d <- query' repo (ST.GetDelta serial)
  return $ case d of
    Just (Delta _ ps ws) -> Right $ U.lazy $ XS.format $ XS.deltaElem dd publishElements withdrawElements
      where
        publishElements  = [ XS.publishElem u b64 mHash | Publish u b64 mHash <- ps ]
        withdrawElements = [ XS.withdrawElem u hash | Withdraw u hash   <- ws ]
    Nothing -> Left $ NoDelta sessionId serial


---------
createNotification :: Repository -> String -> L.ByteString -> Map Int L.ByteString -> L.ByteString
createNotification (Repository (Snapshot sd@(SnapshotDef _ (SessionId sId) _) _) _deltas) _repoUrlBase sSnapshot sDeltas =
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


createSnapshot :: [ST.RepoObject] -> SnapshotDef -> L.ByteString
createSnapshot ros snapshotDef = U.lazy $ XS.format $ XS.snapshotElem snapshotDef publishElements
  where
    publishElements = [XS.publishElem u b64 Nothing | ST.RepoObject { ST.uri = u, ST.base64 = b64 } <- ros]
