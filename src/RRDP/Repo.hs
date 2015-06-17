{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE OverloadedStrings  #-}

module RRDP.Repo where

import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Set as S
import Network.URI

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import System.Directory
import System.Directory.Tree
import System.FilePath
import System.IO.Error

import Types
import qualified Util as U
import qualified RRDP.XMLStream as XS

type RRDPValue r = Either RRDPError r

type RRDPResponse = RRDPValue L.ByteString

type DeltaMap = M.Map Int Delta

data Repository = Repository {
  snapshost :: Snapshot,
  deltas :: DeltaMap
} deriving (Show)

-- Serialized representation of the repository
data SerializedRepo = SerializedRepo {
  snapshotS :: L.ByteString,
  deltaS :: M.Map Int L.ByteString,
  notificationS :: L.ByteString
} deriving (Show)

data AppState = AppState {
  sessions :: Map SessionId Repository,
  currentSession :: Repository,
  repoPath :: String,
  repoUrlBase :: String,
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


-- private utility type to make logic easier to understand
data ObjOperation a u d w = Add a | Update u | Delete d | Wrong w

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
            | otherwise = Wrong $ BadHash { passed = queryHash, stored = storedHash, uriW = uri }
        in
      case p of
        PublishQ uri base64 Nothing ->
          case existingHash uri of
            Nothing -> Add (uri, base64)
            Just _  -> Wrong $ CannotInsertExistingObject uri

        PublishQ uri base64 (Just hash) ->
          case existingHash uri of
            Nothing -> Wrong $ ObjectNotFound uri
            Just h  -> withHashCheck uri hash h $ Update (uri, base64, hash)
        WithdrawQ uri hash ->
          case existingHash uri of
            Nothing -> Wrong $ ObjectNotFound uri
            Just h  -> withHashCheck uri hash h $ Delete (uri, hash)

      | p <- pdus ]


    deltaP = [ c | Just c <- [
        case c of
          Add (uri, base64)          -> Just $ Publish uri base64 Nothing
          Update (uri, base64, hash) -> Just $ Publish uri base64 (Just hash)
          _                          -> Nothing
        | c <- changes ]]

    deltaW = [ Withdraw uri hash | Delete (uri, hash) <- changes ]

    newUrls  = S.fromList $
                 [ uri | Publish uri _ _ <- deltaP ] ++
                 [ uri | Withdraw uri _  <- deltaW ]
    onlyOldObjects = Prelude.filter oldObject existingPublishes
      where oldObject (Publish uri _ _) = not $ uri `S.member` newUrls

    newSnapshotP = onlyOldObjects ++ deltaP

    newSnapshot  = Snapshot (SnapshotDef version sessionId $ Serial newSerial) newSnapshotP
    newDelta     = Delta (DeltaDef version sessionId $ Serial newSerial) deltaP deltaW
    newDeltas    = M.insert newSerial newDelta existingDeltas
    newRepo      = Repository newSnapshot newDeltas

     -- generate reply
    reply        = Message (Version mVersion) (publishR ++ withdrawR ++ reportErrors) :: Message ReplyPdu
    publishR     = [ PublishR    uri  | Publish uri _ _ <- deltaP  ]
    withdrawR    = [ WithdrawR   uri  | Withdraw uri _  <- deltaW  ]
    reportErrors = [ ReportError err_ | Wrong err_      <- changes ]



{-
  Repository schema:

  notification.xml
  /x-session-id-1/snapshot.xml
  /x-session-id-1/1/delta.xml
  /x-session-id-1/2/delta.xml
  ...
  /session-id-1 -> x-session-id-1
  ...
  /x-session-id-2/snapshot.xml
  /x-session-id-2/1/delta.xml
  ...

-}
readRepoFromFS :: AppOptions -> SessionId -> IO (Either RepoError AppState)
readRepoFromFS (AppOptions {
                  repositoryPathOpt = _repoPath,
                  repositoryBaseUrlOpt = _urlBase
                }) currentSessionId =

  {- Read the whole directory content, each directory corresponds to a session,
     in most practical cases there will be only one session -}
  readRepo =<< readDirectoryWith L.readFile _repoPath

  where

    readRepo :: AnchoredDirTree L.ByteString -> IO (Either RepoError AppState)
    readRepo (_ :/ Dir { contents = repoDirContent } ) = do
      -- read sessions and separate successfull ones from broken ones.
      let potentialRepositories = [ U.leftmap (sId,) $ fmap (sId,) $ readSession sId dir
                                  | dir@Dir { name = sessionId } <- repoDirContent, let sId = SessionId $ T.pack sessionId ]

      {- we don't enforce a restriction for all sessions to be valid,
         so we only bail out in case of no valid sessions -}
      return $ case partitionEithers potentialRepositories of
                 ([],       []) -> Left $ CouldNotReadRepoDirectory _repoPath
                 (badRepos, []) -> Left $ RepoESeq $ Prelude.map snd badRepos
                 (_, goodRepos) -> createAppState goodRepos currentSessionId

    -- it's not a directory (a file or a failure)
    readRepo _ = return $ Left $ CouldNotReadRepoDirectory _repoPath

    createAppState :: [(SessionId, Repository)] -> SessionId -> Either RepoError AppState
    createAppState repos sId = U.maybeToEither (NotFoundSessionWithId sId) $
      fmap (\cs -> AppState {
           sessions              = sessionMap,
           currentSession        = cs,
           serializedCurrentRepo = serializeRepo cs _urlBase,
           repoPath              = _repoPath,
           repoUrlBase           = _urlBase
        }) current
        where
          sessionMap = M.fromList repos
          current    = M.lookup sId sessionMap

    -- read one directory and try to find snapshot and the sequence of deltas in it
    readSession :: SessionId -> DirTree L.ByteString -> Either RepoError Repository
    readSession _ (File _ _) = Left $ NonFoundRepoDirectory $ "Wrong path " ++ _repoPath
    readSession _ (Failed _name exception) = Left $ NonFoundRepoDirectory $ "Error occured for file " ++ _name ++ " " ++ show exception
    readSession s (Dir { name = _, contents = content }) = do
      snapshot <- readSnapshot content
      _deltas  <- readDeltas content
      verifyRepo $ Repository snapshot _deltas
      where
        {- find exactly one snapshot.xml inside of the assumed session store
           TODO Find out if we really do the parsing in strict manner to
                avoid "too many open files" problem -}
        readSnapshot :: [DirTree L.ByteString] -> Either RepoError Snapshot
        readSnapshot dirContent =
          case [ f | f@File { name = n } <- dirContent, n == "snapshot.xml" ] of
               [File { file = c }] -> U.leftmap (BadSnapshot s) $! XS.parseSnapshot c
               _                   -> Left $ CannotFindSnapshot s

        -- find "<serial_number>/delta.xml" inside of the session store
        readDeltas :: [DirTree L.ByteString] -> Either RepoError DeltaMap
        readDeltas dirContent =
          case partitionEithers deltaList of
            ([], parsed)  -> Right $ M.fromList parsed
            (problems, _) -> Left $ RepoESeq $ Prelude.map (uncurry $ BadDelta s) problems
          where
            deltaList = do
              (dName, dContent) <- [ (dName, dContent) | Dir { name = dName, contents = dContent } <- dirContent ]
              fContent          <- [ fContent | File { name = fName, file = fContent} <- dContent, fName == "delta.xml"]
              dSerial           <- rights [U.parseSerial dName]
              return $ U.leftmap (dSerial,) $ fmap (dSerial,) $! XS.parseDelta fContent


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
    matchingSessionId = Prelude.null [ sId | Delta (DeltaDef _ sId _) _ _ <- deltaList, sId /= sessionId ]
    deltaVersion      = Prelude.null [ v   | Delta (DeltaDef v _ _) _ _   <- deltaList, v   /= protocolVersion ]


{- Write one more delta and replace snapshot.xml -}
syncToFS :: Repository -> FilePath -> IO (Either RRDPError ())
syncToFS (Repository s@(Snapshot (SnapshotDef _ (SessionId sId) (Serial serial)) _) _deltas) repoDir = do
  _ <- catchIOError writeDelta $ \e -> return $ Left $ DeltaSyncError e
  catchIOError writeSnapshot   $ \e -> return $ Left $ SnapshotSyncError e
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
    deltaBytes  = fmap serializeDelta $ M.lookup serial _deltas


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do
  queryMessage               <- mapParseError $ XS.parseMessage queryXml
  let (newRepo, replyMessage) = updateRepo repo queryMessage
  return (newRepo, XS.createReply replyMessage)
  where
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r
