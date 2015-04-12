{-# LANGUAGE TupleSections  #-}

module RRDP.Repo where

import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Set as S
import Data.UUID as UU
import Network.URI

import qualified Data.ByteString.Lazy.Char8  as L

import Text.XML.Light.Output
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.IO.Error

import Types
import Util
import RRDP.XML

data RRDPError = NoSnapshot SessionId
               | NoDelta SessionId Serial
               | BadHash { passed :: Hash, stored ::Hash, uriW :: URI }
               | BadMessage ParseError
               | SnapshotSyncError IOError
               | DeltaSyncError IOError
               | InconsistenSerial Int
  deriving (Eq, Show)

data RepoError = CannotFindSnapshot SessionId
               | CannotFindDelta SessionId
               | NonMatchingSessionId
               | NonFoundRepoDirectory String
               | NotFoundSessionWithId SessionId
               | BadRRDPVersion Version
               | NonContinuousDeltas [Int]
               | BadSnapshot SessionId ParseError
               | BadDelta SessionId Int ParseError
               | RepoESeq [RepoError]
  deriving (Eq, Show)

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
serializeNotification (Repository (Snapshot sd@(SnapshotDef _ (SessionId sId) _) _) _deltas)
  _repoUrlBase sSnapshot sDeltas = L.pack . ppElement $ notificationXml sd elements
  where
    elements = [ snapshotDefElem sUri (getHash sSnapshot)
               | sUri <- maybeToList snapshotUri] ++
               [ deltaDefElem   dUri (getHash sDelta) serial
               | (serial, _) <- M.toList _deltas,
                  sDelta     <- maybeToList $ M.lookup serial sDeltas,
                  dUri       <- maybeToList $ deltaUri serial]

    snapshotUri = parseURI $ _repoUrlBase ++ "/" ++ sId ++ "/snapshot.xml"
    deltaUri s  = parseURI $ _repoUrlBase ++ "/" ++ sId ++ "/" ++ show s ++ "/delta.xml"

    snapshotDefElem uri hash     = uriXml uri . hashXml hash $ mkElem "snapshot"
    deltaDefElem uri hash serial = uriXml uri . hashXml hash . serialXml serial $ mkElem "delta"



serializeSnapshot :: Snapshot -> L.ByteString
serializeSnapshot (Snapshot snapshotDef publishes) =
  L.pack . ppElement $ snapshotXml snapshotDef publishElements
  where
    publishElements = [base64Xml base64 . uriXml uri $ publishXml | SnapshotPublish uri base64 _ <- publishes]

serializeDelta :: Delta -> L.ByteString
serializeDelta (Delta deltaDef ps ws) = L.pack . ppElement $ deltaXml deltaDef pElems wElems
  where
    pElems = [uriXml uri . base64Xml base64 . maybe id hashXml hash $ publishXml | DeltaPublish uri base64 hash <- ps]
    wElems = [uriXml uri . hashXml hash $ withdrawXml| Withdraw uri hash <- ws]


{- Update repository based on incoming message -}
updateRepo :: Repository -> QMessage -> (Repository, RMessage)
updateRepo repo@(Repository
                  (Snapshot (SnapshotDef version sessionId (Serial serial)) publishes)
                  existingDeltas) (Message (Version mVersion) pdus) =

  {- TODO:
     That needs to be clarified: should we update anything if there're errors?
     The standard is pretty vague about it. For now the strategy is to be as
     strict as possible: update the repository only if there're no errors.
   -}
  case reportErrors of
    [] -> (newRepo, reply)
    _  -> (repo, reply)

  where
    newSerial = serial + 1

    existingObjects = M.fromList [ (uri, hash) | SnapshotPublish uri _ hash <- publishes ]

    newUri (SnapshotPublish uri _ _) = uri `S.member` newUris
    newUris = S.fromList [ case p of
                             PublishQ uri _  -> uri
                             WithdrawQ uri   -> uri
                         | p <- pdus ]

    -- hash computation can fail in case base64 doesn't contain properly encoded data
    newObjects    = [ fmap (SnapshotPublish uri base64) $ getHash64 base64 | PublishQ uri base64 <- pdus ]
    (badlyHashed, wellHashed) = partitionEithers newObjects

    -- delta publish must contain hashes only if it's supposed to replace an exising object
    deltaP = [ DeltaPublish uri base64 $ M.lookup uri existingObjects
             | SnapshotPublish uri base64 _ <- wellHashed ]

    -- separate withdraw elements pointing to non-existinent object from the proper ones
    (badWithdraws, deltaW) = partitionEithers [
                               case M.lookup uri existingObjects of
                                 Just hash -> Right $ Withdraw uri hash
                                 Nothing   -> Left $ ObjectNotFound uri
                             | WithdrawQ uri <- pdus ]

    {- We need to remove objects that must be withdrawn, replace the existing
       ones and add new ones. In fact that means to remove all that are mentioned
       in the query and add newly published ones.
    -}
    previousPublishes = Prelude.filter (not . newUri) publishes

    newSnapshotP = previousPublishes ++ wellHashed

    newSnapshot  = Snapshot (SnapshotDef version sessionId $ Serial newSerial) newSnapshotP
    newDelta     = Delta (DeltaDef version sessionId $ Serial newSerial) deltaP deltaW
    newDeltas    = M.insert newSerial newDelta existingDeltas
    newRepo      = Repository newSnapshot newDeltas

     -- generate reply
    reply        = Message (Version mVersion) replies :: Message ReplyPdu
    replies      = publishR ++ withdrawR ++ reportErrors
    publishR     = [ PublishR    uri | SnapshotPublish uri _ _ <- wellHashed  ]
    withdrawR    = [ WithdrawR   uri | Withdraw uri _          <- deltaW      ]
    reportErrors = [ ReportError err | err     <- badlyHashed ++ badWithdraws ]



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
                }) currentSessionId = do
  {- Read the whole directory content, each directory corresponds to a session,
     in most practical cases there will be only one session -}
  (_ :/ Dir { contents = repoDirContent } ) <- readDirectoryWith L.readFile _repoPath

  -- read sessions and separate successfull ones from broken ones.
  let potentialRepositories = [ leftmap (sId,) $ fmap (sId,) $ readSession sId dir
                              | dir@Dir { name = sessionId } <- repoDirContent, let sId = SessionId sessionId ]

  {- we don't enforce a restriction for all sessions to be valid,
     so we only bail out in case of no valid sessions -}
  return $ case partitionEithers potentialRepositories of
             (badRepos, []) -> Left $ RepoESeq $ Prelude.map snd badRepos
             (_, goodRepos) -> createAppState goodRepos currentSessionId
  where

    createAppState :: [(SessionId, Repository)] -> SessionId -> Either RepoError AppState
    createAppState repos sId = maybeToEither (NotFoundSessionWithId sId) $
      fmap (\cs -> AppState {
           sessions              = sessionMap,
           currentSession        = cs,
           serializedCurrentRepo = serializeRepo cs _urlBase,
           repoPath              = _repoPath,
           repoUrlBase           = _urlBase
        }) current
        where
          sessionMap = M.fromList repos
          current = M.lookup sId sessionMap

    -- read one directory and try to find snapshot and the sequence of deltas in it
    readSession :: SessionId -> DirTree L.ByteString -> Either RepoError Repository
    readSession _ (File _ _) = Left $ NonFoundRepoDirectory $ "Wrong path " ++ _repoPath
    readSession _ (Failed _name exception) = Left $ NonFoundRepoDirectory $ "Error occured for file " ++ _name ++ " " ++ show exception
    readSession s (Dir { name = _, contents = content }) = do
      snapshot <- readSnapshot content
      _deltas   <- readDeltas content
      verifyRepo $ Repository snapshot _deltas
      where
        {- find exactly one snapshot.xml inside of the assumed session store
           NOTE: we do the parsing in strict manner to avoid "too many open files" problem -}
        readSnapshot :: [DirTree L.ByteString] -> Either RepoError Snapshot
        readSnapshot dirContent =
          case [ f | f@File { name = n } <- dirContent, n == "snapshot.xml" ] of
               [File { file = c }] -> leftmap (BadSnapshot s) $! parseSnapshot c
               _                   -> Left $ CannotFindSnapshot s

        -- find "<serial_number>/delta.xml" inside of the session store
        readDeltas :: [DirTree L.ByteString] -> Either RepoError DeltaMap
        readDeltas dirContent =
          case partitionEithers deltaList of
            (_,  [])      -> Left $ CannotFindDelta s
            ([], parsed)  -> Right $ M.fromList parsed
            (problems, _) -> Left $ RepoESeq $ Prelude.map (uncurry $ BadDelta s) problems
          where
            deltaList = do
              (dName, dContent) <- [ (dName, dContent) | Dir { name = dName, contents = dContent } <- dirContent ]
              fContent          <- [ fContent | File { name = fName, file = fContent} <- dContent, fName == "delta.xml"]
              dSerial           <- rights [parseSerial dName]
              return $ leftmap (dSerial,) $ fmap (dSerial,) $! parseDelta fContent


-- Check some precoditions of the repository consistency
-- TODO Add check for contigous delta range
verifyRepo :: Repository -> Either RepoError Repository
verifyRepo r@(Repository (Snapshot (SnapshotDef version sessionId _) _) _deltas) =
  verify matchingSessionId NonMatchingSessionId r >>=
  verify (version == protocolVersion) (BadRRDPVersion version) >>=
  verify deltaVersion (BadRRDPVersion version)
  where
    protocolVersion = Version 1
    deltaList = M.elems _deltas
    matchingSessionId = Prelude.null [ sId | Delta (DeltaDef _ sId _) _ _ <- deltaList, sId /= sessionId ]
    deltaVersion      = Prelude.null [ v | Delta (DeltaDef v _ _) _ _ <- deltaList, v /= protocolVersion ]


{- Write one more delta and replace snapshot.xml -}
syncToFS :: Repository -> FilePath -> IO (Either RRDPError ())
syncToFS (Repository s@(Snapshot (SnapshotDef _ (SessionId sId) (Serial serial)) _) _deltas) repoDir = do
  catchIOError writeDelta    $ \e -> return $ Left $ DeltaSyncError e
  catchIOError writeSnapshot $ \e -> return $ Left $ SnapshotSyncError e
  where
    snapshotTmpName = "snapshot.xml.tmp"
    snapshotName    = "snapshot.xml"

    sessionStoreDir = repoDir </> sId

    writeSnapshot = do
      L.writeFile (sessionStoreDir </> snapshotTmpName) $ serializeSnapshot s
      renameFile snapshotTmpName snapshotName
      return $ Right ()

    writeDelta = case deltaBytes of
      Just d  -> do
        createDirectoryIfMissing False $ sessionStoreDir </> show serial
        L.writeFile (sessionStoreDir </> show serial </> "delta.xml") d
        return $ Right ()
      Nothing -> return $ Left $ InconsistenSerial serial

    -- TODO: Think about getting rid of Maybe here and how to always
    -- have a delta
    deltaBytes  = fmap serializeDelta $ M.lookup serial _deltas


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do
  queryMessage               <- mapParseError $ parseMessage queryXml
  let (newRepo, replyMessage) = updateRepo repo queryMessage
  return (newRepo, createReply replyMessage)
  where
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r


{- TODO Remove it after it's not used anymore -}
emptyRepo :: Maybe Repository
emptyRepo = do
    uuid <- UU.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
    uri <- parseURI "rsync://host.com/a/b"
    return $ Repository
          ( Snapshot
            (SnapshotDef
              (Version 1)
              (SessionId (show uuid))
              (Serial 1)
              )
            [SnapshotPublish uri (Base64 "kjbrh9f835f98b5f98f89b0897ewrb07b5bero34b") (Hash "7675675757")])
          M.empty

readRepo :: String -> IO (Either RepoError Repository)
readRepo repoPath = return $ maybeToEither (CannotFindSnapshot $ SessionId "") emptyRepo
