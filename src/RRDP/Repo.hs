{-# LANGUAGE TupleSections  #-}

module RRDP.Repo where

import Data.Either
import Data.Map as M
import Data.Set as S
import Data.UUID as UU
import Network.URI
import Control.Monad

import qualified Data.ByteString.Lazy.Char8  as L

import Text.XML.Light.Output
import System.Directory.Tree

import Types
import Util
import RRDP.XML

data RRDPError = NoSnapshot SessionId
               | NoDelta SessionId Serial
               | BadHash { passed :: Hash, stored ::Hash, uriW :: URI }
               | BadMessage ParseError
               | BadSnapshot SessionId ParseError
               | BadDelta SessionId Int ParseError
               | Seq [RRDPError]
  deriving (Eq, Show)

data RepoError = CannotReadNotification
               | CannotReadSnapshot
               | CannotReadDelta
  deriving (Eq, Show)

type RRDPValue r = Either RRDPError r

type RRDPResponse = RRDPValue L.ByteString

-- TODO Force snapshot and deltas to have the same session id
data Repository = Repository {
  snapshost :: Snapshot,
  deltas :: M.Map Int Delta
} deriving (Show)

data AppState = AppState {
  sessions :: Map SessionId Repository,
  currentSession :: SessionId
}


{- Serialize the current repository elements to XML -}

getSnapshot :: Repository -> RRDPResponse
getSnapshot (Repository (Snapshot snapshotDef publishes) _) =
  Right . L.pack . ppElement $ snapshotXml snapshotDef publishElements
  where
      publishElements = [base64Xml base64 . uriXml uri $ publishXml | SnapshotPublish uri base64 _ <- publishes]

getDelta :: Repository -> SessionId -> Serial -> RRDPResponse
getDelta (Repository { deltas = deltas }) sessionId serial @ (Serial deltaNumber) = do
  delta <- maybeToEither (NoDelta sessionId serial) $ M.lookup deltaNumber deltas
  return $ serializeDelta delta
  where
    serializeDelta :: Delta -> L.ByteString
    serializeDelta (Delta deltaDef ps ws) = L.pack . ppElement $ deltaXml deltaDef pElems wElems
      where
        pElems  = [maybe id hashXml hash . base64Xml base64 . uriXml uri $ publishXml | DeltaPublish uri base64 hash <- ps]
        wElems = [hashXml hash . uriXml uri $ withdrawXml | Withdraw uri hash <- ws]


{- Update repository based on incoming message -}
updateRepo :: Repository -> QMessage -> (Repository, RMessage)
updateRepo repo@(Repository
                  (Snapshot (SnapshotDef version sessionId (Serial serial)) publishes)
                  deltas) (Message (Version mVersion) pdus) =

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
    newObjects    = [ liftM (SnapshotPublish uri base64) $ getHash base64 | PublishQ uri base64 <- pdus ]
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
    newDeltas    = M.insert newSerial newDelta deltas
    newRepo      = Repository newSnapshot newDeltas

     -- generate reply
    reply        = Message (Version mVersion) replies :: Message ReplyPdu
    replies      = publishR ++ withdrawR ++ reportErrors
    publishR     = [ PublishR    uri | SnapshotPublish uri _ _ <- wellHashed  ]
    withdrawR    = [ WithdrawR   uri | Withdraw uri _          <- deltaW      ]
    reportErrors = [ ReportError err | err     <- badlyHashed ++ badWithdraws ]



{- IO-related operations -}

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
readRepo repoPath = return $ maybeToEither CannotReadSnapshot emptyRepo

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


readRepo1 :: SessionId -> String -> IO (Either RRDPError AppState)
readRepo1 s@(SessionId sessionId) repoPath = do
  (a :/ repoDir) <- readDirectoryWith L.readFile repoPath
  let repository = readSession repoDir
  return $ liftM (\sessionRepo -> AppState {
                                    sessions = M.fromList [(s, sessionRepo)],
                                    currentSession = s
                                    }) repository
  where
    readSession :: DirTree L.ByteString -> Either RRDPError Repository
    readSession (Dir { name = sessionDir, contents = content }) = do
      snapshot <- readSnapshot content
      deltas   <- readDeltas content
      verifyRepo $ Repository snapshot deltas
      where
        readSnapshot :: [DirTree L.ByteString] -> Either RRDPError Snapshot
        readSnapshot dirContent =
          case [ f | f@File { name = n } <- dirContent, n == "snapshot.xml" ] of
               []                  -> Left $ NoSnapshot s
               [File { file = c }] -> mapLeft (BadSnapshot s) $ parseSnapshot c

        readDeltas :: [DirTree L.ByteString] -> Either RRDPError (Map Int Delta)
        readDeltas dirContent =
          case partitionEithers dd of
            ([], parsed)  -> Right $ M.fromList parsed
            (problems, _) -> Left $ Seq $ Prelude.map (uncurry $ BadDelta s) problems
          where
            dd = [ mapLeft (dSerial,) $ liftM (dSerial,) $ parseDelta fContent
                 | Dir { name = dName, contents = [ File { name = fName, file = fContent} ] } <- dirContent,
                   let dSerial = parseSerial dName, isSerial dSerial && fName == "delta.xml" ]

        isSerial :: Int -> Bool
        isSerial _ = True

        parseSerial :: String -> Int
        parseSerial _ = 1


verifyRepo :: Repository -> Either RRDPError Repository
verifyRepo r = Right r
