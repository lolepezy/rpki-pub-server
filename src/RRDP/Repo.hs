module RRDP.Repo where

import Data.Either
import Data.Map as M
import Data.Set as S
import Data.UUID as UU
import Network.URI
import Control.Monad

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Base64.Lazy as B64

import Text.XML.Light.Output
import System.Directory

import Types
import Util
import RRDP.XML

data RRDPError = NoSnapshot SessionId 
               | NoDelta SessionId Serial
               | BadHash { passed :: Hash, stored ::Hash, uri :: URI }
               | BadMessage ParseError
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
  session :: Map SessionId Repository,
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

    -- hash computation can fail in case base64 doesn't contain properly encoded data   
    newObjects    = [ liftM (SnapshotPublish uri base64) $ getHash base64 | PublishQ uri base64 <- pdus ]
    (badHashes, goodHashes) = partitionEithers newObjects

    existingObjects = M.fromList [ (uri, hash) | SnapshotPublish uri _ hash <- publishes ]

    -- delta publish must contain hashes only if it supposed to replace an exising object       
    deltaP = [ DeltaPublish uri base64 $ M.lookup uri existingObjects 
             | SnapshotPublish uri base64 hash <- goodHashes ]
    
    deltaW       = [ Withdraw uri hash | WithdrawQ uri hash  <- pdus ]
    newSnapshotP = (Prelude.filter (not . shouldBeWithdrawn) publishes) ++ goodHashes
      
    newSnapshot  = Snapshot (SnapshotDef version sessionId $ Serial newSerial) newSnapshotP
    newDelta     = Delta (DeltaDef version sessionId $ Serial newSerial) deltaP deltaW
    newDeltas    = M.insert newSerial newDelta deltas
    newRepo      = Repository newSnapshot newDeltas
  
    reply        = Message (Version mVersion) replies :: Message ReplyPdu
    replies      = snapshotPublishR ++ deltaPublishR ++ deltaWithdrawR ++ reportErrors
 
    -- generate reply
    snapshotPublishR = [ PublishR    uri | PublishQ uri base64  <- pdus       ]
    deltaPublishR    = [ PublishR    uri | SnapshotPublish uri _ _ <- goodHashes ]
    deltaWithdrawR   = [ WithdrawR   uri | Withdraw uri _       <- deltaW     ]
    reportErrors     = [ ReportError err | err                  <- badHashes  ]
 
    -- filtering of the withdrawn objects
    shouldBeWithdrawn (SnapshotPublish uri _ _) = uri `S.member` urisToWithdraw 
    urisToWithdraw = S.fromList [ uri | WithdrawQ uri _  <- pdus ]



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

  /x-session-id-1/snapshot.xml
  /x-session-id-1/1/delta.xml
  /x-session-id-1/2/delta.xml
  ...
  /session-id-1 -> x-session-id-1
  ...
  /x-session-id-2/snapshot.xml
  /x-session-id-2/1/delta.xml


-}

{-
readRepo1 :: String -> IO (Maybe Repository)
readRepo1 repoPath = do
    let realPath = repoPath ++ "/actual"
    let notification = "notification.xml"
    dirContent <- getDirectoryContents realPath
    let sessions = filter (/= notification) dirContent
    let r = emptyRepo
    return r
-}

