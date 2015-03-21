module RRDP.Repo where

import Data.Map as M
import Data.Set as S
import Data.UUID as UU
import Network.URI
import qualified Data.ByteString.Lazy.Char8  as L
import Text.XML.Light.Output
import System.Directory

import Types
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
}

data AppState = AppState {
  session :: Map SessionId Repository,
  currentSession :: SessionId
}


{- Serialize the current repository elements to XML -}

getSnapshot :: Repository -> RRDPResponse
getSnapshot (Repository (Snapshot snapshotDef publishes) _) =
  Right . L.pack . ppElement $ snapshotXml snapshotDef publishElements
  where
      publishElements = [base64Xml base64 . uriXml uri $ publishXml | SnapshotPublish uri base64 <- publishes]

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
updateRepo :: Repository -> QMessage -> RRDPValue (Repository, RMessage)
updateRepo r@(Repository 
               (Snapshot (SnapshotDef v@(Version rVersion) s@(SessionId sessionId) (Serial serial)) publishes) 
               deltas) (Message (Version mVersion) pdus) =
  -- TODO Implement proper error generation in case of mismatching hashes
  Right (newRepo, reply)
  where
    newSerial    = serial + 1
    newRepo      = Repository (Snapshot (SnapshotDef v s (Serial newSerial)) newPublishes) newDeltas

    reply        = Message (Version mVersion) replies :: Message ReplyPdu
    replies      = Prelude.map (\m -> case m of  
                                        PublishQ uri base64 -> PublishR uri
                                        WithdrawQ uri _     -> WithdrawR uri
                               ) pdus  
    
    newPublishes = (Prelude.filter shouldBeWithdrawn publishes) ++ newPublish
    newPublish   = [ SnapshotPublish uri base64 | PublishQ uri base64 <- pdus ]
    newDeltas    = M.insert newSerial newDelta deltas

    -- TODO compute the hash
    newDelta     = Delta (DeltaDef v s $ Serial newSerial)
                         [ DeltaPublish uri base64 Nothing | PublishQ  uri base64 <- pdus ] 
                         [ Withdraw     uri hash           | WithdrawQ uri hash   <- pdus ]

    -- TODO compare the hash and complain if it differs from the hash in withdraw message
    shouldBeWithdrawn (SnapshotPublish uri base64) = uri `S.member` urisToWithdraw 
    urisToWithdraw = S.fromList [ uri | WithdrawQ uri _  <- pdus ]



{- IO-related operations -}

emptyRepo :: Maybe Repository
emptyRepo = do
    uuid <- UU.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
    return $ Repository
          ( Snapshot
            (SnapshotDef
              (Version 1)
              (SessionId (show uuid))
              (Serial 1)
              )
            [SnapshotPublish nullURI (Base64 "kjbrh9f835f98b5f98f89b0897ewrb07b5bero34b")])
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

