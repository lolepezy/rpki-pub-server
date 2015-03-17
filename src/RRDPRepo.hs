module RRDPRepo where

import Data.Map as M
import Data.UUID as UU
import Network.URI
import qualified Data.ByteString.Lazy.Char8  as L
import Text.XML.Light.Output

import Types

data RRDPError = NoSnapshot SessionId 
               | NoDelta SessionId Serial

type RRDPResponse = Either RRDPError L.ByteString

-- TODO Force snapshot and deltas to have the same session id
data Repository = Repository {
  snaphost :: Snapshot,
  deltas :: M.Map Int Delta
}

data AppState = AppState {
  repository :: Repository
}

emptyRepo :: Maybe Repository
emptyRepo = do
    uuid <- UU.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
    return $ Repository
          ( Snapshot
            (SnapshotDef
              (Version 1)
              (SessionId (show uuid))
              (Serial 1)
              nullURI)
            [SnapshotPublish nullURI (Base64 "kjbrh9f835f98b5f98f89b0897ewrb07b5bero34b") (Hash "ab96794yjsbdcjlb")])
          M.empty

readRepo :: String -> IO (Maybe Repository)
readRepo repoPath = do
    let r = emptyRepo
    return r

getSnapshot :: Repository -> RRDPResponse
getSnapshot (Repository (Snapshot snapshotDef publishes) _) =
  Right . L.pack . ppElement $ snapshotXml snapshotDef publishElements
  where
      publishElements = [base64Xml base64 . uriXml uri $ publishXml | SnapshotPublish uri base64 _ <- publishes]

getDelta :: Repository -> SessionId -> Serial -> RRDPResponse
getDelta (Repository { deltas = deltas }) sessionId serial @ (Serial deltaNumber) =
    case M.lookup deltaNumber deltas of
        Nothing -> Left $ NoDelta sessionId serial
        Just delta -> Right $ serializeDelta delta
          where 
            serializeDelta :: Delta -> L.ByteString
            serializeDelta (Delta deltaDef publishes withdraws) = L.pack . ppElement $ deltaXml deltaDef publishElements withdrawElements
              where 
                publishElements  = [hashXml hash . base64Xml base64 . uriXml uri $ publishXml | DeltaPublish uri base64 hash <- publishes]
                withdrawElements = [hashXml hash . uriXml uri $ withdrawXml | Withdraw uri hash <- withdraws]

