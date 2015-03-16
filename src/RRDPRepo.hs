module RRDPRepo where

import Data.UUID as UU
import Network.URI
import qualified Data.ByteString.Lazy.Char8  as L
import Text.XML.Light.Output

import Types

-- TODO Force snapshot and deltas to have the same session id
data Repository = Repository Snapshot [Delta]

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
              (SessionId uuid)
              (Serial 1)
              nullURI)
            [SnapshotPublish (nullURI) (Base64 "some base64") (Hash "some hash")])
          []

readRepo :: String -> IO (Maybe Repository)
readRepo repoPath = do 
    let r = emptyRepo
    return r

getSnapshot :: Repository -> L.ByteString
getSnapshot (Repository (Snapshot snapshotDef publishes) _) =
  L.pack . showElement $ snapshotXml snapshotDef publishElements
  where
      publishElements = [base64Xml base64 . uriXml uri $ publishXml | SnapshotPublish uri base64 _ <- publishes]


--data Material = Material [Re]

