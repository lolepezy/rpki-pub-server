module Repo where
    
import Data.UUID as UU
import Network.URI

newtype Serial = Serial Integer
newtype Version = Version Int
newtype SessionId = SessionId UUID
newtype Hash = Hash String
newtype Base64 = Base64 String

data SnapshotDef = SnapshotDef UUID Serial URI 

data DeltaDef = DeltaDef Serial URI

data Notification = Notification Version Serial SnapshotDef [DeltaDef]

data Snapshot = Snapshot SnapshotDef [SnapshotPublish]
data Delta = Delta [DeltaPublish] [Withdraw]

data SnapshotPublish = SnapshotPublish URI Base64
data DeltaPublish = DeltaPublish URI Hash Base64

data Withdraw = Withdraw URI Hash

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
              uuid
              (Serial 1)
              nullURI)
            [])
          []

readRepo :: String -> IO (Maybe Repository)
readRepo repoPath = do 
    let r = emptyRepo
    return r