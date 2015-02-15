module Repo where

import Data.UUID
import Network.URI

data SnapshotDef = Snapshot {
    snapshotSession_id :: UUID
  , snapshotSerial :: Integer
  , snapshotUri :: URI
}

data DeltaDef = DeltaDef {
    deltaSerial :: Integer
  , deltaUri :: URI  
}

data Notification = Notification {
    version :: Int    
}


