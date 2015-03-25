module Types where

import Network.URI
import qualified Data.ByteString.Lazy.Char8  as L

newtype Serial = Serial Int deriving (Show, Eq)
newtype SessionId = SessionId String deriving (Show, Eq)
newtype Hash = Hash String deriving (Show, Eq)
newtype Version = Version Int deriving (Show, Eq)
newtype Base64 = Base64 String deriving (Show, Eq)

data SnapshotDef = SnapshotDef Version SessionId Serial
  deriving (Show, Eq)

data DeltaDef = DeltaDef Version SessionId Serial
  deriving (Show, Eq)

data Notification = Notification Version Serial SnapshotDef [DeltaDef]
  deriving (Show, Eq)

data Snapshot = Snapshot SnapshotDef [SnapshotPublish]
  deriving (Show, Eq)
  
data Delta = Delta DeltaDef [DeltaPublish] [Withdraw]
  deriving (Show, Eq)

data SnapshotPublish = SnapshotPublish !URI !Base64 !Hash
  deriving (Show, Eq)

data DeltaPublish = DeltaPublish !URI !Base64 !(Maybe Hash)
  deriving (Show, Eq)

data Withdraw = Withdraw URI Hash
  deriving (Show, Eq)

-- publish/withdraw messages
data Message pdu = Message Version [pdu]
  deriving (Show, Eq)

type QMessage = Message QueryPdu
type RMessage = Message ReplyPdu

data QueryPdu = PublishQ URI Base64 
  | WithdrawQ URI
  deriving (Show, Eq)

data ReplyPdu = PublishR URI 
  | WithdrawR URI
  | ReportError ParseError
  deriving (Show, Eq)

data ParseError = BadXml String
  | NoVersion
  | NoMessageType
  | BadMessageType String
  | UnexpectedElement String
  | NoURI
  | NoSessionId
  | NoSerial
  | NoHash
  | HashInSnapshotIsNotAllowed
  | ObjectNotFound URI
  | BadURI String
  | BadBase64 String
  | BadVersion String
  | BadSerial String
  deriving (Eq, Show)

