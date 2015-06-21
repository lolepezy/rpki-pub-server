{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Types where

import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Network.URI
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.Text as T

newtype Serial = Serial Int deriving (Show, Eq, Ord)
newtype SessionId = SessionId T.Text deriving (Show, Eq, Ord)
newtype Version = Version Int deriving (Show, Eq)

newtype Hash = Hash L.ByteString deriving (Show, Eq, Ord, Typeable, Data)
$(deriveSafeCopy 0 'base ''Hash)

data Base64 = Base64 !L.ByteString Hash
  deriving (Show, Eq, Ord, Typeable, Data)

$(deriveSafeCopy 0 'base ''Base64)

-- some other derivations 
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)

data SnapshotDef = SnapshotDef !Version !SessionId !Serial
  deriving (Show, Eq)

data DeltaDef = DeltaDef !Version !SessionId !Serial
  deriving (Show, Eq)

data Notification = Notification !Version !Serial !SnapshotDef [DeltaDef]
  deriving (Show, Eq)

data Snapshot = Snapshot !SnapshotDef [Publish]
  deriving (Show, Eq)

data Delta = Delta !DeltaDef [Publish] [Withdraw]
  deriving (Show, Eq)

data Publish = Publish !URI !Base64 !(Maybe Hash)
  deriving (Show, Eq)

data Withdraw = Withdraw !URI !Hash
  deriving (Show, Eq)

-- publish/withdraw messages
data Message pdu = Message Version [pdu]
  deriving (Show, Eq)

type QMessage = Message QueryPdu
type RMessage = Message ReplyPdu

data QueryPdu = PublishQ !URI !Base64 !(Maybe Hash)
  | WithdrawQ !URI !Hash
  deriving (Show, Eq)

data ReplyPdu = PublishR !URI
  | WithdrawR !URI
  | ReportError !RepoError
  deriving (Show, Eq)

data ParseError = BadXml T.Text
              | NoVersion
              | NoMessageType
              | BadMessageType T.Text
              | UnexpectedElement T.Text
              | NoURI
              | NoSessionId
              | NoSerial
              | NoHash
              | HashInSnapshotIsNotAllowed
              | BadURI T.Text
              | BadBase64 T.Text
              | BadVersion T.Text
              | BadSerial T.Text
              | BadXmlNs T.Text
  deriving (Eq, Show)

data RRDPError = NoSnapshot SessionId
              | NoDelta SessionId Serial
              | BadMessage ParseError
              | SnapshotSyncError IOError
              | DeltaSyncError IOError
              | InconsistentSerial Int
  deriving (Eq, Show)

data RepoError = CannotFindSnapshot SessionId
              | CannotFindDelta SessionId
              | NonMatchingSessionId
              | NonFoundRepoDirectory String
              | CouldNotReadRepoDirectory String
              | NotFoundSessionWithId SessionId
              | BadRRDPVersion Version
              | NonContinuousDeltas [Int]
              | BadSnapshot SessionId ParseError
              | BadDelta SessionId Int ParseError
              | ObjectNotFound URI
              | CannotInsertExistingObject URI
              | BadHash { passed :: Hash, stored ::Hash, uriW :: URI }
              | RepoESeq [RepoError]
  deriving (Eq, Show)

data AppOptions = AppOptions {
  repositoryPathOpt :: String,
  repositoryBaseUrlOpt :: String,
  currentSessionOpt :: String
}
