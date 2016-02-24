{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Types where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Data                  (Data, Typeable)
import           Data.SafeCopy              (base, deriveSafeCopy)
import qualified Data.Text                  as T
import           Network.URI

newtype Serial = Serial Int deriving (Show, Eq, Ord, Typeable, Data)
newtype SessionId = SessionId T.Text deriving (Show, Eq, Ord, Typeable, Data)
newtype Version = Version Int deriving (Show, Eq, Ord, Typeable, Data)

newtype Hash = Hash L.ByteString deriving (Show, Eq, Ord, Typeable, Data)
newtype ClientId = ClientId String deriving (Show, Eq, Ord, Typeable, Data)

data Base64 = Base64 !L.ByteString !Hash deriving (Show, Eq, Ord, Typeable, Data)

data SnapshotDef = SnapshotDef !Version !SessionId !Serial
  deriving (Show, Eq)

data DeltaDef = DeltaDef !Version !SessionId !Serial
  deriving (Show, Eq, Ord, Typeable, Data)

data Notification = Notification !Version !Serial !SnapshotDef [DeltaDef]
  deriving (Show, Eq)

data Snapshot = Snapshot !SnapshotDef [Publish]
  deriving (Show, Eq)

data Delta = Delta !DeltaDef [QueryPdu]
  deriving (Show, Eq, Ord, Typeable, Data)

-- publish/withdraw messages
data Message pdu = Message !Version [pdu]
  deriving (Show, Eq)

type QMessage = Message QueryPdu
type RMessage = Message ReplyPdu

data Publish = Publish !URI !Base64 !(Maybe Hash)
  deriving (Show, Eq, Ord, Typeable, Data)

data Withdraw = Withdraw !URI !Hash
  deriving (Show, Eq, Ord, Typeable, Data)

data QueryPdu = QP Publish | QW Withdraw
  deriving (Show, Eq, Ord, Typeable, Data)

data ReplyPdu = PublishR !URI
  | WithdrawR !URI
  | ReportError !RepoError
  deriving (Show, Eq)

data ParseError = BadXml !T.Text
              | NoVersion
              | NoMessageType
              | BadMessageType !T.Text
              | UnexpectedElement !T.Text
              | NoURI
              | NoSessionId
              | NoSerial
              | NoHash
              | HashInSnapshotIsNotAllowed
              | BadURI !T.Text
              | BadBase64 !T.Text
              | BadVersion !T.Text
              | BadSerial !T.Text
              | BadXmlNs !T.Text
  deriving (Eq, Show, Typeable, Data)

data RRDPError = NoSnapshot !SessionId
              | NoDelta !SessionId !Serial
              | BadMessage !ParseError
              | SnapshotSyncError !IOError
              | DeltaSyncError !IOError
              | NotificationSyncError !IOError
              | InconsistentSerial !Int
  deriving (Eq, Show)

data RepoError = CannotFindSnapshot !SessionId
              | CannotFindDelta !SessionId
              | NonMatchingSessionId
              | NonFoundRepoDirectory !String
              | CouldNotReadRepoDirectory !String
              | NotFoundSessionWithId !SessionId
              | BadRRDPVersion !Version
              | NonContinuousDeltas [Int]
              | BadSnapshot !SessionId !ParseError
              | BadDelta !SessionId !Int !ParseError
              | ObjectNotFound !URI
              | CannotInsertExistingObject !URI
              | CannotChangeOtherClientObject { oUri :: !URI, storedClientId :: !ClientId, queryClientId :: !ClientId }
              | BadHash { passed :: !Hash, stored :: !Hash, uriW :: !URI }
              | RepoESeq [RepoError]
  deriving (Eq, Show, Typeable, Data)

-- private utility type to make logic easier to understand
data ObjOperation a u d w = AddOrUpdate_ u | Delete_ d | Wrong_ w
  deriving (Eq, Show, Typeable, Data)

type Action = ObjOperation (URI, Base64) (URI, Base64) URI RepoError

$(deriveSafeCopy 0 'base ''Hash)
$(deriveSafeCopy 0 'base ''Serial)
$(deriveSafeCopy 0 'base ''Version)
$(deriveSafeCopy 0 'base ''SessionId)
$(deriveSafeCopy 0 'base ''Base64)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
$(deriveSafeCopy 0 'base ''ClientId)
$(deriveSafeCopy 0 'base ''QueryPdu)
$(deriveSafeCopy 0 'base ''Publish)
$(deriveSafeCopy 0 'base ''Withdraw)
$(deriveSafeCopy 0 'base ''DeltaDef)
$(deriveSafeCopy 0 'base ''Delta)
$(deriveSafeCopy 0 'base ''ObjOperation)
$(deriveSafeCopy 0 'base ''ParseError)
$(deriveSafeCopy 0 'base ''RepoError)
