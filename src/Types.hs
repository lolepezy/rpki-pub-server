{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Types where

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Data                  (Data, Typeable)
import           Data.Hashable
import           Data.SafeCopy              (base, deriveSafeCopy)
import qualified Data.Text                  as T
import           Network.URI

newtype Serial = Serial Integer deriving (Show, Eq, Ord, Typeable, Data)
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

type Message = QMessage QueryPdu

data QMessage pdu = PduMessage !Version [pdu] | ListMessage
  deriving (Show, Eq)

data Publish = Publish !URI !Base64 !(Maybe Hash)
  deriving (Show, Eq, Ord, Typeable, Data)

data Withdraw = Withdraw !URI !Hash
  deriving (Show, Eq, Ord, Typeable, Data)

data QueryPdu = QP Publish | QW Withdraw
  deriving (Show, Eq, Ord, Typeable, Data)

data ListPdu = ListPdu !URI !Hash
  deriving (Show, Eq)

data Reply = Success
  | ListReply [ListPdu]
  | Errors [RepoError]
  deriving (Show, Eq)


protocolVersion :: String
protocolVersion = "4"

data ParseError = BadXml !T.Text
              | UnexpectedElement !T.Text
              | NoPdus
              | ListWithPdus
              | NoURI
              | NoSerial
              | NoHash
              | NoVersion
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
  deriving (Eq, Show)


data RepoError = NoObjectPresent !URI !Hash
              | ObjectAlreadyPresent !QueryPdu
              | PermissionFailure { oUri :: !URI, storedClientId :: !ClientId, queryClientId :: !ClientId }
              | NoObjectMatchingHash !Hash !QueryPdu
              | BadCmsSignature !QueryPdu
              | XMLError !ParseError
              | ConsistencyProblem !T.Text
              | OtherError !T.Text
  deriving (Eq, Show, Typeable, Data)

-- private utility type to make logic easier to understand
data ObjOperation a u d w = AddOrUpdate_ u | Delete_ d | Wrong_ w | List_ ClientId
  deriving (Eq, Show, Typeable, Data)

type Action = ObjOperation (URI, Base64) (URI, Base64) URI RepoError

instance Hashable URI where
  hashWithSalt salt u = hashWithSalt salt (S.pack $ show u :: S.ByteString)

instance Hashable ClientId where
  hashWithSalt s (ClientId ss) = s `hashWithSalt` ss

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
