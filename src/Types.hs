module Types where

import Data.UUID as UU
import Network.URI
import Text.XML.Light

newtype Serial = Serial Integer
newtype Version = Version Int
newtype SessionId = SessionId UUID
newtype Hash = Hash String
newtype Base64 = Base64 String

data SnapshotDef = SnapshotDef Version SessionId Serial URI

data DeltaDef = DeltaDef Version SessionId Serial

data Notification = Notification Version Serial SnapshotDef [DeltaDef]

data Snapshot = Snapshot SnapshotDef [SnapshotPublish]
data Delta = Delta DeltaDef [DeltaPublish] [Withdraw]

data SnapshotPublish = SnapshotPublish URI Base64 Hash
data DeltaPublish = DeltaPublish URI Base64 Hash

data Withdraw = Withdraw URI Hash


snapshotXml :: SnapshotDef -> [Element] -> Element 
snapshotXml (SnapshotDef (Version version) (SessionId uuid) (Serial serial) _) publishElements = blank_element { 
  elName = simpleQName "snapshot", 
  elAttribs = attrs [
    ("xmlns","http://www.ripe.net/rpki/rrdp"),
    ("version", show version),
    ("serial", show serial),
    ("session_id", show uuid) 
    ],
  elContent = map Elem publishElements
  }

deltaXml :: DeltaDef -> [Element] -> [Element] -> Element
deltaXml (DeltaDef (Version version) (SessionId uuid) (Serial serial)) publishElements withdrawElements = blank_element { 
  elName = simpleQName "delta", 
  elAttribs = attrs [
    ("xmlns","http://www.ripe.net/rpki/rrdp"),
    ("version", show version),
    ("serial", show serial),
    ("session_id", show uuid) 
    ],
  elContent = map Elem $ publishElements ++ withdrawElements
  }


publishXml :: Element
publishXml = blank_element { elName = simpleQName "publish" }

withdrawXml :: Element
withdrawXml = blank_element { elName = simpleQName "withdraw" }

uriXml :: URI -> Element -> Element
uriXml uri (xml @ Element { elAttribs = a }) = xml { elAttribs = a ++ attrs [("uri", show uri)] }

base64Xml :: Base64 -> Element -> Element
base64Xml (Base64 base64) xml = xml { elContent = [Text blank_cdata { cdData = base64 }] }

hashXml :: Hash -> Element -> Element
hashXml (Hash hash) (xml @ Element { elAttribs = a }) = xml { elAttribs = a ++ attrs [("hash", hash)] }

attrs :: [(String, String)] -> [Attr]
attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }