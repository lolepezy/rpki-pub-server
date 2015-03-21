module RRDP.XML (
  snapshotXml, deltaXml, publishXml, withdrawXml, 
  uriXml, base64Xml, hashXml, 
  parseMessage, createReply,
  parseSnapshot, parseDelta
)
where

import qualified Data.ByteString.Lazy.Char8 as L
import           Network.URI
import           Text.Read
import           Text.XML.Light
import           Control.Monad
import           Types

snapshotXml :: SnapshotDef -> [Element] -> Element
snapshotXml (SnapshotDef version sId serial) publishElements = 
  commonXml version sId serial "snapshot" publishElements  

deltaXml :: DeltaDef -> [Element] -> [Element] -> Element
deltaXml (DeltaDef version sId serial) publishElements withdrawElements = 
  commonXml version sId serial "delta" (publishElements ++ withdrawElements)

commonXml :: Version -> SessionId -> Serial -> String -> [Element] -> Element
commonXml (Version version) (SessionId uuid) (Serial serial) elemName elements = blank_element {
  elName = simpleQName elemName,
  elAttribs = attrs [
    ("xmlns","http://www.ripe.net/rpki/rrdp"),
    ("version", show version),
    ("serial", show serial),
    ("session_id", uuid)
    ],
  elContent = map Elem elements
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


simplePublishParse :: (URI -> Base64 -> p) -> 
                      Maybe String -> Maybe String -> String -> Either Error p
simplePublishParse contructor maybeUri _ base64 = do
    u <- maybeToEither NoURI maybeUri
    uri <- maybeToEither (BadURI u) (parseURI u)
    return $ contructor uri (Base64 base64)

hashedPublishParse :: (URI -> Base64 -> Maybe Hash -> p) -> 
                       Maybe String -> Maybe String -> String -> Either Error p
hashedPublishParse constructor maybeUri hash base64 = do
    u <- maybeToEither NoURI maybeUri
    uri <- maybeToEither (BadURI u) (parseURI u)
    return $ constructor uri (Base64 base64) (liftM Hash hash)

withdrawParse :: (URI -> Hash -> p) -> 
                 Maybe String -> Maybe String -> Either Error p
withdrawParse constructor maybeUri maybeHash = do
    u    <- maybeToEither NoURI maybeUri
    hash <- maybeToEither NoHash maybeHash
    uri  <- maybeToEither (BadURI u) (parseURI u)
    return $ constructor uri $ Hash hash

parsePWElements :: Element -> 
                   (Maybe String -> Maybe String -> String -> Either Error p) ->
                   (Maybe String -> Maybe String -> Either Error w) ->
                   Either Error ([p], [w])
parsePWElements doc publishC withdrawC = do
    {- wrap "publish" records in to Right and "withdraw" records into Left
       so pdus consists of stuff like "Right Right p" and "Right Left w" -}
    pdus <- mapM (\element ->
            case element of
                Element { 
                    elName = QName { qName = "publish" },
                    elAttribs = attrs,
                    elContent = [Text CData { cdData = base64 }]
                  } -> fmap Right (publishC (getAttr "uri" attrs) (getAttr "hash" attrs) (normalize base64))

                Element { 
                   elName = QName { qName = "withdraw" },
                   elAttribs = attr
                  } -> fmap Left (withdrawC (getAttr "uri" attr) (getAttr "hash" attr))

                _         -> Left $ UnexpectedElement (strContent element)
            ) $ elChildren doc

    return ([pdu | Right pdu <- pdus], [pdu | Left pdu <- pdus])
    where normalize = filter (\c -> not (c `elem` " \n\t"))


parseMessage :: L.ByteString -> Either Error QMessage
parseMessage xml = do
    doc      <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    version  <- maybeToEither NoVersion $ lookupAttr (simpleQName "version") $ elAttribs doc
    (ps, ws) <- parsePWElements doc (simplePublishParse PublishQ) (withdrawParse WithdrawQ)
    return $ Message (Version (read version)) $ ps ++ ws

parseSnapshot :: L.ByteString -> Either Error Snapshot
parseSnapshot xml = do
    (doc, sessionId, version, serial) <- parseCommonAttrs xml
    (ps, _)   <- parsePWElements doc (simplePublishParse SnapshotPublish) (\_ _ -> Left NoHash)
    return $ Snapshot (SnapshotDef version sessionId serial) ps

parseDelta :: L.ByteString -> Either Error Delta
parseDelta xml = do
    (doc, sessionId, version, serial) <- parseCommonAttrs xml
    (ps, ws)  <- parsePWElements doc (hashedPublishParse DeltaPublish) (withdrawParse Withdraw)
    return $ Delta (DeltaDef version sessionId serial) ps ws

parseCommonAttrs :: L.ByteString
                      -> Either Error (Element, SessionId, Version, Serial)
parseCommonAttrs xml = do
    doc       <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    sVersion  <- maybeToEither NoVersion $ getAttr "version" $ elAttribs doc
    sessionId <- maybeToEither NoSessionId $ getAttr "session_id" $ elAttribs doc
    sSerial   <- maybeToEither NoSerial $ getAttr "serial" $ elAttribs doc
    version   <- maybeToEither (BadVersion sVersion) (readMaybe sVersion)
    serial    <- maybeToEither (BadSerial sSerial) (readMaybe sSerial)
    return (doc, SessionId sessionId, Version version, Serial serial)


createReply :: RMessage -> String
createReply (Message version pdus) =
  showElement $ blank_element {
    elName = simpleQName "msg",
    elAttribs = attrs [("version", printV version), ("type", "reply")],
    elContent = map (\pdu ->
      case pdu of
        PublishR uri  -> pduElem "publish" uri
        WithdrawR uri -> pduElem "withdraw" uri
      ) pdus
    }
  where
      printV (Version v) = show v
      pduElem name uri = Elem $ blank_element { elName = simpleQName name, elAttribs = attrs [("uri", show uri)] }

getAttr :: String -> [Attr] -> Maybe String
getAttr name = lookupAttr (simpleQName name)

attrs :: [(String, String)] -> [Attr]
attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }

