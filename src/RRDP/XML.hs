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
import           Data.Either

import           Types
import           Util

snapshotXml :: SnapshotDef -> [Element] -> Element
snapshotXml (SnapshotDef version sId serial) = commonXml version sId serial "snapshot"

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
                      Maybe String -> Maybe String -> String -> Either ParseError p
simplePublishParse contructor maybeUri _ base64 = do
    u <- maybeToEither NoURI maybeUri
    uri <- maybeToEither (BadURI u) (parseURI u)
    return $ contructor uri (Base64 base64)

publishParseWithHashing :: (URI -> Base64 -> Hash -> p) ->
                            Maybe String -> Maybe String -> String -> Either ParseError p
publishParseWithHashing contructor maybeUri _ base64 = do
    u    <- maybeToEither NoURI maybeUri
    uri  <- maybeToEither (BadURI u) (parseURI u)
    hash <- getHash $ Base64 base64
    return $ contructor uri (Base64 base64) hash


hashedPublishParse :: (URI -> Base64 -> Maybe Hash -> p) ->
                       Maybe String -> Maybe String -> String -> Either ParseError p
hashedPublishParse constructor maybeUri hash base64 = do
    u <- maybeToEither NoURI maybeUri
    uri <- maybeToEither (BadURI u) (parseURI u)
    return $ constructor uri (Base64 base64) (liftM Hash hash)

withdrawParse :: (URI -> Hash -> p) ->
                 Maybe String -> Maybe String -> Either ParseError p
withdrawParse constructor maybeUri maybeHash = do
    u    <- maybeToEither NoURI maybeUri
    uri  <- maybeToEither (BadURI u) (parseURI u)
    hash <- maybeToEither NoHash maybeHash
    return $ constructor uri $ Hash hash

withdrawQueryParse :: (URI -> p) ->
                      Maybe String -> Maybe String -> Either ParseError p
withdrawQueryParse constructor maybeUri _ = do
    u    <- maybeToEither NoURI maybeUri
    uri  <- maybeToEither (BadURI u) (parseURI u)
    return $ constructor uri



parsePWElements :: Element ->
                   (Maybe String -> Maybe String -> String -> Either ParseError p) ->
                   (Maybe String -> Maybe String -> Either ParseError w) ->
                   Either ParseError ([p], [w])
parsePWElements doc publishC withdrawC = do
    {- wrap "publish" records in to Right and "withdraw" records into Left
       so pdus consists of stuff like "Right Right p" and "Right Left w" -}
    pdus <- mapM (\element ->
            case element of
                Element {
                    elName = QName { qName = "publish" },
                    elAttribs = attrs,
                    elContent = [Text CData { cdData = base64 }]
                  } -> fmap Left (publishC (getAttr "uri" attrs) (getAttr "hash" attrs) (normalize base64))

                Element {
                   elName = QName { qName = "withdraw" },
                   elAttribs = attr
                  } -> fmap Right (withdrawC (getAttr "uri" attr) (getAttr "hash" attr))

                _         -> Left $ UnexpectedElement (strContent element)
            ) $ elChildren doc

    return $ partitionEithers pdus
    where normalize = filter (`notElem` " \n\t")


parseMessage :: L.ByteString -> Either ParseError QMessage
parseMessage xml = do
    doc      <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    version  <- maybeToEither NoVersion $ lookupAttr (simpleQName "version") $ elAttribs doc
    (ps, ws) <- parsePWElements doc (simplePublishParse PublishQ) (withdrawQueryParse WithdrawQ)
    return $ Message (Version (read version)) $ ps ++ ws

parseSnapshot :: L.ByteString -> Either ParseError Snapshot
parseSnapshot xml = do
    (doc, sessionId, version, serial) <- parseCommonAttrs xml
    (ps, _)   <- parsePWElements doc (publishParseWithHashing SnapshotPublish)
                   (\_ _ -> Left HashInSnapshotIsNotAllowed)
    return $ Snapshot (SnapshotDef version sessionId serial) ps

parseDelta :: L.ByteString -> Either ParseError Delta
parseDelta xml = do
    (doc, sessionId, version, serial) <- parseCommonAttrs xml
    (ps, ws)  <- parsePWElements doc (hashedPublishParse DeltaPublish) (withdrawParse Withdraw)
    return $ Delta (DeltaDef version sessionId serial) ps ws

parseCommonAttrs :: L.ByteString
                      -> Either ParseError (Element, SessionId, Version, Serial)
parseCommonAttrs xml = do
    doc       <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    sVersion  <- maybeToEither NoVersion $ getAttr "version" $ elAttribs doc
    sessionId <- maybeToEither NoSessionId $ getAttr "session_id" $ elAttribs doc
    sSerial   <- maybeToEither NoSerial $ getAttr "serial" $ elAttribs doc
    version   <- maybeToEither (BadVersion sVersion) (readMaybe sVersion)
    serial    <- maybeToEither (BadSerial sSerial) (readMaybe sSerial)
    return (doc, SessionId sessionId, Version version, Serial serial)


{-
   Refactor that to put all the dependencies on the concrete
   XML library inside of the RRDP.XML module.
-}
createReply :: RMessage -> L.ByteString
createReply (Message version pdus) =
  L.pack . ppElement $ blank_element {
    elName = simpleQName "msg",
    elAttribs = attrs [("version", printV version), ("type", "reply")],
    elContent = map (\pdu ->
      case pdu of
        PublishR uri  -> pduElem "publish" uri
        WithdrawR uri -> pduElem "withdraw" uri
        ReportError e -> reportErrorElem e
      ) pdus
    }
  where
      printV (Version v) = show v
      pduElem name uri = Elem $ blank_element { elName = simpleQName name, elAttribs = attrs [("uri", show uri)] }
      reportErrorElem e = Elem $ blank_element { elName = simpleQName "report_error", elAttribs = attrs [("error_code", show e)] }

getAttr :: String -> [Attr] -> Maybe String
getAttr name = lookupAttr (simpleQName name)

attrs :: [(String, String)] -> [Attr]
attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }
