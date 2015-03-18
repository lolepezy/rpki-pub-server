module XML where


import qualified Data.ByteString.Lazy.Char8 as L
import           Network.URI
import           Text.Read
import           Text.XML.Light
import           Control.Monad
import           Types

snapshotXml :: SnapshotDef -> [Element] -> Element
snapshotXml (SnapshotDef (Version version) (SessionId uuid) (Serial serial)) publishElements = blank_element {
  elName = simpleQName "snapshot",
  elAttribs = attrs [
    ("xmlns","http://www.ripe.net/rpki/rrdp"),
    ("version", show version),
    ("serial", show serial),
    ("session_id", uuid)
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
       so pdu consists of stuff like "Right Right p" and "Right Left w" -}
    pdus <- mapM (\element ->
            case element of
                Element { 
                    elName = QName { qName = "publish" },
                    elAttribs = attributes,
                    elContent = [Text CData { cdData = base64 }]
                  } -> fmap Right (publishC (getAttr "uri" attributes) (getAttr "hash" attributes) base64)

                Element { 
                   elName = QName { qName = "withdraw" },
                   elAttribs = attributes
                  } -> fmap Left (withdrawC (getAttr "uri" attributes) (getAttr "hash" attributes))

                _         -> Left $ UnexpectedElement (strContent element)
            ) $ elChildren doc

    return ([pdu | Right pdu <- pdus], [pdu | Left pdu <- pdus])


parseMessage :: L.ByteString -> Either Error QMessage
parseMessage xml = do
    doc     <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    version <- maybeToEither NoVersion $ lookupAttr (simpleQName "version") $ elAttribs doc
    (p, w)  <- parsePWElements doc (simplePublishParse PublishQ) (withdrawParse WithdrawQ)
    return $ Message (Version (read version)) $ p ++ w


parseSnapshot :: L.ByteString -> Either Error Snapshot
parseSnapshot xml = do
    doc       <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)    
    sVersion  <- maybeToEither NoVersion $ getAttr "version" $ elAttribs doc
    sessionId <- maybeToEither NoSessionId $ getAttr "session_id" $ elAttribs doc
    sSerial   <- maybeToEither NoSerial $ getAttr "serial" $ elAttribs doc
    version   <- maybeToEither (BadVersion sVersion) (readMaybe sVersion)
    serial    <- maybeToEither (BadSerial sSerial) (readMaybe sSerial)
    (p, _)    <- parsePWElements doc (simplePublishParse SnapshotPublish) (\_ _ -> Left NoHash)
    return $ Snapshot (SnapshotDef (Version version) (SessionId sessionId) (Serial serial)) p


parseDelta :: L.ByteString -> Either Error Delta
parseDelta xml = do
    doc       <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)    
    sVersion  <- maybeToEither NoVersion $ getAttr "version" $ elAttribs doc
    sessionId <- maybeToEither NoSessionId $ getAttr "session_id" $ elAttribs doc
    sSerial   <- maybeToEither NoSerial $ getAttr "serial" $ elAttribs doc
    version   <- maybeToEither (BadVersion sVersion) (readMaybe sVersion)
    serial    <- maybeToEither (BadSerial sSerial) (readMaybe sSerial)
    (p, w)    <- parsePWElements doc (hashedPublishParse DeltaPublish) (withdrawParse Withdraw)
    return $ Delta (DeltaDef (Version version) (SessionId sessionId) (Serial serial)) p w



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

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

getAttr :: String -> [Attr] -> Maybe String
getAttr name a = lookupAttr (simpleQName name) a

attrs :: [(String, String)] -> [Attr]
attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }


