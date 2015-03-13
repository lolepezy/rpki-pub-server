module Messages where

import           Network.URI

import           Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as L

newtype Version = Version Int
  deriving (Show, Eq)

newtype Base64 = Base64 String
  deriving (Show, Eq)

data MType = Query | Reply
  deriving (Show, Eq)

data Message = Message Version MType [Pdu]
  deriving (Show, Eq)

data Pdu = Publish URI Base64
  | Withdraw URI
  deriving (Show, Eq)

data Error = BadXml String
  | NoVersion
  | NoMessageType
  | BadMessageType String
  | BrokenPublishElement
  | BrokenWithdrawElement
  | NoURI
  | BadURI String

parseMessage :: L.ByteString -> Either Error Message
parseMessage xml = do
    doc         <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    version     <- maybeToEither NoVersion $ lookupAttr (qname "version") (elAttribs doc)
    messageType <- maybeToEither NoMessageType $ lookupAttr (qname "type") (elAttribs doc)
    mType       <- case messageType of
                        "query" -> Right Query
                        "reply" -> Right Reply
                        _       -> Left $ BadMessageType messageType

    let publishPdus = map (\e -> 
            case e of 
                Element { elAttribs = attrs,  elContent = [Text CData { cdData = base64 }]} -> toPublish (lookupAttr (qname "uri") attrs) base64
                _                                                                           -> Left BrokenPublishElement
            ) $ findElements (qname "publish") doc

    let withdrawPdus = map (\e -> 
            case e of  
                Element { elAttribs = attrs } -> toWithdraw $ lookupAttr (qname "uri") attrs
                _                             -> Left BrokenWithdrawElement 
            ) $ findElements (qname "withdraw") doc

    pdus <- sequence $ publishPdus ++ withdrawPdus
    return $ Message (Version (read version)) mType pdus
    where
        qname n = QName { qName = n, qURI = Nothing, qPrefix = Nothing }

        toPublish :: Maybe String -> String -> Either Error Pdu
        toPublish Nothing _ = Left NoURI
        toPublish (Just u) base64 = do
            uri <- maybeToEither (BadURI u) (parseURI u)
            return $ Publish uri (Base64 base64)

        toWithdraw :: Maybe String -> Either Error Pdu
        toWithdraw Nothing = Left NoURI
        toWithdraw (Just u) = do
            uri <- maybeToEither (BadURI u) (parseURI u)
            return $ Withdraw uri


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a
