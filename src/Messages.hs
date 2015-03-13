module Messages where

import           Network.URI
import           Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as L

newtype Version = Version Int
  deriving (Show, Eq)

newtype Base64 = Base64 String
  deriving (Show, Eq)

data QMessage = QueryMessage Version [QueryPdu] 
  deriving (Show, Eq)
  
data Message = QMessage | RMessage
  
data RMessage = ReplyMessage Version [ReplyPdu] 
  deriving (Show, Eq)

data QueryPdu = PubkishQ URI Base64 | WithdrawQ URI
  deriving (Show, Eq)
  
data ReplyPdu = PublishR URI | WithdrawR URI
  deriving (Show, Eq)

data Error = BadXml String
  | NoVersion
  | NoMessageType
  | BadMessageType String
  | UnexpectedElement String
  | NoURI
  | BadURI String

parseMessage :: L.ByteString -> Either Error QMessage
parseMessage xml = do
    doc         <- maybeToEither (BadXml $ L.unpack xml) (parseXMLDoc xml)
    version     <- maybeToEither NoVersion $ lookupAttr (simpleQName "version") (elAttribs doc)
    pdus <- mapM (\element -> 
            case element of 
                Element { elName = QName { qName = "publish" },
                          elAttribs = attrs,  
                          elContent = [Text CData { cdData = base64 }]
                        } -> toPublishPdu (lookupAttr (simpleQName "uri") attrs) base64

                Element { elName = QName { qName = "withdraw" }, 
                          elAttribs = attrs 
                        } -> toWithdrawPdu $ lookupAttr (simpleQName "uri") attrs

                _         -> Left $ UnexpectedElement (strContent element)
            ) $ elChildren doc

    return $ QueryMessage (Version (read version)) pdus
    where       
        toPublishPdu :: Maybe String -> String -> Either Error QueryPdu
        toPublishPdu Nothing _ = Left NoURI
        toPublishPdu (Just u) base64 = do
            uri <- maybeToEither (BadURI u) (parseURI u)
            return $ PubkishQ uri (Base64 base64)

        toWithdrawPdu :: Maybe String -> Either Error QueryPdu
        toWithdrawPdu Nothing = Left NoURI
        toWithdrawPdu (Just u) = do
            uri <- maybeToEither (BadURI u) (parseURI u)
            return $ WithdrawQ uri


createReply :: RMessage -> String
createReply (ReplyMessage version pdus) = 
  showElement $ blank_element {
    elName = simpleQName "msg",
    elAttribs = attrs [("version", printV version), ("type", "reply")],
    elContent = map (\pdu -> 
      case pdu of 
        PublishR uri -> pduElem "publish" uri 
        WithdrawR uri -> pduElem "withdraw" uri 
      ) pdus
    }
  where 
      printV (Version v) = show v
      pduElem name uri = Elem $ blank_element { elName = simpleQName name, elAttribs = attrs [("uri", show uri)] }
      attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

