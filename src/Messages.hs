module Messages where

import           Network.URI
import           Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as L

import Types

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
      attrs as = [ Attr { attrKey = simpleQName name, attrVal = value } | (name, value) <- as]

simpleQName :: String -> QName
simpleQName name = blank_name { qName = name }

