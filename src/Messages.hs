module Messages where

import Network.URI

newtype Version = Version Int 
data MType = Query | Reply

data Message = Message {
  version :: Version,
  mtype :: MType,
  pdus :: [Pdu]
}

data Pdu = Publish {
  publisUri :: URI,
  base64 :: String
} | Withdraw {
  withdrawUri :: URI
} deriving (Show, Eq)
