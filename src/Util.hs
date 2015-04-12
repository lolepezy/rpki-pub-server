module Util where

import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256

import Types

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

leftmap :: (a -> c) -> Either a b -> Either c b
leftmap f (Left x)  = Left $ f x
leftmap _ (Right r) = Right r

getHash64 :: Base64 -> Either ParseError Hash
getHash64 (Base64 base64) = case B64.decode $ L.pack base64 of
  Right b64 -> Right $ getHash b64
  Left err  -> Left $ BadBase64 err

getHash :: L.ByteString -> Hash
getHash s = Hash . C.unpack . B16.encode . SHA256.hashlazy $ s

parseSerial :: String -> Either ParseError Int
parseSerial s = case reads s of
 [(d,"")] -> Right d
 _        -> Left $ BadSerial s

verify :: Bool -> e -> x -> Either e x
verify condition e x = if condition then Right x else Left e
