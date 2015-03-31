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
leftmap f (Left l) = Left $ f l
leftmap _ (Right r) = Right r

getHash :: Base64 -> Either ParseError Hash
getHash (Base64 base64) = case B64.decode $ L.pack base64 of
   Right b64 -> Right $ Hash . C.unpack . B16.encode . SHA256.hashlazy $ b64
   Left err -> Left $ BadBase64 err

parseSerial :: String -> Either ParseError Int
parseSerial s = case reads s of
 [(d,"")] -> Right d
 _        -> Left $ BadSerial s

verify :: Bool -> e -> x -> Either e x
verify condition e x = if condition then Right x else Left e
