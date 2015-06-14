module Util where

import qualified Data.ByteString.Char8  as S
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Types

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

leftmap :: (a -> c) -> Either a b -> Either c b
leftmap f (Left x)  = Left $ f x
leftmap _ (Right r) = Right r

mkBase64 :: L.ByteString -> Either ParseError Base64
mkBase64 s = case B64.decode s of
   Right b64 -> Right $ Base64 b64 $ getHash b64
   Left err  -> Left $ BadBase64 $ T.pack err

getHash :: L.ByteString -> Hash
getHash s = Hash . L.fromStrict . B16.encode . SHA256.hashlazy $ s

parseSerial :: String -> Either ParseError Int
parseSerial s = case maybeInt s of
  Just d  -> Right d
  Nothing -> Left $ BadSerial $ T.pack s

maybeInt :: String -> Maybe Int
maybeInt s = case reads s of
  [(d,"")] -> Just d
  _        -> Nothing

verify :: Bool -> e -> x -> Either e x
verify condition e x = if condition then Right x else Left e

text2Lbs :: T.Text -> L.ByteString
text2Lbs = L.fromStrict . TE.encodeUtf8

s2l :: S.ByteString -> L.ByteString
s2l = L.fromStrict

l2s :: L.ByteString -> S.ByteString
l2s = S.concat . L.toChunks

class BString s where
  lazy :: s -> L.ByteString
  strict :: s -> S.ByteString
  pack :: String -> s
  text :: T.Text -> s

instance BString L.ByteString where
  lazy = id
  strict = l2s
  pack = L.pack
  text = L.fromStrict . TE.encodeUtf8

instance BString S.ByteString where
  lazy = s2l
  strict = id
  pack = S.pack
  text = TE.encodeUtf8
