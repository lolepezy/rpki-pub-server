module Util where

import qualified Data.ByteString.Char8  as S
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16 as B16
import           Data.Hashable
import Data.UUID
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.URI (URI)

import Types

nextS :: Serial -> Serial
nextS (Serial s) = Serial $ s + 1

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
getHash = Hash . L.fromStrict . B16.encode . SHA256.hashlazy

maybeInteger :: String -> Maybe Integer
maybeInteger s = case reads s of
  [(d,"")] -> Just d
  _        -> Nothing

verify :: Bool -> e -> x -> Either e x
verify condition e x = if condition then Right x else Left e

text2Lbs :: T.Text -> L.ByteString
text2Lbs = L.fromStrict . TE.encodeUtf8

text2bs :: T.Text -> S.ByteString
text2bs = TE.encodeUtf8

base64bs :: Base64 -> S.ByteString
base64bs (Base64 b64 _) = strict . B64.encode $ b64

uuid2SessionId :: UUID -> SessionId
uuid2SessionId uuid = SessionId $ T.pack $ show uuid

class BString s where
  lazy :: s -> L.ByteString
  strict :: s -> S.ByteString
  pack :: String -> s
  text :: T.Text -> s
  length :: s -> Integer

instance BString L.ByteString where
  lazy = id
  strict = S.concat . L.toChunks
  pack = L.pack
  text = L.fromStrict . TE.encodeUtf8
  length = toInteger . L.length

instance BString S.ByteString where
  lazy = L.fromStrict
  strict = id
  pack = S.pack
  text = TE.encodeUtf8
  length = toInteger . S.length

instance Hashable URI where
  hashWithSalt salt u = hashWithSalt salt (S.pack $ show u :: S.ByteString)
