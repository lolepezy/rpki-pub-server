module Util where

import           Control.Exception

import qualified Crypto.Hash.SHA256          as SHA256
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.String.Conversions     as SC
import qualified Data.Text                   as T
import           Data.UUID
import           Types

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

base64bs :: Base64 -> S.ByteString
base64bs (Base64 b64 _) = cs . B64.encode $ b64

uuid2SessionId :: UUID -> SessionId
uuid2SessionId uuid = SessionId $ T.pack $ show uuid

type SBS = SC.SBS
type LBS = SC.LBS

cs :: SC.ConvertibleStrings s1 s2 => s1 -> s2
cs = SC.cs

lbslen :: LBS -> Integer
lbslen = toInteger . L.length

sbslen :: SBS -> Integer
sbslen = toInteger . S.length

writeB64 :: Base64 -> FilePath -> IO ()
writeB64 (Base64 b64 _) fileName = L.writeFile fileName b64

data IOAction = IOAction {
  execute  :: IO (),
  rollback :: IO ()
}

executeAll :: [IOAction] -> IO ()
executeAll [] = return ()
executeAll (a : as) = do
  execute a
  executeAll as `catch` rollbackAndRethrow
  where
    rollbackAndRethrow :: SomeException -> IO ()
    rollbackAndRethrow e = rollback a >> throw e
