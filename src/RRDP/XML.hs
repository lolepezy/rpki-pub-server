{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module RRDP.XML where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Types
import qualified Util                       as U

import           Network.URI

import qualified Text.Read                  as TR

import           Data.Char                 (isSpace)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text                  as T
import qualified Text.XML.Expat.Format      as XF
import qualified Text.XML.Expat.Tree        as XT


extractPdus :: [XT.UNode T.Text] ->
               ([(T.Text, T.Text)] -> T.Text -> Either ParseError pdu) ->
               ([(T.Text, T.Text)] -> Either ParseError pdu) ->
               Either ParseError [pdu]
extractPdus pduChildren publishC withdrawC = sequence $ mapMaybe (\e -> case e of
        XT.Element "publish"  attrs pChildren  -> Just $ publishC attrs (getBase64 pChildren)
        XT.Element "withdraw" attrs _          -> Just $ withdrawC attrs
        XT.Element n _ _                       -> Just $ Left $ UnexpectedElement n
        _                                      -> Nothing
      ) pduChildren
  where
    getBase64 pChildren = T.concat [ T.filter (not . isSpace) t | XT.Text t <- pChildren ]


getAttr :: [(T.Text, T.Text)] -> String -> Maybe T.Text
getAttr attrs name = case [ v | (n,v) <- attrs, n == T.pack name ] of
   [v] -> Just v
   _   -> Nothing

parseUri :: [(T.Text, T.Text)] -> Either ParseError URI
parseUri attrs = do
  u   <- U.maybeToEither NoURI $ getAttr attrs "uri"
  U.maybeToEither (BadURI u) (parseURI $ T.unpack u)

parseVersion :: [(T.Text, T.Text)] -> T.Text -> Either ParseError Version
parseVersion attrs correctVersion = do
  sVersion  <- U.maybeToEither NoVersion $ getAttr attrs "version"
  vsVersion <- U.verify (sVersion == correctVersion) (BadVersion sVersion) sVersion
  version   <- U.maybeToEither (BadVersion sVersion) (TR.readMaybe $ T.unpack vsVersion)
  return $ Version version


parsePublishWithHash :: (URI -> Base64 -> Maybe Hash -> p) -> [(T.Text, T.Text)] -> T.Text -> Either ParseError p
parsePublishWithHash pubC attrs b64 = do
  uri    <- parseUri attrs
  base64 <- U.mkBase64 $ U.text2Lbs b64
  let hash  = Hash . U.text2Lbs <$> getAttr attrs "hash"
  return $ pubC uri base64 hash

parsePublish :: [(T.Text, T.Text)] -> T.Text -> Either ParseError QueryPdu
parsePublish = parsePublishWithHash (\u b h -> QP $ Publish u b h)


withdraw :: (URI -> Hash -> w) -> [(T.Text, T.Text)] -> Either ParseError w
withdraw withC attrs  = do
  uri  <- parseUri attrs
  hash <- U.maybeToEither NoHash $ getAttr attrs "hash"
  return $ withC uri $ Hash $ U.text2Lbs hash

parseWithdraw :: [(T.Text, T.Text)] -> Either ParseError QueryPdu
parseWithdraw  = withdraw (\u h -> QW $ Withdraw u h)


parseGeneric :: LBS.ByteString ->
                ([(T.Text, T.Text)] -> [XT.UNode T.Text] -> Either ParseError document) ->
                Either ParseError document
parseGeneric xml extract = case err of
  Nothing                            -> extractData tree
  Just (XT.XMLParseError e location) -> Left $ BadXml $ T.pack $ e ++ " at " ++ show location
  where
    (tree, err) = XT.parse XT.defaultParseOptions xml :: (XT.UNode T.Text, Maybe XT.XMLParseError)

    extractData (XT.Element _ attrs children) = extract attrs children
    extractData _ = Left $ BadXml $ T.pack "Couldn't find the main document element"


parseMessage :: LBS.ByteString -> Either ParseError QMessage
parseMessage xml = parseGeneric xml $ \attrs children -> do
  version  <- parseVersion attrs "3"
  pdus <- extractPdus children parsePublish parseWithdraw
  return $ Message version pdus


extractCommonAttrs :: [(T.Text, T.Text)] -> Either ParseError (SessionId, Version, Serial)
extractCommonAttrs attrs = do
  sessionId <- U.maybeToEither NoSessionId $ getAttr attrs "session_id"
  sSerial   <- U.maybeToEither NoSerial $ getAttr attrs "serial"
  version   <- parseVersion attrs "1"
  serial    <- U.maybeToEither (BadSerial sSerial) (TR.readMaybe $ T.unpack sSerial)
  return (SessionId sessionId, version, Serial serial)


verifyXmlNs :: T.Text -> Either ParseError ()
verifyXmlNs xmlNs = U.verify ("HTTP://www.ripe.net/rpki/rrdp" == xmlNs) (BadXmlNs xmlNs) ()


type Elem s = XT.Node String s

mkElem :: U.BString s => String -> [(String, s)] -> [Elem s] -> Elem s
mkElem = XT.Element

publishElem :: URI -> Base64 -> Maybe Hash -> Elem BS.ByteString
publishElem uri base64 Nothing            = mkElem "publish" [("uri", U.pack $ show uri)] [XT.Text $ U.base64bs base64]
publishElem uri base64 (Just (Hash hash)) = mkElem "publish" [("uri", U.pack $ show uri), ("hash", U.strict hash)] [XT.Text $ U.base64bs base64]

withdrawElem :: URI -> Hash -> Elem BS.ByteString
withdrawElem uri (Hash hash) = mkElem "withdraw" [("uri", U.pack $ show uri), ("hash", U.strict hash)] []

snapshotElem :: U.BString s => SnapshotDef -> [Elem s] -> Elem s
snapshotElem (SnapshotDef version sId serial) = commonElem version sId serial "snapshot"

deltaElem :: U.BString s => DeltaDef -> [Elem s] -> Elem s
deltaElem (DeltaDef version sId serial) = commonElem version sId serial "delta"

notificationElem :: U.BString s => SnapshotDef -> [Elem s] -> Elem s
notificationElem (SnapshotDef version sId serial) = commonElem version sId serial "notification"

commonElem :: U.BString s => Version -> SessionId -> Serial -> String -> [XT.Node String s] -> XT.Node String s
commonElem (Version version) (SessionId uuid) (Serial serial) elemName = mkElem elemName [
    ("xmlns", U.pack "http://www.ripe.net/rpki/rrdp"),
    ("version", U.pack $ show version),
    ("serial",  U.pack $ show serial),
    ("session_id", U.text uuid)
  ]


createReply :: RMessage -> LBS.ByteString
createReply (Message version pdus) = XF.formatNode $
  mkElem "msg" [("version", printV version), ("type", BS.pack "reply")] $ map (\pdu ->
      case pdu of
        PublishR uri  -> pduElem "publish" uri
        WithdrawR uri -> pduElem "withdraw" uri
        ReportError e -> reportErrorElem e
      ) pdus
  where
      printV (Version v) = BS.pack $ show v
      pduElem name uri = mkElem name [("uri", BS.pack $ show uri)] []
      reportErrorElem e = mkElem "report_error" [("error_code", BS.pack $ show e)] []

format :: Elem BS.ByteString -> LBS.ByteString
format = XF.formatNode
