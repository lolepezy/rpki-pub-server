{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module RRDP.XML where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

import           Types
import qualified Util as U

import Network.URI

import Control.Applicative ((<*>), (<$>))

import qualified Text.Read as TR

import qualified Data.Text as T
import qualified Text.XML.Expat.Tree as XT
import qualified Text.XML.Expat.Format as XF


data Pdu p w = Pub p | With w | Nope

extractPdus :: [XT.UNode T.Text] ->
               ([(T.Text, T.Text)] -> T.Text -> Either ParseError p) ->
               ([(T.Text, T.Text)] -> Either ParseError w) ->
               Either ParseError ([p], [w])
extractPdus pduChildren publishC withdrawC = (,) <$>
  sequence [ pdu | Pub pdu  <- pdus ] <*>
  sequence [ pdu | With pdu <- pdus ]
  where
    pdus = map (\e -> case e of
        XT.Element "publish"  attrs pChildren  -> Pub  $ publishC attrs (getBase64 pChildren)
        XT.Element "withdraw" attrs _          -> With $ withdrawC attrs
        _                                      -> Nope
        ) pduChildren

    getBase64 pChildren = T.concat [ T.filter (`notElem` " \n\t") t | XT.Text t <- pChildren ]


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
  let hash  = fmap (Hash . U.text2Lbs) $ getAttr attrs "hash"
  return $ pubC uri base64 hash

parsePublish :: [(T.Text, T.Text)] -> T.Text -> Either ParseError Publish
parsePublish = parsePublishWithHash Publish

parsePublishQ :: [(T.Text, T.Text)] -> T.Text -> Either ParseError QueryPdu
parsePublishQ = parsePublishWithHash PublishQ



withdraw :: (URI -> Hash -> w) -> [(T.Text, T.Text)] -> Either ParseError w
withdraw withC attrs  = do
  uri  <- parseUri attrs
  hash <- U.maybeToEither NoHash $ getAttr attrs "hash"
  return $ withC uri $ Hash $ U.text2Lbs hash

parseWithdrawQ :: [(T.Text, T.Text)] -> Either ParseError QueryPdu
parseWithdrawQ = withdraw WithdrawQ

parseWithdraw :: [(T.Text, T.Text)] -> Either ParseError Withdraw
parseWithdraw  = withdraw Withdraw


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
  (ps, ws) <- extractPdus children parsePublishQ parseWithdrawQ
  return $ Message version $ ps ++ ws

parseDelta :: LBS.ByteString -> Either ParseError Delta
parseDelta xml = parseGeneric xml $ \attrs children -> do
  (sessionId, version, serial) <- extractCommonAttrs attrs
  (ps, ws)                     <- extractPdus children parsePublish parseWithdraw
  return $ Delta (DeltaDef version sessionId serial) ps ws

parseSnapshot :: LBS.ByteString -> Either ParseError Snapshot
parseSnapshot xml = parseGeneric xml $ \attrs children -> do
  (sessionId, version, serial) <- extractCommonAttrs attrs
  (pdus, _)                    <- extractPdus children parsePublish parseWithdraw
  return $ Snapshot (SnapshotDef version sessionId serial) pdus


extractCommonAttrs :: [(T.Text, T.Text)] -> Either ParseError (SessionId, Version, Serial)
extractCommonAttrs attrs = do
  sessionId <- U.maybeToEither NoSessionId $ getAttr attrs "session_id"
  sSerial   <- U.maybeToEither NoSerial $ getAttr attrs "serial"
  version   <- parseVersion attrs "1"
  serial    <- U.maybeToEither (BadSerial sSerial) (TR.readMaybe $ T.unpack sSerial)
  return (SessionId sessionId, version, Serial serial)


verifyXmlNs xmlNs = U.verify ("HTTP://www.ripe.net/rpki/rrdp" == xmlNs) (BadXmlNs xmlNs) ()


type Elem s = XT.Node String s

mkElem :: U.BString s => String -> [(String, s)] -> [Elem s] -> Elem s
mkElem name attrs children = XT.Element name attrs children

publishElem :: URI -> Base64 -> Maybe Hash -> Elem BS.ByteString
publishElem uri base64 Nothing            = mkElem "publish" [("uri", U.pack $ show uri)] [XT.Text $ U.base64bs base64]
publishElem uri base64 (Just (Hash hash)) = mkElem "publish" [("uri", U.pack $ show uri), ("hash", U.strict hash)] [XT.Text $ U.base64bs base64]

withdrawElem :: URI -> Hash -> Elem BS.ByteString
withdrawElem uri (Hash hash) = mkElem "withdraw" [("uri", U.pack $ show uri), ("hash", U.strict hash)] []

snapshotElem :: U.BString s => SnapshotDef -> [Elem s] -> Elem s
snapshotElem (SnapshotDef version sId serial) = commonElem version sId serial "snapshot"

deltaElem :: U.BString s => DeltaDef -> [Elem s] -> [Elem s] -> Elem s
deltaElem (DeltaDef version sId serial) publishElements withdrawElements =
  commonElem version sId serial "delta" (publishElements ++ withdrawElements)

notificationElem :: U.BString s => SnapshotDef -> [Elem s] -> Elem s
notificationElem (SnapshotDef version sId serial) elements = commonElem version sId serial "notification" elements

commonElem :: U.BString s => Version -> SessionId -> Serial -> String -> [XT.Node String s] -> XT.Node String s
commonElem (Version version) (SessionId uuid) (Serial serial) elemName children = mkElem elemName [
    ("xmlns", U.pack "http://www.ripe.net/rpki/rrdp"),
    ("version", U.pack $ show version),
    ("serial",  U.pack $ show serial),
    ("session_id", U.text uuid)
  ] children


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
