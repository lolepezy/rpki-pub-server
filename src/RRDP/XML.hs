{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RRDP.XML where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Types
import qualified Util                       as U

import           Network.URI

import qualified Text.Read                  as TR

import           Data.Char                  (isSpace)
import           Data.Maybe                 (mapMaybe, maybeToList)
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


tryParseList :: [XT.UNode T.Text] -> Either ParseError Bool
tryParseList [] = Left NoPdus
tryParseList children = if listExists children then
        if elementCount children > 1 then
          Left ListWithPdus
        else
          Right True
      else
        Right False
  where
    listExists = any (\e -> case e of
        XT.Element "list" _ _  -> True
        _                      -> False
      )
    elementCount x = length $ filter (\e -> case e of
        XT.Element {}  -> True
        _                 -> False
      ) x


getAttr :: [(T.Text, T.Text)] -> String -> Maybe T.Text
getAttr attrs name = case [ v | (n,v) <- attrs, n == T.pack name ] of
   [v] -> Just v
   _   -> Nothing

parseUri :: [(T.Text, T.Text)] -> Either ParseError URI
parseUri attrs = do
  u   <- U.maybeToEither NoURI $ getAttr attrs "uri"
  U.maybeToEither (BadURI u) (parseURI $ T.unpack u)

parseTag :: [(T.Text, T.Text)] -> Either ParseError Tag
parseTag attrs = fmap (Tag . T.unpack) $ U.maybeToEither NoTag $ getAttr attrs "tag"


parseVersion :: [(T.Text, T.Text)] -> T.Text -> Either ParseError Version
parseVersion attrs correctVersion = do
  sVersion  <- U.maybeToEither NoVersion $ getAttr attrs "version"
  vsVersion <- U.verify (sVersion == correctVersion) (BadVersion sVersion) sVersion
  version   <- U.maybeToEither (BadVersion sVersion) (TR.readMaybe $ T.unpack vsVersion)
  return $ Version version


parsePublish :: [(T.Text, T.Text)] -> T.Text -> Either ParseError Publish
parsePublish attrs b64 = do
  uri    <- parseUri attrs
  tag    <- parseTag attrs
  base64 <- U.mkBase64 $ U.text2Lbs b64
  let hash  = Hash . U.text2Lbs <$> getAttr attrs "hash"
  return $ Publish uri base64 hash tag


parseWithdraw :: [(T.Text, T.Text)] -> Either ParseError Withdraw
parseWithdraw attrs  = do
  uri  <- parseUri attrs
  tag  <- parseTag attrs
  hash <- U.maybeToEither NoHash $ getAttr attrs "hash"
  return $ Withdraw uri (Hash (U.text2Lbs hash)) tag


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


parseMessage :: LBS.ByteString -> Either ParseError (QMessage QueryPdu)
parseMessage xml = parseGeneric xml $ \attrs children -> do
  version   <- parseVersion attrs $ T.pack protocolVersion
  maybeList <- tryParseList children
  if maybeList then
    return ListMessage
  else do
    pdus <- extractPdus children (\p t -> fmap QP (parsePublish p t)) (fmap QW . parseWithdraw)
    return $ PduMessage version pdus


verifyXmlNs :: T.Text -> Either ParseError ()
verifyXmlNs xmlNs = U.verify ("HTTP://www.ripe.net/rpki/rrdp" == xmlNs) (BadXmlNs xmlNs) ()


type Elem s = XT.Node String s

mkElem :: U.BString s => String -> [(String, s)] -> [Elem s] -> Elem s
mkElem = XT.Element

pduElem :: QueryPdu -> Elem BS.ByteString
pduElem (QP (Publish u b64 mHash tag)) = publishElemWTag u b64 mHash tag
pduElem (QW (Withdraw u hash tag))     = withdrawElemWTag u hash tag

publishElemWTag :: URI -> Base64 -> Maybe Hash -> Tag -> Elem BS.ByteString
publishElemWTag uri base64 mHash (Tag t) = mkElem "publish"
  ([("uri", U.pack $ show uri), ("tag", BS.pack t)] ++ [("hash", U.strict h) | Hash h <- maybeToList mHash])
  [XT.Text $ U.base64bs base64]

publishElem :: URI -> Base64 -> Maybe Hash -> Elem BS.ByteString
publishElem uri base64 mHash = mkElem "publish"
  (("uri", U.pack $ show uri) : [("hash", U.strict h) | Hash h <- maybeToList mHash])
  [XT.Text $ U.base64bs base64]


withdrawElemWTag :: URI -> Hash -> Tag -> Elem BS.ByteString
withdrawElemWTag uri (Hash hash) (Tag t) = mkElem "withdraw" [("uri", U.pack $ show uri), ("hash", U.strict hash), ("tag", BS.pack t)] []

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


createReply :: Reply -> LBS.ByteString
createReply reply = XF.formatNode $
  mkElem "msg" [("version", BS.pack protocolVersion), ("type", BS.pack "reply")] $ formatReply reply
  where
    formatReply :: Reply -> [ Elem BS.ByteString ]
    formatReply Success = [ mkElem "success" [] [] ]
    formatReply (ListReply listPdus) = map (\(ListPdu uri (Hash hash)) ->
        mkElem "list" [("uri", BS.pack $ show uri), ("hash", U.strict hash)] []
      ) listPdus
    formatReply (Errors errors) = map reportError errors

    reportError :: RepoError -> Elem BS.ByteString
    reportError (XMLError parseError)  = errorElem "xml_error"          [ textElem $ BS.pack $ show parseError ]
    reportError PermissionFailure{..}  = errorElem "permission_failure" [XT.Text $ BS.pack message]
      where message = "Cannot update object " ++ show oUri ++
                     " on behalf of " ++ show queryClientId ++
                     ", it belongs to the client " ++ show storedClientId

    reportError (BadCmsSignature pdu)              = errorElem "bad_cms_signature"      [pduE pdu]
    reportError (ObjectAlreadyPresent pdu)         = errorElem "object_already_present" [pduE pdu]
    reportError (NoObjectPresent uri (Hash hash))  = errorElem "no_object_present"      [XT.Text $ BS.pack message]
      where message = "Object " ++ show uri ++ " with hash " ++ show hash ++ " is not found."

    reportError (NoObjectMatchingHash (Hash hash) pdu) = errorElem "no_object_matching_hash" [ textElem $ BS.pack message, pduE pdu ]
      where message = "Object in the <publish> element doesn't have hash " ++ show hash

    reportError (ConsistencyProblem text) = errorElem "consistency_problem" [ textElem $ U.text2bs text ]
    reportError (OtherError text)         = errorElem "other_error"        [ textElem $ U.text2bs text ]

    pduE p = mkElem "failed_pdu" [] [pduElem p]
    errorElem code = mkElem "report_error" [("error_code", code)]
    textElem message = mkElem "error_text" [] [XT.Text message]

format :: Elem BS.ByteString -> LBS.ByteString
format = XF.formatNode
