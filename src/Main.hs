{-# LANGUAGE OverloadedStrings #-}

import           System.Exit

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception          (bracket)
import           Control.Monad              (msum)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import qualified Data.ByteString.Lazy.Char8 as L
import           Happstack.Server           (ServerPart, asContentType, askRq,
                                             badRequest, dir, method, notFound,
                                             ok, path, serveFile, setHeaderM,
                                             simpleHTTP, toResponse, ToMessage)
import           Happstack.Server.Types

import           Data.Acid                  (AcidState, openLocalState)
import           Data.Acid.Advanced         (query', update')

import           Data.Acid.Local            (createCheckpointAndClose)


import qualified Data.Text                  as T
import           Options

import           RRDP.Repo
import qualified RRDP.XML                   as XS
import qualified Store                      as ST
import           Types
import           Util

die :: String -> IO a
die err = do putStrLn err
             exitWith (ExitFailure 1)

defaultHost :: String
defaultHost = "localhost"
defaultPort :: Int
defaultPort = 9999

instance Options AppOptions where
  defineOptions = pure AppOptions
      <*> simpleOption "repo-path" "" "Path to the repository"
      <*> simpleOption "repo-uri"
                       ("http://" ++ defaultHost ++ ":" ++ show defaultPort)
                       "URI to the repository root. Is used for generating URI for the notification files."
      <*> simpleOption "session" "" "Actual session id"

main :: IO ()
main = runCommand $ \opts _ -> setupWebAppAcid opts


setupWebAppAcid :: AppOptions -> IO ()
setupWebAppAcid appOptions@(AppOptions { currentSessionOpt = sId }) =
  bracket (openLocalState $ initialState sId) createCheckpointAndClose
  (\acid ->
      simpleHTTP nullConf { port = defaultPort } $ msum
        [ dir "message" $ method POST >>
             rpkiContentType (processMessageAcid acid),

          dir "notification.xml" $ method GET >>
            serveXml appOptions "notification.xml",

          path $ \sessionId -> dir "snapshot.xml" $ method GET >>
            serveXml appOptions (sessionId ++ "/snapshot.xml"),

          path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ method GET >>
            let
              serveDelta :: String -> Integer -> ServerPart Response
              serveDelta sid dn = serveXml appOptions $ sid ++ "/" ++ show dn ++ "/delta.xml"
            in serveDelta sessionId deltaNumber
        ]
    )
  where
    serveXml :: AppOptions -> String -> ServerPart Response
    serveXml (AppOptions { repositoryPathOpt = path }) name =
      serveFile (asContentType "application/rpki-publication") $ path ++ name
    initialState sId = ST.initialStore (SessionId $ T.pack sId)

rpkiContentType, rrdpContentType :: ServerPart a -> ServerPart a
rpkiContentType response = setHeaderM "Content-Type" "application/rpki-publication" >> response

-- TODO Set the proper content type for XML files we serve
rrdpContentType response = setHeaderM "Content-Type" "text/xml" >> response


respondRRDP :: Either RRDPError L.ByteString -> ServerPart Response
respondRRDP (Right response) = mkResp ok response

respondRRDP (Left (NoDelta (SessionId sessionId) (Serial serial))) = mkResp notFound $
  L.pack $ "No delta for session_id " ++ show sessionId ++ " and serial " ++ show serial

respondRRDP (Left (NoSnapshot (SessionId sessionId))) = mkResp notFound $
  L.pack $ "No snapshot for session_id " ++ show sessionId

-- respondRRDP (Left (BadHash { passed = p, stored = s, uriW = u })) = badRequest $
--   L.pack $ "The replacement for the object " ++ show u ++ " has hash "
--                ++ show s ++ " but is expected to have hash " ++ show p

respondRRDP (Left (BadMessage parseError)) = mkResp badRequest $ L.pack $ "Message parse error " ++ show parseError

respondRRDP (Left e) = mkResp badRequest $ L.pack $ "Error: " ++ show e

mkResp :: ToMessage a => (Response -> t) -> a -> t
mkResp resp output = resp $ toResponse output


processMessageAcid :: AcidState ST.Repo -> ServerPart Response
processMessageAcid acid = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> respond rqbody
      Nothing     -> mkResp badRequest $ L.pack "Request has no body"
    where
      -- TODO Read in from the request as well
      clientId = ClientId "defaultClientId"

      respond :: RqBody -> ServerPart Response
      respond rqbody = respondRRDP $ snd <$> processMessage2 acid (unBody rqbody) clientId
