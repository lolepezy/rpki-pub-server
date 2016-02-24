{-# LANGUAGE OverloadedStrings #-}

import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception          (bracket)
import           Control.Monad              (msum)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import           Happstack.Server           (ServerPart, ToMessage,
                                             asContentType, askRq, badRequest,
                                             dir, method, notFound, ok, path,
                                             serveFile, setHeaderM, simpleHTTP,
                                             toResponse)
import           Happstack.Server.Types

import           Data.Acid                  (AcidState, openLocalState)
import           Data.Acid.Local            (createCheckpointAndClose)

import qualified Data.Text                  as T
import           Options

import           Config
import           RRDP.Repo
import qualified Store                      as ST
import           Types

die :: String -> IO a
die err = do putStrLn err
             exitWith (ExitFailure 1)

main :: IO ()
main = runCommand $ \opts _ -> setupWebAppAcid opts


setupWebAppAcid :: AppConfig -> IO ()
setupWebAppAcid appContext @ AppConfig { currentSessionOpt = sId } =
  bracket (openLocalState initialState) createCheckpointAndClose
  (\acid -> do
      changeSet <- newEmptyMVar
      forkIO $ rrdpSyncThread acid appContext changeSet
      simpleHTTP nullConf { port = defaultPort } $ msum
        [ dir "message" $ method POST >>
             rpkiContentType (processMessageAcid acid appContext changeSet),

          dir "notification.xml" $ method GET >>
            serveXml appContext "notification.xml",

          path $ \sessionId -> dir "snapshot.xml" $ method GET >>
            serveXml appContext (sessionId ++ "/snapshot.xml"),

          path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ method GET >>
            let
              serveDelta :: String -> Integer -> ServerPart Response
              serveDelta sid dn = serveXml appContext $ sid ++ "/" ++ show dn ++ "/delta.xml"
            in serveDelta sessionId deltaNumber
        ]
    )
  where
    serveXml :: AppConfig -> String -> ServerPart Response
    serveXml AppConfig { repositoryPathOpt = p } name =
      serveFile (asContentType "application/rpki-publication") $ p ++ name
    initialState = ST.initialStore (SessionId $ T.pack sId)


processMessageAcid :: AcidState ST.Repo -> AppConfig -> SyncFlag -> ServerPart Response
processMessageAcid acid appContext changeSet = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> respond rqbody
      Nothing     -> mkResp badRequest $ L.pack "Request has no body"
    where
      -- TODO Read in from the request as well
      clientId = ClientId "defaultClientId"

      respond :: RqBody -> ServerPart Response
      respond rqbody = do
        m <- liftIO $ processMessage acid appContext clientId changeSet $ unBody rqbody
        respondRRDP $ snd <$> m


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
