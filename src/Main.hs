{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        (optional)
import           Control.Exception          (bracket)
import           Control.Monad              (msum)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import           Happstack.Server           (ServerPart, asContentType, askRq,
                                             badRequest, dir, look, method, ok,
                                             path, queryString, serveFile,
                                             setHeaderM, simpleHTTP, toResponse)
import           Happstack.Server.Types

import           Data.Acid                  (openLocalState)
import           Data.Acid.Local            (createCheckpointAndClose)
import           Options

import           Config
import qualified Repo.Repo                  as R
import qualified Repo.State                 as RS
import qualified Store                      as ST
import           Types
import qualified XML                        as XS

main :: IO ()
main = runCommand $ \opts _ -> do
  conf <- config opts
  case conf of
    Left e     -> print $ "Error occured while reading the config file: " ++ e
    Right c -> setupWebAppAcid c


setupWebAppAcid :: AppConfig -> IO ()
setupWebAppAcid appConf @ AppConfig { appPort = pPort } =
  bracket (openLocalState ST.initialStore) createCheckpointAndClose
  (\acid -> do
      appState <- R.initialAppState appConf acid
      simpleHTTP nullConf { port = pPort } $ msum [
          dir "message" $ method POST >>
            rpkiContentType (processMessageAcid appState),

          dir "notification.xml" $ method GET >>
            serveXml appConf "notification.xml",

          path $ \sessionId -> dir "snapshot.xml" $ method GET >>
            serveXml appConf (sessionId ++ "/snapshot.xml"),

          path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ method GET >>
            let
              -- force types here
              serveDelta :: String -> Integer -> ServerPart Response
              serveDelta sid dn = serveXml appConf $ sid ++ "/" ++ show dn ++ "/delta.xml"
            in serveDelta sessionId deltaNumber
        ]
    )
  where
    serveXml :: AppConfig -> String -> ServerPart Response
    serveXml AppConfig { repositoryPath = p } name =
      serveFile (asContentType "text/xml") $ p ++ name


processMessageAcid :: RS.AppState -> ServerPart Response
processMessageAcid appState = do
    req  <- askRq
    bod <- liftIO $ takeRequestBody req
    cId  <- optional $ queryString $ look "clientId"
    case (bod, cId) of
      (Just rqbody, Just cid) -> respond (ClientId cid) rqbody
      (_, Nothing)            -> badRequest `mkR` L.pack "No clientId provided"
      (Nothing, _)            -> badRequest `mkR` L.pack "Request has no body"
    where
      respond :: ClientId -> RqBody -> ServerPart Response
      respond clientId rqbody = do
        m <- liftIO $ RS.processMessage appState clientId $ unBody rqbody
        ok `mkR` XS.createReply (snd m)

      mkR resp output = resp $ toResponse output


rpkiContentType, rrdpContentType :: ServerPart a -> ServerPart a
rpkiContentType response = setHeaderM "Content-Type" "text/xml" >> response

-- TODO Set the proper content type for XML files we serve
rrdpContentType response = setHeaderM "Content-Type" "text/xml" >> response
