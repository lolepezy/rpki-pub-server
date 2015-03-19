{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import           Control.Concurrent.STM
import           Control.Monad               (msum)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Lazy.Char8  as L
import           Happstack.Server            (ServerPart, simpleHTTP, askRq, badRequest, notFound, dir, method, ok, path)
--import           Happstack.Server.Env        (simpleHTTP)
import           Happstack.Server.Types

import           Types
import           RRDP.Repo

-- TODO get it from the config of command line
repoPath :: String
repoPath = "./repo"

die :: String -> IO a
die err = do putStrLn err
             exitWith (ExitFailure 1)

main :: IO ()
main = do
    existingRepo <- readRepo repoPath
    case existingRepo of
      Left e -> die $ "No repo!" ++ show e
      Right repo -> setupWebApp repo

setupWebApp :: Repository -> IO ()
setupWebApp repo = do
  appState <- atomically $ newTVar repo
  simpleHTTP nullConf { port = 9999 } $ msum
    [ dir "message" $ do 
        method POST
        processMessage appState,

      dir "notification.xml" $ do 
          method GET
          r <- notificationXml appState
          respondRRDP r,

      path $ \sessionId -> dir "snapshot.xml" $ do 
          method GET
          s <- snapshotXmlFile appState sessionId
          respondRRDP s,

      path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ do
          method GET
          d <- deltaXmlFile appState sessionId deltaNumber
          respondRRDP d
    ]

respondRRDP :: RRDPResponse -> ServerPart L.ByteString
respondRRDP (Right response) = ok response

respondRRDP (Left (NoDelta (SessionId sessionId) (Serial serial))) = notFound $ 
  L.pack $ "No delta for session_id " ++ show sessionId ++ " and serial " ++ show serial
  
respondRRDP (Left (NoSnapshot (SessionId sessionId))) = notFound $ 
  L.pack $ "No snapshot for session_id " ++ show sessionId

 
processMessage :: TVar Repository -> ServerPart L.ByteString
processMessage appState = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> lift . getResponse appState . unBody $ rqbody
      Nothing     -> badRequest "Request has no body"
    where
        getResponse :: TVar Repository -> L.ByteString -> IO L.ByteString
        getResponse as request = atomically $ do
            state <- readTVar as
            let (newRepo, res) = response state request
            writeTVar as newRepo
            return res

response :: Repository -> L.ByteString -> (Repository, L.ByteString)
response r xml = (r, xml)

notificationXml :: TVar Repository -> ServerPart RRDPResponse
notificationXml repository = lift $ atomically $ do
    r <- readTVar repository
    return $ getSnapshot r

snapshotXmlFile :: TVar Repository -> String -> ServerPart RRDPResponse
snapshotXmlFile repository sessionId = lift . atomically $ do
    repo <- readTVar repository
    return $ getSnapshot repo

deltaXmlFile :: TVar Repository -> String -> Int -> ServerPart RRDPResponse
deltaXmlFile repository sessionId deltaNumber = lift . atomically $ do
    repo <- readTVar repository
    return $ getDelta repo (SessionId sessionId) (Serial deltaNumber)
    
