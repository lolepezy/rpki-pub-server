{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Exit

import           Control.Concurrent.STM
import           Control.Monad               (msum)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Data.String.Interpolate
import qualified Data.ByteString.Lazy.Char8  as L
import           Happstack.Server            (ServerPart, simpleHTTP, askRq, 
                                              badRequest, notFound, internalServerError, 
                                              dir, method, ok, path)
--import           Happstack.Server.Env        (simpleHTTP)
import           Happstack.Server.Types

import           Types
import           RRDP.Repo
import           RRDP.XML

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
      Left e -> die $ [i|Repository at the location #{repoPath} is not found or can not be read, error: #{e}.|]
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
  L.pack $ [i|No delta for session_id=#{sessionId} and serial #{serial}|]
  
respondRRDP (Left (NoSnapshot (SessionId sessionId))) = notFound $ 
  L.pack $ [i|No snapshot for session_id=#{sessionId}|]

respondRRDP (Left (BadHash { passed = p, stored = s, uri = u })) = badRequest $ 
  L.pack $ [i|The replacement for the object #{u} has hash #{s} but is required to have #{p}|]

respondRRDP (Left (BadMessage parseError)) = badRequest $ L.pack $ [i|Message parse error #{parseError}|]

 
processMessage :: TVar Repository -> ServerPart L.ByteString
processMessage appState = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> respond rqbody
      Nothing     -> badRequest "Request has no body"
    where        
      respond rqbody = do
        rr <- liftIO $ getResponse $ unBody rqbody
        case rr of
          Right reply   -> ok reply
          e @ (Left _ ) -> respondRRDP e                       

      getResponse :: L.ByteString -> IO (Either RRDPError L.ByteString)
      getResponse request = atomically $ do
            state <- readTVar appState
            updateAppState $ applyToRepo state request

      updateAppState :: Either RRDPError (Repository, L.ByteString) -> STM (Either RRDPError L.ByteString)
      updateAppState (Left e) = return $ Left e
      updateAppState (Right (newRepo, reply)) = do
        writeTVar appState newRepo
        return $ Right reply        


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do 
  queryMessage            <- mapParseError $ parseMessage queryXml
  (newRepo, replyMessage) <- updateRepo repo queryMessage
  return (newRepo, createReply replyMessage)
  where 
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r


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
    
