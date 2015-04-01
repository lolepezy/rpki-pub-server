{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad               (msum)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Lazy.Char8  as L
import           Happstack.Server            (ServerPart, simpleHTTP, askRq,
                                              badRequest, notFound, dir, method, ok, path)
--import           Happstack.Server.Env        (simpleHTTP)
import           Happstack.Server.Types

import           Options

import           Types
import           RRDP.Repo
import           RRDP.XML

die :: String -> IO a
die err = do putStrLn err
             exitWith (ExitFailure 1)

data MainOptions = MainOptions {
  repositoryPathArg :: String,
  currentSessionArg :: String
}

instance Options MainOptions where
   defineOptions = pure MainOptions
       <*> simpleOption "repo-path" "" "Path to the repository"
       <*> simpleOption "session" "" "Actual session id"

main :: IO ()
main = runCommand $ \opts args -> do
    let repoPath = repositoryPathArg opts
    existingRepo <- readRepoFromFS repoPath (SessionId $ currentSessionArg opts)
    case existingRepo of
      Left e -> die $ "Repository at the location " ++ show repoPath ++
                      " is not found or can not be read, error: " ++ show e ++ "."
      Right appState -> case getCurrentSession appState of
        Nothing   -> die "Could not find current session in the repository"
        Just repo -> setupWebApp repo



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

respondRRDP (Left (BadHash { passed = p, stored = s, uriW = u })) = badRequest $
  L.pack $ "The replacement for the object " ++ show u ++ " has hash "
           ++ show s ++ " but is expected to have hash " ++ show p

respondRRDP (Left (BadMessage parseError)) = badRequest $
  L.pack $ "Message parse error " ++ show parseError


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
            case applyToRepo state request of
              Right (newRepo, reply) -> do
                writeTVar appState newRepo
                return $ Right reply
              Left e -> return $ Left e


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do
  queryMessage               <- mapParseError $ parseMessage queryXml
  let (newRepo, replyMessage) = updateRepo repo queryMessage
  return (newRepo, createReply replyMessage)
  where
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r


notificationXml :: TVar Repository -> ServerPart RRDPResponse
notificationXml repository = lift $ atomically $ do
    repo <- readTVar repository
    return $ getSnapshot repo

snapshotXmlFile :: TVar Repository -> String -> ServerPart RRDPResponse
snapshotXmlFile repository sessionId = lift . atomically $ do
    repo <- readTVar repository
    return $ getSnapshot repo

deltaXmlFile :: TVar Repository -> String -> Int -> ServerPart RRDPResponse
deltaXmlFile repository sessionId deltaNumber = lift . atomically $ do
    repo <- readTVar repository
    return $ getDelta repo (SessionId sessionId) (Serial deltaNumber)
