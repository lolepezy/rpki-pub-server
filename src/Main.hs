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
import           Util
import           RRDP.Repo
import           RRDP.XML

die :: String -> IO a
die err = do putStrLn err
             exitWith (ExitFailure 1)

data AppOptions = AppOptions {
  repositoryPathArg :: String,
  currentSessionArg :: String
}

instance Options AppOptions where
   defineOptions = pure AppOptions
       <*> simpleOption "repo-path" "" "Path to the repository"
       <*> simpleOption "session" "" "Actual session id"

main :: IO ()
main = runCommand $ \opts args -> do
    let repoPath = repositoryPathArg opts
    existingRepo <- readRepoFromFS repoPath (SessionId $ currentSessionArg opts)
    case existingRepo of
      Left e -> die $ "Repository at the location " ++ show repoPath ++
                      " is not found or can not be read, error: " ++ show e ++ "."
      Right appState -> setupWebApp appState


setupWebApp :: AppState -> IO ()
setupWebApp appState = do
  let repository = currentSession appState
  repositoryState <- atomically $ newTVar repository
  simpleHTTP nullConf { port = 9999 } $ msum
    [ dir "message" $ do
        method POST
        processMessage repositoryState $ repoPath appState,

      dir "notification.xml" $ method GET >>
        notificationXml repositoryState >>= respondRRDP,

      path $ \sessionId -> dir "snapshot.xml" $ method GET >>
        snapshotXmlFile repositoryState sessionId >>= respondRRDP,

      path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ method GET >>
        deltaXmlFile repositoryState sessionId deltaNumber >>= respondRRDP
    ]

respondRRDP :: Either RRDPError L.ByteString -> ServerPart L.ByteString
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


processMessage :: TVar Repository -> String -> ServerPart L.ByteString
processMessage repository repoPath = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> respond rqbody
      Nothing     -> badRequest "Request has no body"
    where
      respond :: RqBody -> ServerPart L.ByteString
      respond rqbody = do
        _result <- liftIO $ getResponse $ unBody rqbody
        case _result of
          Right (reply, repo) -> do
            syncResult <- liftIO $ syncToFS repo repoPath
            case syncResult of
              Left e  -> respondRRDP $ Left e
              Right _ -> ok reply
          Left e              -> respondRRDP $ Left e

      getResponse :: L.ByteString -> IO (Either RRDPError (L.ByteString, Repository))
      getResponse request = atomically $ do
            state <- readTVar repository
            case applyToRepo state request of
              Right (newRepo, reply) -> do
                writeTVar repository newRepo
                return $ Right (reply, newRepo)
              Left e -> return $ Left e


applyToRepo :: Repository -> L.ByteString -> Either RRDPError (Repository, L.ByteString)
applyToRepo repo queryXml = do
  queryMessage               <- mapParseError $ parseMessage queryXml
  let (newRepo, replyMessage) = updateRepo repo queryMessage
  return (newRepo, createReply replyMessage)
  where
    mapParseError (Left e)  = Left $ BadMessage e
    mapParseError (Right r) = Right r


-- TODO Generate proper notification.xml
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
