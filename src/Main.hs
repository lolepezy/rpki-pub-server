{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad               (msum)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Lazy.Char8  as L
import           Happstack.Server            (ServerPart, simpleHTTP, askRq, setHeaderM,
                                              badRequest, notFound, dir, method, ok, path)
--import           Happstack.Server.Env        (simpleHTTP)
import           Happstack.Server.Types

import           Options

import           Types
import           Util
import           RRDP.Repo

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
main = runCommand $ \opts _ -> do
    existingRepo <- readRepoFromFS opts (SessionId $ currentSessionOpt opts)
    case existingRepo of
      Left e -> die $ "Repository at the location " ++ show (repositoryPathOpt opts) ++
                      " is not found or can not be read, error: " ++ show e ++ "."
      Right appState -> setupWebApp appState


setupWebApp :: AppState -> IO ()
setupWebApp appState = do
  let repository           = currentSession appState
  let serializedRepository = serializedCurrentRepo appState
  repositoryState     <- atomically $ newTVar repository
  serializedRepoState <- atomically $ newTVar serializedRepository
  simpleHTTP nullConf { port = defaultPort } $ msum
    [ dir "message" $ method POST >>
        (rpkiContentType $ processMessage repositoryState serializedRepoState appState),

      dir "notification.xml" $ method GET >>
        (rrdpContentType $ notificationXml serializedRepoState >>= respondRRDP),

      path $ \sessionId -> dir "snapshot.xml" $ method GET >>
        (rrdpContentType $ snapshotXmlFile serializedRepoState sessionId >>= respondRRDP),

      path $ \sessionId -> path $ \deltaNumber -> dir "delta.xml" $ method GET >>
        (rrdpContentType $ deltaXmlFile serializedRepoState sessionId deltaNumber >>= respondRRDP)
    ]


rpkiContentType, rrdpContentType :: ServerPart a -> ServerPart a
rpkiContentType response = setHeaderM "Content-Type" "application/rpki-publication" >> response

-- TODO Set the proper content type for XML files we serve
rrdpContentType response = setHeaderM "Content-Type" "text/xml" >> response


respondRRDP :: Either RRDPError L.ByteString -> ServerPart L.ByteString
respondRRDP (Right response) = ok response

respondRRDP (Left (NoDelta (SessionId sessionId) (Serial serial))) = notFound $
  L.pack $ "No delta for session_id " ++ show sessionId ++ " and serial " ++ show serial

respondRRDP (Left (NoSnapshot (SessionId sessionId))) = notFound $
  L.pack $ "No snapshot for session_id " ++ show sessionId

respondRRDP (Left (BadHash { passed = p, stored = s, uriW = u })) = badRequest $
  L.pack $ "The replacement for the object " ++ show u ++ " has hash "
               ++ show s ++ " but is expected to have hash " ++ show p

respondRRDP (Left (BadMessage parseError)) = badRequest $ L.pack $ "Message parse error " ++ show parseError

respondRRDP (Left e) = badRequest $ L.pack $ "Error: " ++ show e



processMessage :: TVar Repository -> TVar SerializedRepo -> AppState -> ServerPart L.ByteString
processMessage repository serializedRepo appState = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> respond rqbody
      Nothing     -> badRequest "Request has no body"
    where
      respond :: RqBody -> ServerPart L.ByteString
      respond rqbody = do
        -- apply changes to in-memory repo first
        _result <- liftIO $ applyChange $ unBody rqbody

        mapRrdp _result $ \(reply, repo) -> do
          syncResult <- liftIO $ syncToFS repo $ repoPath appState
          mapRrdp syncResult $ \_ -> ok reply

      mapRrdp (Left e) _ = respondRRDP $ Left e
      mapRrdp (Right x) f = f x

      applyChange :: L.ByteString -> IO (Either RRDPError (L.ByteString, Repository))
      applyChange request = atomically $ do
        repoState <- readTVar repository
        case applyToRepo repoState request of
          Right (newRepo, reply) -> do
            writeTVar repository newRepo
            writeTVar serializedRepo $ serializeRepo newRepo $ repoUrlBase appState
            return $ Right (reply, newRepo)
          Left  e -> return $ Left e

notificationXml :: TVar SerializedRepo -> ServerPart RRDPResponse
notificationXml serializedRepo = lift $ atomically $ do
    repo <- readTVar serializedRepo
    return $ Right $ notificationS repo

-- TODO Handle requests for non-current sessions as well
snapshotXmlFile :: TVar SerializedRepo -> String -> ServerPart RRDPResponse
snapshotXmlFile serializedRepo sessionId = lift $ atomically $ do
    repo <- readTVar serializedRepo
    return $ Right $ snapshotS repo

deltaXmlFile :: TVar SerializedRepo -> String -> Int -> ServerPart RRDPResponse
deltaXmlFile serializedRepo sessionId deltaNumber = lift $ atomically $ do
    repo <- readTVar serializedRepo
    return $ maybeToEither (NoDelta (SessionId sessionId) (Serial deltaNumber)) $
             serializedDelta repo deltaNumber
