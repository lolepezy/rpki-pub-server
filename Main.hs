{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import           Control.Concurrent.STM
import           Control.Monad               (msum)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Lazy.Char8  as L
import           Happstack.Server            (ServerPart, askRq, badRequest, dir, method, ok, path)
import           Happstack.Server.Env        (simpleHTTP)
import           Happstack.Server.Types

import           RRDPRepo

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
      Nothing -> die "No repo!"
      Just repo -> webApp repo
    where 
      webApp repo = do
        appState <- atomically $ newTVar repo
        simpleHTTP nullConf $ msum
          [ dir "message" $ do method POST
                               processMessage appState,
            dir "notification.xml" $ do method GET
                                        notificationXml appState,
            snapshotPath $ snapshotXml appState,
            deltaPath $ deltaXml appState
          ]

      snapshotPath action = path $ \sessionId -> 
        dir "snapshot.xml" $ do
          method GET
          action sessionId

      deltaPath action = path $ \sessionId -> 
        path $ \deltaNumber -> dir "delta.xml" $ do 
          method GET
          action sessionId deltaNumber

processMessage :: TVar Repository -> ServerPart L.ByteString
processMessage appState = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
      Just rqbody -> lift . getResponse appState . unBody $ rqbody
      Nothing     -> badRequest "Request has no body"
    where
        getResponse :: TVar Repository -> L.ByteString -> IO L.ByteString
        getResponse appState request = atomically $ do
            state <- readTVar appState
            let (newRepo, res) = response state request
            writeTVar appState newRepo
            return res

response :: Repository -> L.ByteString -> (Repository, L.ByteString)
response r xml = (r, xml)

notificationXml :: TVar Repository -> ServerPart L.ByteString
notificationXml repository = lift $ atomically $ do
    r <- readTVar repository
    return $ getSnapshot r

snapshotXml :: TVar Repository -> String -> ServerPart L.ByteString
snapshotXml repository sessionId = lift $ atomically $ do
    r <- readTVar repository
    return $ getSnapshot r

deltaXml :: TVar Repository -> String -> Int -> ServerPart L.ByteString
deltaXml appState sessionId deltaNumber = ok "Fine delta"
