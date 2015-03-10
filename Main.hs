{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Control.Monad (msum)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Happstack.Server (ServerPart, ServerPartT, Response, takeRequestBody, askRq, ok, dir, 
                         method, unBody, toResponse, Method(POST))
import Happstack.Server.Types
import Happstack.Server.Env (simpleHTTP)
import qualified Data.ByteString.Lazy.Char8 as L

import Repo

main :: IO ()
main = do
    appState <- atomically $ newTVar emptyRepo
    simpleHTTP nullConf $ msum 
      [ do dir "message" $ do method POST 
                              processMessage appState
      ]

processMessage :: TVar (Maybe Repository) -> ServerPart L.ByteString
processMessage appState = do 
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of 
      Just rqbody -> lift . (getResponse appState) . unBody $ rqbody 
      Nothing     -> return ""
    where
        getResponse :: TVar (Maybe Repository) -> L.ByteString -> IO L.ByteString
        getResponse appState request = atomically $ do
            state <- readTVar appState
            return $ case state of
                Nothing -> "nothing"
                Just s -> response s request

response :: Repository -> L.ByteString -> L.ByteString
response r xml = xml