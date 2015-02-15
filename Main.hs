{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server (ServerPart, Response, takeRequestBody, askRq, ok, dir, 
                         method, unBody, toResponse, Method(POST))
import Happstack.Server.Types
import Happstack.Server.Env (simpleHTTP)
import qualified Data.ByteString.Lazy.Char8 as L

import Repo

main :: IO ()
main = simpleHTTP nullConf $ msum 
  [ do dir "message" $ do method POST 
                          processMessage ]


processMessage :: ServerPart L.ByteString
processMessage = do 
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of 
      Just rqbody -> return . unBody $ rqbody 
      Nothing     -> return ""

response :: L.ByteString -> L.ByteString
response xml = xml