{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Configurator as C
import Data.Configurator.Types
import Control.Exception (catch)

import Data.Text (unpack)

import           Options

data AppOptions = AppOptions {
  configPath :: String
}

data AppConfig  = AppConfig {
  repositoryPath      :: String,
  repositoryBaseUrl   :: String,
  rsyncBasePath       :: String,
  snapshotSyncPeriod  :: Int,
  oldDataRetainPeriod :: Int,
  appPort             :: Int,
  rsyncRepoMap        :: [(String, String)]
} deriving (Show)

config :: AppOptions -> IO (Either String AppConfig)
config (AppOptions confPath) = readConfig `catch` catchError
  where
    readConfig = do
      conf          <- C.load [C.Required confPath]
      port          <- C.lookupDefault 19999 conf "repository.rrdpPath"
      repoPath      <- C.require conf "repository.rrdpPath"
      repoBaseUrl   <- C.require conf "repository.baseUrl"
      rsyncPath     <- C.require conf "repository.rsync.basePath"
      urlMapping    <- C.require conf "repository.rsync.urlMapping"
      syncPeriod    <- C.lookupDefault 10 conf "snaphotSyncPeriod"
      retainPeriod  <- C.lookupDefault 3600 conf "oldDataRetainPeriod"
      return $ Right AppConfig {
        repositoryPath = repoPath,
        repositoryBaseUrl   = repoBaseUrl,
        rsyncBasePath       = rsyncPath,
        snapshotSyncPeriod  = syncPeriod,
        oldDataRetainPeriod = retainPeriod,
        appPort             = port,
        rsyncRepoMap        = urlMapping
      }
    catchError :: KeyError -> IO (Either String AppConfig)
    catchError (KeyError key) = return $ Left $ unpack key ++ " must be defined"


instance Options AppOptions where
  defineOptions = pure AppOptions
      <*> simpleOption "config-path" "rpki-pub-server.conf" "Path to the config file"
