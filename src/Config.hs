module Config where

import           Options

defaultHost :: String
defaultHost = "localhost"

defaultPort :: Int
defaultPort = 9999

defaultFSSyncPeriod :: Int
defaultFSSyncPeriod = 30

defaultOldDataRetainPeriod :: Int
defaultOldDataRetainPeriod = 3600

data AppConfig = AppConfig {
  repositoryPathOpt      :: String,
  repositoryBaseUrlOpt   :: String,
  snapshotSyncPeriodOpt  :: Int,
  oldDataRetainPeriodOpt :: Int,
  appPortOpt             :: Int
}

instance Options AppConfig where
  defineOptions = pure AppConfig
      <*> simpleOption "repo-path" "" "Path to the repository"
      <*> simpleOption "repo-uri"
                       ("http://" ++ defaultHost ++ ":" ++ show defaultPort)
                       "URI to the repository root. Is used for generating URI for the notification files."
      <*> simpleOption "snapshot-sync-period" defaultFSSyncPeriod "Minimal period of time in seconds to synchronize snapshots to FS"
      <*> simpleOption "old-data-retain-period" defaultOldDataRetainPeriod "Minimal period of time in seconds to retain old session data"
      <*> simpleOption "port" defaultPort "Port to bind"
