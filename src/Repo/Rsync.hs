{-# LANGUAGE RecordWildCards    #-}

module Repo.Rsync where

import Types
import Config
import Network.URI

import           Control.Concurrent.STM

import Repo.State

save :: AppConfig -> URI -> Base64 -> IO ()
save appConf uri (Base64 b64 _) = return ()

rsyncThread :: TChan ChangeSet -> AppState -> IO ()
rsyncThread syncChan AppState {
    currentState  = TRepoState{..},
    appConfig     = AppConfig { snapshotSyncPeriodOpt = syncPeriod }
    } = do
      return ()
