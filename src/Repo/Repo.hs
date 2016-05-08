module Repo.Repo where

import           Control.Concurrent
import           Control.Concurrent.STM

import           Config
import qualified Store                  as ST

import           Repo.Rrdp
import           Repo.Rsync
import           Repo.State

initialAppState :: AppConfig -> ST.RepoState -> IO AppState
initialAppState appConf acid = do
  ros       <- ST.getAllObjects acid
  (tRepo, syncV, fsSyncChan, rsyncChan) <- atomically $ (,,,) <$>
                                            initialTRepo ros <*>
                                            newEmptyTMVar <*>
                                            newTChan <*>
                                            newTChan
  let appState = AppState {
    appConfig = appConf,
    acidRepo = acid,
    currentState = tRepo,
    changeSync = \pdus ->
      tryPutTMVar syncV pdus >>
      writeTChan rsyncChan pdus
  }
  _ <- forkIO $ rrdpSyncThread fsSyncChan syncV appState
  _ <- forkIO $ syncFSThread fsSyncChan appState
  _ <- forkIO $ rsyncThread rsyncChan appState

  return appState
