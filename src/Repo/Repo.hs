module Repo.Repo where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Config
import           Types
import qualified Store                  as ST
import qualified Log                  as LG

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


  let changeNotify pdus = tryPutTMVar syncV pdus >> writeTChan rsyncChan pdus

  let appState = AppState {
    appConfig = appConf,
    acidRepo = acid,
    currentState = tRepo,
    changeSync = changeNotify
  }
  rrdpLogger  <- LG.newLogger "RRDP logger"
  fsLogger    <- LG.newLogger "FS sync logger"
  rsyncLogger <- LG.newLogger "Rsync logger"

  void $ forkIO $ rrdpAccumulatorThread rrdpLogger fsSyncChan syncV appState
  void $ forkIO $ syncFSThread fsLogger fsSyncChan appState
  void $ forkIO $ rsyncThread rsyncLogger rsyncChan appState

  -- send notification to flush the initial state
  -- to RRDP and Rsync repostories
  void $ atomically $ do
    tState <- stateSnapshot $ rMap tRepo
    writeTChan fsSyncChan (tState, [])

    -- it should look better, but... create a list of artificial
    -- Publish pdus to recreate the whole rsync repository
    writeTChan rsyncChan [ QP (Publish u b64 Nothing (Tag "")) | (u, (b64, _)) <- tState ]

  return appState
