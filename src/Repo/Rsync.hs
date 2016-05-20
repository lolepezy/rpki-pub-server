{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Repo.Rsync where

import           Data.Function          (on)
import           Data.List              (isPrefixOf, sortBy, stripPrefix)
import           Data.Maybe             (listToMaybe)
import           Data.String.Interpolate

import           Config
import qualified Log                    as L
import           Types
import           qualified Util as U

import           Control.Concurrent.STM

import           System.Directory
import           System.FilePath

import           Repo.State

rsyncThread :: TChan ChangeSet -> AppState -> IO ()
rsyncThread syncChan AppState {
    currentState  = TRepoState{..},
    appConfig     = AppConfig {..}
    } = do
      pdus <- atomically $ readTChan syncChan
      mapM_ applyToFs pdus
    where
      applyToFs :: QueryPdu -> IO (Either RsyncError ())
      applyToFs (QP (Publish u b64 _ _)) =
        doApply u $ \fileName filePath ->
          createDirectoryIfMissing True filePath >>
          U.writeB64 b64 fileName

      applyToFs (QW (Withdraw u _ _)) =
        doApply u $ \fileName _ -> removeFile fileName

      doApply u f =
        case mapToPath u of
          Just (fileName, filePath) -> do
            _ <- f fileName filePath
            return $ Right ()
          Nothing -> do
            L.error_ [i| Couldn't find FS mapping for url #{u} |]
            return $ Right ()


      mapToPath u = do
        -- get the longest FS prefix
        (prefix, path) <- listToMaybe $ sortBy (compare `on` (length . fst)) $ filter (\(m, _) -> m `isPrefixOf` us) rsyncRepoMap
        fName          <- (path </> ) <$> stripPrefix prefix us
        return (fName, dropFileName fName)
          where us = show u



-- actors + 2-fase commmit
-- minimal cumulative notification time for the full graph
-- update of the tree-like cache from on-insert, on-update triggers
