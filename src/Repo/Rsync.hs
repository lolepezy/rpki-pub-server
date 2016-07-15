{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Repo.Rsync (rsyncThread) where

import           Control.Exception
import           Control.Monad           (unless, forever)

import           Data.Function           (on)
import           Data.List               (isPrefixOf, sortBy, stripPrefix)
import           Data.Maybe              (listToMaybe)
import           Data.String.Interpolate

import           Config
import           Types
import qualified Util                    as U

import           Control.Concurrent.STM

import           System.Directory
import           System.FilePath
import qualified Log as L

import           Repo.State

rsyncThread :: L.Logger -> TChan ChangeSet -> AppState -> IO ()
rsyncThread logger syncChan AppState {
    currentState  = TRepoState{..},
    appConfig     = AppConfig {..}
    } = forever $ do
      pdus   <- atomically $ readTChan syncChan
      mapM_ (\pdu -> applyToFs pdu `catch` catchError) pdus

    where
      applyToFs :: QueryPdu -> IO ()
      applyToFs (QP (Publish u b64 _ _)) =
        doApply u $ \fileName filePath _ ->
          createDirectoryIfMissing True filePath >>
          U.writeB64 b64 fileName

      applyToFs (QW (Withdraw u _ _)) =
        doApply u $ \fileName filePath storePath -> do
          removeFile fileName
          prune filePath storePath

      doApply u f =
        case mapToPath u of
          Just (fileName, filePath, storePath) -> f fileName filePath storePath
          Nothing -> _err [i|Couldn't find FS mapping for url #{u} |]

      catchError :: SomeException -> IO ()
      catchError e = _err [i|Error occured #{e} |]

      _err s = L.err logger $ L.msg s

      prune path storePath = do
        files <- list path
        case files of
          [] -> do
            removeDirectory path
            let oneLevelUp = (joinPath . init . splitPath) path
            unless (normalise oneLevelUp == normalise storePath) $ prune oneLevelUp storePath
          _ -> return ()
        where
         list p = filter (\d -> d `notElem` [".", ".."]) <$> getDirectoryContents p

      mapToPath u = do
        -- get the longest FS prefix
        (prefix, path) <- listToMaybe $ sortBy (compare `on` (length . fst)) $ filter (\(m, _) -> m `isPrefixOf` us) rsyncRepoMap
        fName          <- (path </> ) <$> stripPrefix prefix us
        return (fName, dropFileName fName, path)
          where us = show u
