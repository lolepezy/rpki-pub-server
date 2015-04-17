module Main where

import Control.Monad
import Network.URI
import Test.HUnit
import qualified Data.Map as M

import Types
import RRDP.Repo

sessionId = "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

emptyRepo :: Repository
emptyRepo = Repository ( Snapshot (SnapshotDef (Version 1) (SessionId sessionId) (Serial 1)) []) M.empty

serialize rr = do
   (r, _) <- rr
   return $ serializeRepo r "http://test.net/repo"


main = do
  let opts = AppOptions {
    repositoryPathOpt = "../test/actual/",
    repositoryBaseUrlOpt = "http://localhost:9999",
    currentSessionOpt = "9df4b597-af9e-4dca-bdda-719cce2c4e28"
  }
  existingRepo <- readRepoFromFS opts (SessionId $ currentSessionOpt opts)
  -- print $ "existingRepo = " ++ show existingRepo

  let rr = do
       u <- parseURI "rsync://test.net/repo/xxxx.cer"
       let query = Message (Version 3) [PublishQ u (Base64 "AAAAAA==")]
       return $ liftM (\r -> updateRepo (currentSession r) query) existingRepo

  print $ "serialized repo1: " ++ show (liftM serialize rr)
