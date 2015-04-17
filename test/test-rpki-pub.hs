module Main where

import Network.URI
import Test.HUnit
import qualified Data.Map as M

import Types
import RRDP.Repo

sessionId = "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

emptyRepo :: Repository
emptyRepo = Repository ( Snapshot (SnapshotDef (Version 1) (SessionId sessionId) (Serial 1)) []) M.empty

test0 = TestCase $ assertBool "Failed" $
  case testUpdateRepo of
    Just (newRepo, response) -> True
    _ -> False
  where
    repo = emptyRepo
    testUpdateRepo = do
      u <- parseURI "rsync://test.net/repo/aa.cer"
      let query = Message (Version 3) [WithdrawQ u]
      return $ updateRepo repo query

main = runTestTT $ TestList [
       test0
    ]
