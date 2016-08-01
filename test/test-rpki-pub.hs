{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception          (bracket)

import Network.URI
import Test.HUnit

import Data.Maybe

import Data.Acid.Memory

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Util as U

import Types
import Repo.State
import Store

sessionId :: SessionId
sessionId = SessionId "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

-- unsafe variant of uri construction to avoid carrying Maybe around
unsafeMkUri :: String -> URI
unsafeMkUri s = head $ catMaybes [parseURI s]

unsafeBase64 :: String -> Base64
unsafeBase64 s = head [ b | Right b <- [U.mkBase64 $ LBS.pack s] ]

--
-- testPublishToEmpty :: Test
-- testPublishToEmpty = TestCase $ do
--   x = processMessage appState clientId xml
--   assertEqual "New repo sessionId is wrong" sessionId
--   where
--     xml = ""
--     _uri    = unsafeMkUri "rsync://test.net/repo/aa.cer"
--     _base64 = unsafeBase64 "AAAAAA=="
--     Base64 _ _hash = _base64

testPublishToEmpty :: Test
testPublishToEmpty = TestCase $ do
    acid <- openMemoryState initialStore
    let x = processMessage mkAppState (ClientId "default") mkXml
    assertEqual "New repo sessionId is wrong" "" ""
    where
      mkAppState =
      mkXml = ""

main :: IO Counts
main = runTestTT $ TestList [
      --  testPublishToEmpty
    ]
