{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import           Control.Exception          (bracket)

import Network.URI
import Test.HUnit

import Data.Maybe

import Data.Acid.Memory
import Data.String.Interpolate

import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Util as U

import Types
import Repo.State
import Store
import Config

sessionId :: SessionId
sessionId = SessionId "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

-- unsafe variant of uri construction to avoid carrying Maybe around
unsafeMkUri :: String -> URI
unsafeMkUri s = head $ catMaybes [parseURI s]

unsafeBase64 :: String -> Base64
unsafeBase64 s = head [ b | Right b <- [U.mkBase64 $ L.pack s] ]


mkPublishXml :: String -> Base64 -> L.ByteString
mkPublishXml uri b64 = L.pack [i|
  <msg type="query" version="4" xmlns="http://www.hactrn.net/uris/rpki/publication-spec/">
    <publish uri="#{uri}" tag="tag">#{s}</publish>
  </msg>
  |]
  where s = U.base64bs b64 :: L.ByteString

initialState = do
  acid  <- openMemoryState initialStore
  tRepo <- atomically $ initialTRepo []
  return AppState {
    appConfig = defaultConfig,
    acidRepo = acid,
    currentState = tRepo,
    changeSync = \ch -> return ()
  }

testPublishToEmpty :: Test
testPublishToEmpty = TestCase $ do
    appState @ AppState{..} <- initialState
    let xml = mkPublishXml "rsync://test.url/aaa.cert" ((unsafeBase64 . L.unpack . B64.encode) "*&^random stuff@#$")
    (state, reply) <- processMessage appState (ClientId "default") xml (\a -> return ())

    assertEqual "The result was wrong" reply Success


main :: IO Counts
main = runTestTT $ TestList [
       testPublishToEmpty
    ]
