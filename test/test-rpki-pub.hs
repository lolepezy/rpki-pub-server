{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Main where

import           Control.Exception          (bracket)
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Network.URI
import Test.HUnit hiding (assert)

import Data.Maybe
import Data.List

import Data.Acid.Memory
import Data.String.Interpolate

import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as L

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>), listOf1, elements, vectorOf, suchThat)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, pre, run)

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

initialState :: IO AppState
initialState = do
  acid  <- openMemoryState initialStore
  tRepo <- atomically $ initialTRepo []
  return AppState {
    appConfig = defaultConfig,
    acidRepo = acid,
    currentState = tRepo,
    changeSync = \ch -> return ()
  }

generatePublishable :: PropertyM IO [(String, Base64)]
generatePublishable = do
  uris <- pick $ nub <$> vectorOf 100 (do
      host    <- listOf1 $ elements ['a'..'z']
      path    <- listOf1 $ elements ['a'..'z']
      return [i|rsync://#{host}/#{path}.cert|]
    )

  pick $ mapM (\u -> do
      content <- L.pack <$> suchThat arbitrary (not . null)
      let b64  = (unsafeBase64 . L.unpack . B64.encode) content
      return (u, b64)
    ) uris

exists :: Reply -> [(String, Base64)] -> Bool
exists (ListReply pdus) objects = null [
  "" | ListPdu u h <- pdus,
      null [ 1 | (u', b64 @ (Base64 _ h')) <- objects, unsafeMkUri u' == u && h == h' ]
  ]


prop_publishedShouldBeListed :: Property
prop_publishedShouldBeListed = monadicIO $ do
    let clientId = ClientId "default"

    appState @ AppState{..} <- run initialState
    objects <- generatePublishable

    mapM_ (\(uri, b64 @ (Base64 _ h)) -> do
        let xml = mkPublishXml uri b64
        (state0, reply0) <- run $ processMessage appState clientId xml (\a -> return ())
        assert $ reply0 == Success
      ) objects

    (_, replyAll) <- run $ listObjects appState clientId

    assert $ exists replyAll objects


prop_publishedShouldBeListedAsync :: Property
prop_publishedShouldBeListedAsync = monadicIO $ do
    let clientId = ClientId "default"

    appState @ AppState{..} <- run initialState
    objects <- generatePublishable

    asyncs <- mapM (\(uri, b64 @ (Base64 _ h)) -> do
        let xml = mkPublishXml uri b64
        run $ async $ processMessage appState clientId xml (\a -> return ())
      ) objects

    rs <- run $ mapM wait asyncs

    (_, replyAll) <- run $ listObjects appState clientId

    assert $ any (\(_, r) -> r == Success) rs
    assert $ exists replyAll objects


main :: IO ()
main = do
  quickCheck prop_publishedShouldBeListed
  quickCheck prop_publishedShouldBeListedAsync
