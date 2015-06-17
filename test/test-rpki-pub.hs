{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.URI
import Test.HUnit
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Util as U

import Types
import RRDP.Repo

sessionId :: SessionId
sessionId = SessionId "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

-- unsafe variant of uri construction to avoid carrying Maybe around
unsafeMkUri :: String -> URI
unsafeMkUri s = head [ u | Just u <- [parseURI s] ]

unsafeBase64 :: String -> Base64
unsafeBase64 s = head [ b | Right b <- [U.mkBase64 $ LBS.pack s] ]


emptyRepo :: Repository
emptyRepo = Repository ( Snapshot (SnapshotDef (Version 1) sessionId (Serial 1)) []) M.empty

publishQ :: URI -> Base64 -> Message QueryPdu
publishQ u b = Message (Version 3) [PublishQ u b Nothing]

hashedPublishQ :: URI -> Base64 -> Hash -> Message QueryPdu
hashedPublishQ u b h = Message (Version 3) [PublishQ u b $ Just h]


deltaDef :: SessionId -> Int -> DeltaDef
deltaDef sId n = DeltaDef (Version 1) sId (Serial n)


testPublishToEmpty :: Test
testPublishToEmpty = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 2) nSerial
  assertEqual "New repo publish set is wrong" [Publish _uri _base64 Nothing] publishes
  assertEqual "New repo deltas are wrong" (M.fromList [(2, delta)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [PublishR _uri]) response
  where
    (newRepo, response) = updateRepo emptyRepo $ publishQ _uri _base64

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = newRepo
    delta = Delta (deltaDef sessionId 2) [Publish _uri _base64 Nothing] []

    _uri    = unsafeMkUri "rsync://test.net/repo/aa.cer"
    _base64 = unsafeBase64 "AAAAAA=="
    Base64 _ _hash = _base64


testPublishToUpdateExisting :: Test
testPublishToUpdateExisting = TestCase $ do
  assertEqual "New repo serial is wrong" (Serial 3) nSerial
  assertEqual "New repo publish set is wrong" [Publish _uri _2base64 $ Just _hash] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2), (3, delta3)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [PublishR _uri]) response

  where
    (repo1, _) = updateRepo emptyRepo $ publishQ _uri _1base64
    (replacedRepo, response) = updateRepo repo1 $ hashedPublishQ _uri _2base64 _hash

    Repository (Snapshot (SnapshotDef (Version 1) _ nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (deltaDef sessionId 2) [Publish _uri _1base64 Nothing] []
    delta3 = Delta (deltaDef sessionId 3) [Publish _uri _2base64 $ Just _hash] []

    _uri     = unsafeMkUri "rsync://test.net/repo/aa.cer"
    _1base64 = unsafeBase64 "AAAAAA=="
    _2base64 = unsafeBase64 "BBBBBB=="
    Base64 _ _hash = _1base64


testWithdrawExisting :: Test
testWithdrawExisting = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 3) nSerial
  assertEqual "New repo publish set is wrong" [] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2), (3, delta3)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [WithdrawR _uri]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ Message (Version 3) [PublishQ _uri _base64 Nothing]
    (replacedRepo, response) = updateRepo repo1 $ Message (Version 3) [WithdrawQ _uri _hash]

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (DeltaDef (Version 1) sessionId (Serial 2)) [Publish _uri _base64 Nothing] []
    delta3 = Delta (DeltaDef (Version 1) sessionId (Serial 3)) [] [Withdraw _uri _hash]

    _uri     = unsafeMkUri "rsync://test.net/repo/aa.cer"
    _base64 = unsafeBase64 "AAAAAA=="
    Base64 _ _hash = _base64
    _hash1   = Hash "whatever"


testWithdrawNotExisting :: Test
testWithdrawNotExisting = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 2) nSerial
  assertEqual "New repo publish set is wrong" [Publish _uri _base64 Nothing] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [ReportError $ ObjectNotFound _wrongUri]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ Message (Version 3) [PublishQ _uri _base64 Nothing]
    (replacedRepo, response) = updateRepo repo1 $ Message (Version 3) [WithdrawQ _wrongUri _hash1]

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (DeltaDef (Version 1) sessionId (Serial 2)) [Publish _uri _base64 Nothing] []

    _uri      = unsafeMkUri "rsync://test.net/repo/aa.cer"
    _wrongUri = unsafeMkUri "rsync://test.net/repo/bbb.cer"
    _base64   = unsafeBase64 "AAAAAA=="
    Base64 _ _hash = _base64
    _hash1    = Hash "whatever"


testWithdrawExistingWithWrongHash :: Test
testWithdrawExistingWithWrongHash = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 2) nSerial
  assertEqual "New repo publish set is wrong" [Publish _uri _base64 Nothing] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [ReportError (BadHash {
           passed = Hash "whatever",
           stored = Hash "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119",
           uriW = unsafeMkUri "rsync://test.net/repo/aa.cer"
         })]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ Message (Version 3) [PublishQ _uri _base64 Nothing]
    (replacedRepo, response) = updateRepo repo1 $ Message (Version 3) [WithdrawQ _uri _hash1]

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (DeltaDef (Version 1) sessionId (Serial 2)) [Publish _uri _base64 Nothing] []

    _uri     = unsafeMkUri "rsync://test.net/repo/aa.cer"
    _base64 = unsafeBase64 "AAAAAA=="
    Base64 _ _hash = _base64
    _hash1   = Hash "whatever"


main :: IO Counts
main = runTestTT $ TestList [
       testPublishToEmpty,
       testPublishToUpdateExisting,
       testWithdrawExisting,
       testWithdrawNotExisting,
       testWithdrawExistingWithWrongHash
    ]
