module Main where

import Network.URI
import Test.HUnit
import qualified Data.Map as M

import Types
import RRDP.Repo

sessionId :: SessionId
sessionId = SessionId "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

-- unsafe variant of uri construction to avoid carrying Maybe around
mkUri :: String -> URI
mkUri s = head [ u | Just u <- [parseURI s] ]

emptyRepo :: Repository
emptyRepo = Repository ( Snapshot (SnapshotDef (Version 1) sessionId (Serial 1)) []) M.empty

publishQ :: URI -> Base64 -> Message QueryPdu
publishQ u b = Message (Version 3) [PublishQ u b]

deltaDef :: SessionId -> Int -> DeltaDef
deltaDef sId n = DeltaDef (Version 1) sId (Serial n)

testPublishToEmpty = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 2) nSerial
  assertEqual "New repo publish set is wrong" [SnapshotPublish _uri _base64 _hash] publishes
  assertEqual "New repo deltas are wrong" (M.fromList [(2, delta)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [PublishR _uri]) response
  where
    (newRepo, response) = updateRepo emptyRepo $ publishQ _uri _base64

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = newRepo
    delta = Delta (deltaDef sessionId 2) [DeltaPublish _uri _base64 Nothing] []

    _uri    = mkUri "rsync://test.net/repo/aa.cer"
    _base64 = Base64 "AAAAAA=="
    _hash   = Hash "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119"


testPublishToUpdateExisting = TestCase $ do
  assertEqual "New repo serial is wrong" (Serial 3) nSerial
  assertEqual "New repo publish set is wrong" [SnapshotPublish _uri _2base64 newHash] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2), (3, delta3)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [PublishR _uri]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ publishQ _uri _1base64
    (replacedRepo, response) = updateRepo repo1 $ publishQ _uri _2base64

    Repository (Snapshot (SnapshotDef (Version 1) _ nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (deltaDef sessionId 2) [DeltaPublish _uri _1base64 Nothing] []
    delta3 = Delta (deltaDef sessionId 3) [DeltaPublish _uri _2base64 $ Just _hash] []

    _uri     = mkUri "rsync://test.net/repo/aa.cer"
    _1base64 = Base64 "AAAAAA=="
    _2base64 = Base64 "BBBBBB=="
    _hash    = Hash "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119"
    newHash  = Hash "0a4332736d932892386349cd30994ee48cc871cd913b0083ab79cd441c532cde"


testWithdrawExisting = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 3) nSerial
  assertEqual "New repo publish set is wrong" [] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2), (3, delta3)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [WithdrawR _uri]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ Message (Version 3) [PublishQ _uri _base64 ]
    (replacedRepo, response) = updateRepo repo1 $ Message (Version 3) [WithdrawQ _uri ]

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (DeltaDef (Version 1) sessionId (Serial 2)) [DeltaPublish _uri _base64 Nothing] []
    delta3 = Delta (DeltaDef (Version 1) sessionId (Serial 3)) [] [Withdraw _uri _hash]

    _uri     = mkUri "rsync://test.net/repo/aa.cer"
    _base64 = Base64 "AAAAAA=="
    _hash    = Hash "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119"


testWithdrawNotExisting = TestCase $ do
  assertEqual "New repo sessionId is wrong" sessionId nSessionId
  assertEqual "New repo serial is wrong" (Serial 2) nSerial
  assertEqual "New repo publish set is wrong" [SnapshotPublish _uri _base64 _hash] publishes
  assertEqual "New repo deltas are wrong"  (M.fromList [(2, delta2)]) _deltas
  assertEqual "Response is wrong" (Message (Version 3) [ReportError $ ObjectNotFound _wrongUri]) response
  where
    (repo1, _) = updateRepo emptyRepo           $ Message (Version 3) [PublishQ _uri _base64 ]
    (replacedRepo, response) = updateRepo repo1 $ Message (Version 3) [WithdrawQ _wrongUri ]

    Repository (Snapshot (SnapshotDef (Version 1) nSessionId nSerial) publishes) _deltas = replacedRepo

    delta2 = Delta (DeltaDef (Version 1) sessionId (Serial 2)) [DeltaPublish _uri _base64 Nothing] []

    _uri      = mkUri "rsync://test.net/repo/aa.cer"
    _wrongUri = mkUri "rsync://test.net/repo/bbb.cer"
    _base64   = Base64 "AAAAAA=="
    _hash     = Hash "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119"



main = runTestTT $ TestList [
       testPublishToEmpty,
       testPublishToUpdateExisting,
       testWithdrawExisting,
       testWithdrawNotExisting
    ]
