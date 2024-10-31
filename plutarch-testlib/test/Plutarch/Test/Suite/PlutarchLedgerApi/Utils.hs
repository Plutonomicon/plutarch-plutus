module Plutarch.Test.Suite.PlutarchLedgerApi.Utils (tests) where

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1 (PPosixTime)
import Plutarch.Test.Laws (checkLedgerProperties)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Utils"
    [ checkLedgerProperties @(PMaybeData PPosixTime)
    ]
