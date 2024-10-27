module Plutarch.Test.Suite.Plutarch.Positive (tests) where

import Plutarch.Positive (PPositive)
import Plutarch.Test.Laws (checkLedgerPropertiesPCountable)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Positive"
    [ checkLedgerPropertiesPCountable @PPositive
    ]
