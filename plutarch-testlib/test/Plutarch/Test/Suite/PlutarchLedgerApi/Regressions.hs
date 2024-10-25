module Plutarch.Test.Suite.PlutarchLedgerApi.Regressions (tests) where

import Plutarch.LedgerApi.Interval (PInterval, pcontains)
import Plutarch.LedgerApi.V1 (PPosixTime)
import Plutarch.Prelude (pconstant, plift, (#))
import PlutusLedgerApi.V1.Interval (
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Time (POSIXTime)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: [TestTree]
tests =
  [ testCase "Issue 705 (pcontains)" $ do
      let i1 = Interval (LowerBound (Finite (120 :: POSIXTime)) True) . UpperBound (Finite 129) $ False
      let i2 = Interval (LowerBound (Finite 120) True) . UpperBound (Finite 129) $ True
      let expected = Interval.contains i2 i1
      assertEqual "" expected (plift $ pcontains # pconstant @(PInterval PPosixTime) i2 # pconstant i1)
  ]
