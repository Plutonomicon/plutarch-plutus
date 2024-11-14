module Main (main) where

import Plutarch.LedgerApi.Utils (PMaybeData, pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.Prelude
import Plutarch.Test.Bench (BenchConfig (Optimizing), bcompare, bench, benchWithConfig, defaultMain)
import Test.Tasty (testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      [ testGroup
          "pmaybeToMaybeData . pmaybeDataToMaybe"
          [ bench
              "non-optimized"
              (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
          , bcompare
              "$2 == \"pmaybeToMaybeData . pmaybeDataToMaybe\" && $3 == \"non-optimized\""
              $ benchWithConfig
                "optimized"
                Optimizing
                (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
          ]
      ]
