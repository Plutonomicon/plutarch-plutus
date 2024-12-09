module Main (main) where

import Plutarch.LedgerApi.Utils (PMaybeData, pmapMaybeData, pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.Maybe (pmapMaybe)
import Plutarch.Prelude
import Plutarch.Test.Bench (BenchConfig (Optimizing), bcompare, bench, benchWithConfig, defaultMain)
import Test.Tasty (testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      [ testGroup
          "Maybe"
          [ testGroup
              "pmaybeToMaybeData . pmaybeDataToMaybe"
              [ bench
                  "non-optimized"
                  (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
              , bcompare
                  "$(NF-1) == \"pmaybeToMaybeData . pmaybeDataToMaybe\" && $NF == \"non-optimized\""
                  $ benchWithConfig
                    "optimized"
                    Optimizing
                    (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
              ]
          , testGroup
              "fmap even"
              [ bench
                  "PMaybeData"
                  (pmapMaybeData # plam (\v -> pdata (peven # pfromData v)) # pconstant @(PMaybeData PInteger) (Just 42))
              , bcompare "$(NF-1) == \"fmap even\" && $NF == \"PMaybeData\"" $
                  bench "PMaybe vs PMaybeData" (pmapMaybe # peven # pconstant @(PMaybe PInteger) (Just 42))
              ]
          , -- We run both cheap and expensive calculation in 'pmap*' to mitigate impact of PAsData encoding/decoding
            let
              n :: Integer = 10
             in
              testGroup
                "fmap fib"
                [ bench
                    "PMaybeData"
                    (pmapMaybeData # plam (\v -> pdata (pfib # pfromData v)) # pconstant @(PMaybeData PInteger) (Just n))
                , bcompare "$(NF-1) == \"fmap fib\" && $NF == \"PMaybeData\"" $
                    bench "PMaybe vs PMaybeData" (pmapMaybe # pfib # pconstant @(PMaybe PInteger) (Just n))
                ]
          ]
      ]

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

pfib :: Term s (PInteger :--> PInteger)
pfib = pfix #$ plam $ \self n -> pif (n #<= 1) (pconstant 1) ((self # (n - 1)) * (self # (n - 2)))
