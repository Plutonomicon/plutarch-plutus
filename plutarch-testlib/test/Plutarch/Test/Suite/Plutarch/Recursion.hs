module Plutarch.Test.Suite.Plutarch.Recursion (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)
import Prelude hiding (succ)

tests :: TestTree
tests =
  testGroup
    "Recursion"
    [ plutarchGolden
        "Goldens"
        "recursion"
        [ goldenGroup
            "iterateN"
            [ goldenEval "lam" iterateN
            , goldenGroup
                "app"
                [ goldenEval "succ" (iterateN # 10 # succ # 0)
                , goldenEval "double" (iterateN # 10 # double # 1)
                ]
            ]
        ]
    ]

succ :: Term s (PInteger :--> PInteger)
succ = plam $ \x -> x + 1

double :: Term s (PInteger :--> PInteger)
double = plam $ \x -> x * 2

iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
iterateN = pfix # plam iterateN'
  where
    iterateN' ::
      Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
      Term s PInteger ->
      Term s (a :--> a) ->
      Term s a ->
      Term s a

    iterateN' self n f x =
      pif
        (n #== 0)
        x
        (self # (n - 1) # f #$ f # x)
