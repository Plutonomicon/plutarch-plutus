module Plutarch.Test.Suite.Plutarch.Recursion (tests) where

import Prelude hiding (succ)

import Plutarch
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger)

import Plutarch.Lift (pconstant)
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

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
                [ goldenEvalEqual
                    "succ"
                    (iterateN # 10 # succ # 0)
                    (pconstant @PInteger 10)
                , goldenEvalEqual
                    "double"
                    (iterateN # 10 # double # 1)
                    (pconstant @PInteger 1024)
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
