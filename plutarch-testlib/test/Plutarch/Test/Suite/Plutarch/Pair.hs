module Plutarch.Test.Suite.Plutarch.Pair (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Pair"
    [ plutarchGolden
        "Goldens"
        "pair"
        [ goldenGroup
            "eq"
            [ goldenEval "true" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "Hello"))
            , goldenGroup
                "false"
                [ goldenEval "fst" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "Hello"))
                , goldenEval "snd" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "World"))
                , goldenEval "both" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "World"))
                ]
            ]
        ]
    ]
