module Plutarch.Test.Suite.Plutarch.Pair (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEvalEqual, goldenGroup, plutarchGolden)
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
            [ goldenEvalEqual "true" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "Hello")) (pcon PTrue)
            , goldenGroup
                "false"
                [ goldenEvalEqual "fst" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "Hello")) (pcon PFalse)
                , goldenEvalEqual "snd" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "World")) (pcon PFalse)
                , goldenEvalEqual "both" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "World")) (pcon PFalse)
                ]
            ]
        ]
    ]
