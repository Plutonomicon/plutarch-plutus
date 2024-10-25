module Plutarch.Test.Suite.Plutarch.Pair (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenGroup, plutarchGolden)
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
            [ goldenAssertEqual "true" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "Hello")) (pcon PTrue)
            , goldenGroup
                "false"
                [ goldenAssertEqual "fst" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "Hello")) (pcon PFalse)
                , goldenAssertEqual "snd" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 42 "World")) (pcon PFalse)
                , goldenAssertEqual "both" (pcon @(PPair PInteger PString) (PPair 42 "Hello") #== pcon (PPair 24 "World")) (pcon PFalse)
                ]
            ]
        ]
    ]
