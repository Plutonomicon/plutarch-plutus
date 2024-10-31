module Plutarch.Test.Suite.Plutarch.Either (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEvalEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Either"
    [ plutarchGolden
        "Goldens"
        "either"
        [ goldenGroup
            "eq"
            [ goldenGroup
                "true"
                [ goldenEvalEqual "left" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PLeft 42)) (pcon PTrue)
                , goldenEvalEqual "right" (pcon @(PEither PInteger PInteger) (PRight 42) #== pcon (PRight 42)) (pcon PTrue)
                ]
            , goldenGroup
                "false"
                [ goldenEvalEqual "left-right" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PRight 42)) (pcon PFalse)
                , goldenEvalEqual "left-left" (pcon @(PEither PInteger PInteger) (PLeft 24) #== pcon (PLeft 42)) (pcon PFalse)
                , goldenEvalEqual "right-right" (pcon @(PEither PInteger PInteger) (PRight 24) #== pcon (PRight 42)) (pcon PFalse)
                ]
            ]
        ]
    ]
