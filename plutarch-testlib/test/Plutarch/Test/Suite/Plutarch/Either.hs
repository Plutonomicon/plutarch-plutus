module Plutarch.Test.Suite.Plutarch.Either (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenGroup, plutarchGolden)
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
                [ goldenAssertEqual "left" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PLeft 42)) (pcon PTrue)
                , goldenAssertEqual "right" (pcon @(PEither PInteger PInteger) (PRight 42) #== pcon (PRight 42)) (pcon PTrue)
                ]
            , goldenGroup
                "false"
                [ goldenAssertEqual "left-right" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PRight 42)) (pcon PFalse)
                , goldenAssertEqual "left-left" (pcon @(PEither PInteger PInteger) (PLeft 24) #== pcon (PLeft 42)) (pcon PFalse)
                , goldenAssertEqual "right-right" (pcon @(PEither PInteger PInteger) (PRight 24) #== pcon (PRight 42)) (pcon PFalse)
                ]
            ]
        ]
    ]
