module Plutarch.Test.Suite.Plutarch.Maybe (tests) where

import Plutarch (pcon)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq ((#==)))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Test.Golden (goldenAssertEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Maybe"
    [ plutarchGolden
        "Goldens"
        "maybe"
        [ goldenGroup
            "eq"
            [ goldenGroup
                "true"
                [ goldenAssertEqual "nothing" (pcon @(PMaybe PInteger) PNothing #== pcon PNothing) (pcon PTrue)
                , goldenAssertEqual "just" (pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42)) (pcon PTrue)
                ]
            , goldenGroup
                "false"
                [ goldenAssertEqual "nothing-just" (pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42)) (pcon PFalse)
                , goldenAssertEqual "just-just" (pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42)) (pcon PFalse)
                ]
            ]
        ]
    ]
