module Plutarch.Test.Suite.Plutarch.Maybe (tests) where

import Plutarch.Internal.Builtin (PBool (PFalse, PTrue), PInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.PlutusType (pcon)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Test.Golden (goldenEvalEqual, goldenGroup, plutarchGolden)
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
                [ goldenEvalEqual "nothing" (pcon @(PMaybe PInteger) PNothing #== pcon PNothing) (pcon PTrue)
                , goldenEvalEqual "just" (pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42)) (pcon PTrue)
                ]
            , goldenGroup
                "false"
                [ goldenEvalEqual "nothing-just" (pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42)) (pcon PFalse)
                , goldenEvalEqual "just-just" (pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42)) (pcon PFalse)
                ]
            ]
        ]
    ]
