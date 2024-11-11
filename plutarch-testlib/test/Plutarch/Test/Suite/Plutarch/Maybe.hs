module Plutarch.Test.Suite.Plutarch.Maybe (tests) where

import Plutarch (pcon)
import Plutarch.Bool (PEq ((#==)))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
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
                [ goldenEval "nothing" (pcon @(PMaybe PInteger) PNothing #== pcon PNothing)
                , goldenEval "just" (pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42))
                ]
            , goldenGroup
                "false"
                [ goldenEval "nothing-just" (pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42))
                , goldenEval "just-just" (pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42))
                ]
            ]
        ]
    ]
