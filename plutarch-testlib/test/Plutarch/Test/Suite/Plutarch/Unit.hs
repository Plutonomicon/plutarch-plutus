module Plutarch.Test.Suite.Plutarch.Unit (tests) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Unit"
    [ plutarchGolden
        "Goldens"
        "unit"
        [ goldenEval "pcon" (pcon PUnit)
        , goldenAssertEqual "pmatch" (pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)) (pcon PTrue)
        , goldenGroup
            "compare"
            [ goldenAssertEqual "==" (pcon PUnit #== pcon PUnit) (pcon PTrue)
            , goldenAssertEqual "<" (pcon PUnit #< pcon PUnit) (pcon PFalse)
            , goldenAssertEqual "<=" (pcon PUnit #<= pcon PUnit) (pcon PTrue)
            ]
        ]
    ]
