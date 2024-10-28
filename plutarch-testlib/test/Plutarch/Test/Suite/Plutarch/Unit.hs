module Plutarch.Test.Suite.Plutarch.Unit (tests) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Unit"
    [ plutarchGolden
        "Goldens"
        "unit"
        [ goldenEval "pcon" (pcon PUnit)
        , goldenEvalEqual "pmatch" (pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)) (pcon PTrue)
        , goldenGroup
            "compare"
            [ goldenEvalEqual "==" (pcon PUnit #== pcon PUnit) (pcon PTrue)
            , goldenEvalEqual "<" (pcon PUnit #< pcon PUnit) (pcon PFalse)
            , goldenEvalEqual "<=" (pcon PUnit #<= pcon PUnit) (pcon PTrue)
            ]
        ]
    ]
