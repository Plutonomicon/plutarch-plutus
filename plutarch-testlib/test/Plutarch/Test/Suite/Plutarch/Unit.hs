module Plutarch.Test.Suite.Plutarch.Unit (tests) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Unit"
    [ plutarchGolden
        "Goldens"
        "unit"
        [ goldenEval "pcon" (pcon PUnit)
        , goldenEval "pmatch" (pmatch (pcon PUnit) (\case PUnit -> pcon PTrue))
        , goldenGroup
            "compare"
            [ goldenEval "==" (pcon PUnit #== pcon PUnit)
            , goldenEval "<" (pcon PUnit #< pcon PUnit)
            , goldenEval "<=" (pcon PUnit #<= pcon PUnit)
            ]
        ]
    ]
