module Plutarch.Test.Suite.Plutarch.Either (tests) where

import Plutarch.Either (PEitherData)
import Plutarch.LedgerApi.V1 (PPosixTime)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkLedgerProperties)
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
                [ goldenEval "left" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PLeft 42))
                , goldenEval "right" (pcon @(PEither PInteger PInteger) (PRight 42) #== pcon (PRight 42))
                ]
            , goldenGroup
                "false"
                [ goldenEval "left-right" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PRight 42))
                , goldenEval "left-left" (pcon @(PEither PInteger PInteger) (PLeft 24) #== pcon (PLeft 42))
                , goldenEval "right-right" (pcon @(PEither PInteger PInteger) (PRight 24) #== pcon (PRight 42))
                ]
            ]
        ]
    , testGroup
        "PEitherData"
        [ checkLedgerProperties @(PEitherData PPosixTime PPosixTime)
        ]
    ]
