module Plutarch.Test.Suite.Plutarch.Bool (tests) where

import Plutarch.Builtin.Bool (pand, por)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Bool"
    [ plutarchGolden
        "Goldens"
        "bool"
        [ goldenGroup
            "pnot"
            [ goldenEval "lam" pnot
            , goldenEval "app" (pnot # pcon PTrue)
            ]
        , goldenGroup
            "pand"
            [ goldenEval "tf" (pcon PTrue #&& pcon PFalse)
            , goldenEval "ft" (pcon PFalse #&& pcon PTrue)
            , goldenEval "tt" (pcon PTrue #&& pcon PTrue)
            , goldenEval "ff" (pcon PFalse #&& pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenEval "pand" (pand # pcon PFalse # pdelay perror)
                , goldenEval "op" (pcon PFalse #&& perror)
                , goldenGroup
                    "pand"
                    [ goldenGroup
                        "perror"
                        [ goldenEvalFail "op" (pcon PTrue #&& perror)
                        ]
                    ]
                ]
            ]
        , goldenGroup
            "por"
            [ goldenEval "tf" (pcon PTrue #|| pcon PFalse)
            , goldenEval "ft" (pcon PFalse #|| pcon PTrue)
            , goldenEval "tt" (pcon PTrue #|| pcon PTrue)
            , goldenEval "ff" (pcon PFalse #|| pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenEval "por" (por # pcon PTrue # pdelay perror)
                , goldenEval "op" (pcon PTrue #|| perror)
                , goldenGroup
                    "pand"
                    [ goldenGroup
                        "perror"
                        [ goldenGroup
                            "op"
                            [ goldenEval "true" (pcon PTrue #|| perror)
                            , goldenEvalFail "false" (pcon PFalse #|| perror)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
