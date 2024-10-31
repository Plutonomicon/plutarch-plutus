module Plutarch.Test.Suite.Plutarch.Bool (tests) where

import Plutarch.Internal.Builtin (pand, por)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenEvalFail, goldenGroup, plutarchGolden)
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
            , goldenEvalEqual "app" (pnot # pcon PTrue) (pcon PFalse)
            ]
        , goldenGroup
            "pand"
            [ goldenEvalEqual "tf" (pcon PTrue #&& pcon PFalse) (pcon PFalse)
            , goldenEvalEqual "ft" (pcon PFalse #&& pcon PTrue) (pcon PFalse)
            , goldenEvalEqual "tt" (pcon PTrue #&& pcon PTrue) (pcon PTrue)
            , goldenEvalEqual "ff" (pcon PFalse #&& pcon PFalse) (pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenEvalEqual "pand" (pand # pcon PFalse # pdelay perror) (pdelay $ pcon PFalse)
                , goldenEvalEqual "op" (pcon PFalse #&& perror) (pcon PFalse)
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
            [ goldenEvalEqual "tf" (pcon PTrue #|| pcon PFalse) (pcon PTrue)
            , goldenEvalEqual "ft" (pcon PFalse #|| pcon PTrue) (pcon PTrue)
            , goldenEvalEqual "tt" (pcon PTrue #|| pcon PTrue) (pcon PTrue)
            , goldenEvalEqual "ff" (pcon PFalse #|| pcon PFalse) (pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenEvalEqual "por" (por # pcon PTrue # pdelay perror) (pdelay $ pcon PTrue)
                , goldenEvalEqual "op" (pcon PTrue #|| perror) (pcon PTrue)
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
