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
        , goldenGroup
            "pcond"
            [ goldenEval "direct-1" (pconstant @PInteger 1)
            , goldenEval "pcond-1" (pcond [] (pconstant @PInteger 1))
            , goldenEval "direct-2" (pif (pconstant @PInteger 1 #< 2) (pconstant @PInteger 1) (pconstant 2))
            , goldenEval "pcond-2" (pcond [(pconstant @PInteger 1 #< 2, pconstant @PInteger 1)] 2)
            , goldenEval "direct-3" (pif (pconstant @PInteger 1 #< 2) (pconstant @PInteger 1) (pif (pconstant @PInteger 2 #< 3) 2 3))
            , goldenEval "pcond-3" (pcond [(pconstant @PInteger 1 #< 2, pconstant @PInteger 1), (pconstant @PInteger 2 #< 3, 2)] 3)
            ]
        ]
    ]
