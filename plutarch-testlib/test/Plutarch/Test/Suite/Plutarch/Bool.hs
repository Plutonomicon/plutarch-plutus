module Plutarch.Test.Suite.Plutarch.Bool (tests) where

import Plutarch.Bool (pand, por)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenAssertFail, goldenEval, goldenGroup, plutarchGolden)
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
            , goldenAssertEqual "app" (pnot # pcon PTrue) (pcon PFalse)
            ]
        , goldenGroup
            "pand"
            [ goldenAssertEqual "tf" (pcon PTrue #&& pcon PFalse) (pcon PFalse)
            , goldenAssertEqual "ft" (pcon PFalse #&& pcon PTrue) (pcon PFalse)
            , goldenAssertEqual "tt" (pcon PTrue #&& pcon PTrue) (pcon PTrue)
            , goldenAssertEqual "ff" (pcon PFalse #&& pcon PFalse) (pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenAssertEqual "pand" (pand # pcon PFalse # pdelay perror) (pdelay $ pcon PFalse)
                , goldenAssertEqual "op" (pcon PFalse #&& perror) (pcon PFalse)
                , goldenGroup
                    "pand"
                    [ goldenGroup
                        "perror"
                        [ goldenAssertFail "op" (pcon PTrue #&& perror)
                        ]
                    ]
                ]
            ]
        , goldenGroup
            "por"
            [ goldenAssertEqual "tf" (pcon PTrue #|| pcon PFalse) (pcon PTrue)
            , goldenAssertEqual "ft" (pcon PFalse #|| pcon PTrue) (pcon PTrue)
            , goldenAssertEqual "tt" (pcon PTrue #|| pcon PTrue) (pcon PTrue)
            , goldenAssertEqual "ff" (pcon PFalse #|| pcon PFalse) (pcon PFalse)
            , goldenGroup
                "laziness"
                [ goldenAssertEqual "por" (por # pcon PTrue # pdelay perror) (pdelay $ pcon PTrue)
                , goldenAssertEqual "op" (pcon PTrue #|| perror) (pcon PTrue)
                , goldenGroup
                    "pand"
                    [ goldenGroup
                        "perror"
                        [ goldenGroup
                            "op"
                            [ goldenEval "true" (pcon PTrue #|| perror)
                            , goldenAssertFail "false" (pcon PFalse #|| perror)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
