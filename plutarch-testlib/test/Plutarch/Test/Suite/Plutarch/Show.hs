module Plutarch.Test.Suite.Plutarch.Show (tests) where

import Data.String (IsString (fromString))
import Plutarch.List (pconvertLists)
import Plutarch.Prelude
import Plutarch.Show (pshowAndErr)
import Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Show"
    [ plutarchGolden
        "Goldens"
        "show"
        [ goldenEval "unit" (pshow (pcon PUnit))
        , goldenGroup
            "bool"
            [ goldenEval "true" (pshow (pcon PTrue))
            , goldenEval "false" (pshow (pcon PFalse))
            ]
        , goldenGroup
            "int"
            ( map
                ( \n ->
                    goldenEval (fromString $ show n) (pshow (pconstant @PInteger n))
                )
                [0, 5, -5, 10, -10, 14, -14, 102, -102]
            )
        , goldenGroup
            "bytestring"
            [ goldenEval "empty" (pshow (phexByteStr ""))
            , goldenEval "1" (pshow (phexByteStr "14"))
            , goldenEval "2" (pshow (phexByteStr "14AF"))
            , goldenEval "3" (pshow (phexByteStr "14AF03"))
            , goldenEval "n" (pshow (phexByteStr "FFFFFF"))
            , goldenEval "0" (pshow (phexByteStr "000000"))
            ]
        , goldenGroup
            "str"
            [ goldenEval "empty" (pshow (pconstant @PString ""))
            , goldenEval "hello123" (pshow (pconstant @PString "hello123"))
            , goldenEval "quoted" (pshow (pconstant @PString "hello\"123"))
            , goldenEval "slash" (pshow (pconstant @PString "foo\\bar"))
            , goldenEval "unicode" (pshow (pconstant @PString "vis-à-vis"))
            , goldenEval "unicode-quoted" (pshow (pconstant @PString "vis-\"à\"-vis"))
            ]
        , goldenGroup
            "maybe"
            [ goldenEval "nothing" (pshow @(PMaybe PInteger) (pcon PNothing))
            , goldenEval "just" (pshow @(PMaybe PInteger) (pcon $ PJust $ pconstant @PInteger 42))
            ]
        , goldenGroup
            "either"
            [ goldenEval "right" (pshow (pcon @(PEither PUnit PInteger) $ PRight 42))
            ]
        , goldenEval "maybe.either" (pshow (pcon $ PJust $ pcon @(PEither PInteger PUnit) $ PLeft 42))
        , goldenGroup
            "list"
            [ goldenEval "nil" (pshow (integerList []))
            , goldenEval "1" (pshow (integerList [1]))
            , goldenEval "1,2,3" (pshow (integerList [1, 2, 3]))
            ]
        , goldenGroup
            "builtinlist"
            [ goldenEval "nil" (pshow (pconstant @(PBuiltinList PInteger) []))
            , goldenEval "1,2,3" (pshow (pconstant @(PBuiltinList PInteger) [1, 2, 3]))
            ]
        , goldenGroup
            "pshowAndErr"
            [ goldenEvalFail "nil" (pshowAndErr (pconstant @(PBuiltinList PInteger) []))
            , goldenEvalFail "1,2,3" (pshowAndErr (pconstant @(PBuiltinList PInteger) [1, 2, 3]))
            ]
        , goldenGroup
            "pair"
            [ goldenEval "int-str" (pshow (pcon @(PPair PInteger PString) $ PPair 42 "hello"))
            , goldenEval "int-list" (pshow (pcon @(PPair PInteger (PBuiltinList PInteger)) $ PPair 42 $ pconstant [1, 2, 3]))
            ]
        , goldenGroup
            "rational"
            [ goldenEval "1/2" (pshow ((1 :: Term s PRational) / 2))
            ]
        ]
    ]

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
