module Plutarch.Test.Suite.Plutarch.Show (tests) where

import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Plutarch.Internal.Show (pshowAndErr)
import Plutarch.List (pconvertLists)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEvalEqual, goldenEvalFail, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Show"
    [ plutarchGolden
        "Goldens"
        "show"
        [ goldenEvalEqual "unit" (pshow (pcon PUnit)) (pconstant @PString "()")
        , goldenGroup
            "bool"
            [ goldenEvalEqual "true" (pshow (pcon PTrue)) (pconstant @PString "PTrue")
            , goldenEvalEqual "false" (pshow (pcon PFalse)) (pconstant @PString "PFalse")
            ]
        , goldenGroup
            "int"
            ( map
                ( \n ->
                    goldenEvalEqual (fromString $ show n) (pshow (pconstant @PInteger n)) (pconstant (Text.pack $ show n))
                )
                [0, 5, -5, 10, -10, 14, -14, 102, -102]
            )
        , goldenGroup
            "bytestring"
            [ goldenEvalEqual "empty" (pshow (phexByteStr "")) (pconstant @PString "0x")
            , goldenEvalEqual "1" (pshow (phexByteStr "14")) (pconstant @PString "0x14")
            , goldenEvalEqual "2" (pshow (phexByteStr "14AF")) (pconstant @PString "0x14af")
            , goldenEvalEqual "3" (pshow (phexByteStr "14AF03")) (pconstant @PString "0x14af03")
            , goldenEvalEqual "n" (pshow (phexByteStr "FFFFFF")) (pconstant @PString "0xffffff")
            , goldenEvalEqual "0" (pshow (phexByteStr "000000")) (pconstant @PString "0x000000")
            ]
        , goldenGroup
            "str"
            [ goldenEvalEqual "empty" (pshow (pconstant @PString "")) (pconstant @PString "\"\"")
            , goldenEvalEqual "hello123" (pshow (pconstant @PString "hello123")) (pconstant @PString "\"hello123\"")
            , goldenEvalEqual "quoted" (pshow (pconstant @PString "hello\"123")) (pconstant @PString "\"hello\\\"123\"")
            , goldenEvalEqual "slash" (pshow (pconstant @PString "foo\\bar")) (pconstant @PString "\"foo\\bar\"")
            , goldenEvalEqual "unicode" (pshow (pconstant @PString "vis-à-vis")) (pconstant @PString "\"vis-à-vis\"")
            , goldenEvalEqual "unicode-quoted" (pshow (pconstant @PString "vis-\"à\"-vis")) (pconstant @PString "\"vis-\\\"à\\\"-vis\"")
            ]
        , goldenGroup
            "maybe"
            [ goldenEvalEqual "nothing" (pshow @(PMaybe PInteger) (pcon PNothing)) (pconstant @PString "PNothing")
            , goldenEvalEqual "just" (pshow @(PMaybe PInteger) (pcon $ PJust $ pconstant @PInteger 42)) (pconstant @PString "PJust 42")
            ]
        , goldenGroup
            "either"
            [ goldenEvalEqual "right" (pshow (pcon @(PEither PUnit PInteger) $ PRight 42)) (pconstant @PString "PRight 42")
            ]
        , goldenEvalEqual "maybe.either" (pshow (pcon $ PJust $ pcon @(PEither PInteger PUnit) $ PLeft 42)) (pconstant @PString "PJust (PLeft 42)")
        , goldenGroup
            "list"
            [ goldenEvalEqual "nil" (pshow (integerList [])) (pconstant @PString "[]")
            , goldenEvalEqual "1" (pshow (integerList [1])) (pconstant @PString "[1]")
            , goldenEvalEqual "1,2,3" (pshow (integerList [1, 2, 3])) (pconstant @PString "[1, 2, 3]")
            ]
        , goldenGroup
            "builtinlist"
            [ goldenEvalEqual "nil" (pshow (pconstant @(PBuiltinList PInteger) [])) (pconstant @PString "[]")
            , goldenEvalEqual "1,2,3" (pshow (pconstant @(PBuiltinList PInteger) [1, 2, 3])) (pconstant @PString "[1, 2, 3]")
            ]
        , goldenGroup
            "pshowAndErr"
            [ goldenEvalFail "nil" (pshowAndErr (pconstant @(PBuiltinList PInteger) []))
            , goldenEvalFail "1,2,3" (pshowAndErr (pconstant @(PBuiltinList PInteger) [1, 2, 3]))
            ]
        , goldenGroup
            "pair"
            [ goldenEvalEqual "int-str" (pshow (pcon @(PPair PInteger PString) $ PPair 42 "hello")) (pconstant @PString "PPair 42 \"hello\"")
            , goldenEvalEqual "int-list" (pshow (pcon @(PPair PInteger (PBuiltinList PInteger)) $ PPair 42 $ pconstant [1, 2, 3])) (pconstant @PString "PPair 42 [1, 2, 3]")
            ]
        , goldenGroup
            "rational"
            [ goldenEvalEqual "1/2" (pshow ((1 :: Term s PRational) / 2)) (pconstant @PString "1/2")
            ]
        ]
    ]

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
