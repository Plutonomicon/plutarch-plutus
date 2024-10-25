module Plutarch.Test.Suite.Plutarch.Show (tests) where

import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Plutarch.List (pconvertLists)
import Plutarch.Prelude
import Plutarch.Show (pshowAndErr)
import Plutarch.Test.Golden (goldenAssertEqual, goldenAssertFail, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Show"
    [ plutarchGolden
        "Goldens"
        "show"
        [ goldenAssertEqual "unit" (pshow (pcon PUnit)) (pconstant @PString "()")
        , goldenGroup
            "bool"
            [ goldenAssertEqual "true" (pshow (pcon PTrue)) (pconstant @PString "PTrue")
            , goldenAssertEqual "false" (pshow (pcon PFalse)) (pconstant @PString "PFalse")
            ]
        , goldenGroup
            "int"
            ( map
                ( \n ->
                    goldenAssertEqual (fromString $ show n) (pshow (pconstant @PInteger n)) (pconstant (Text.pack $ show n))
                )
                [0, 5, -5, 10, -10, 14, -14, 102, -102]
            )
        , goldenGroup
            "bytestring"
            [ goldenAssertEqual "empty" (pshow (phexByteStr "")) (pconstant @PString "0x")
            , goldenAssertEqual "1" (pshow (phexByteStr "14")) (pconstant @PString "0x14")
            , goldenAssertEqual "2" (pshow (phexByteStr "14AF")) (pconstant @PString "0x14af")
            , goldenAssertEqual "3" (pshow (phexByteStr "14AF03")) (pconstant @PString "0x14af03")
            , goldenAssertEqual "n" (pshow (phexByteStr "FFFFFF")) (pconstant @PString "0xffffff")
            , goldenAssertEqual "0" (pshow (phexByteStr "000000")) (pconstant @PString "0x000000")
            ]
        , goldenGroup
            "str"
            [ goldenAssertEqual "empty" (pshow (pconstant @PString "")) (pconstant @PString "\"\"")
            , goldenAssertEqual "hello123" (pshow (pconstant @PString "hello123")) (pconstant @PString "\"hello123\"")
            , goldenAssertEqual "quoted" (pshow (pconstant @PString "hello\"123")) (pconstant @PString "\"hello\\\"123\"")
            , goldenAssertEqual "slash" (pshow (pconstant @PString "foo\\bar")) (pconstant @PString "\"foo\\bar\"")
            , goldenAssertEqual "unicode" (pshow (pconstant @PString "vis-à-vis")) (pconstant @PString "\"vis-à-vis\"")
            , goldenAssertEqual "unicode-quoted" (pshow (pconstant @PString "vis-\"à\"-vis")) (pconstant @PString "\"vis-\\\"à\\\"-vis\"")
            ]
        , goldenGroup
            "maybe"
            [ goldenAssertEqual "nothing" (pshow @(PMaybe PInteger) (pcon PNothing)) (pconstant @PString "PNothing")
            , goldenAssertEqual "just" (pshow @(PMaybe PInteger) (pcon $ PJust $ pconstant @PInteger 42)) (pconstant @PString "PJust 42")
            ]
        , goldenGroup
            "either"
            [ goldenAssertEqual "right" (pshow (pcon @(PEither PUnit PInteger) $ PRight 42)) (pconstant @PString "PRight 42")
            ]
        , goldenAssertEqual "maybe.either" (pshow (pcon $ PJust $ pcon @(PEither PInteger PUnit) $ PLeft 42)) (pconstant @PString "PJust (PLeft 42)")
        , goldenGroup
            "list"
            [ goldenAssertEqual "nil" (pshow (integerList [])) (pconstant @PString "[]")
            , goldenAssertEqual "1" (pshow (integerList [1])) (pconstant @PString "[1]")
            , goldenAssertEqual "1,2,3" (pshow (integerList [1, 2, 3])) (pconstant @PString "[1, 2, 3]")
            ]
        , goldenGroup
            "builtinlist"
            [ goldenAssertEqual "nil" (pshow (pconstant @(PBuiltinList PInteger) [])) (pconstant @PString "[]")
            , goldenAssertEqual "1,2,3" (pshow (pconstant @(PBuiltinList PInteger) [1, 2, 3])) (pconstant @PString "[1, 2, 3]")
            ]
        , goldenGroup
            "pshowAndErr"
            [ goldenAssertFail "nil" (pshowAndErr (pconstant @(PBuiltinList PInteger) []))
            , goldenAssertFail "1,2,3" (pshowAndErr (pconstant @(PBuiltinList PInteger) [1, 2, 3]))
            ]
        , goldenGroup
            "pair"
            [ goldenAssertEqual "int-str" (pshow (pcon @(PPair PInteger PString) $ PPair 42 "hello")) (pconstant @PString "PPair 42 \"hello\"")
            , goldenAssertEqual "int-list" (pshow (pcon @(PPair PInteger (PBuiltinList PInteger)) $ PPair 42 $ pconstant [1, 2, 3])) (pconstant @PString "PPair 42 [1, 2, 3]")
            ]
        , goldenGroup
            "rational"
            [ goldenAssertEqual "1/2" (pshow ((1 :: Term s PRational) / 2)) (pconstant @PString "1/2")
            ]
        ]
    ]

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
