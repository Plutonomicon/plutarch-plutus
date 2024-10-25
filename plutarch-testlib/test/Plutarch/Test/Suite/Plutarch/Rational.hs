module Plutarch.Test.Suite.Plutarch.Rational (tests) where

import Plutarch.Prelude
import Plutarch.Rational (pproperFraction, ptruncate)
import Plutarch.Test.Golden (goldenAssertEqual, goldenAssertFail, goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Rational"
    [ plutarchGolden
        "Goldens"
        "rational"
        [ goldenEval "literal" (0.5 :: Term s PRational)
        , goldenGroup
            "ops"
            [ goldenAssertEqual "+" (rat (1 / 2 + 1 / 2)) (1 :: Term s PRational)
            , goldenAssertEqual "-" (1 / 2 - 1 / 3) (1 / 6 :: Term s PRational)
            , goldenAssertEqual "*" ((1 - 3 / 2) * (2 - 5 / 2)) (1 / 4 :: Term s PRational)
            , goldenAssertEqual "harmonic-sum" (1 / 2 + 1 / 3 + 1 / 4 + 1 / 5) (77 / 60 :: Term s PRational)
            , goldenAssertEqual "multi-product" (1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6) (1 / 6 :: Term s PRational)
            ]
        , goldenAssertEqual "compare" (rat (2 / 9) #< (3 / 10)) (pcon PTrue)
        , goldenGroup
            "round"
            [ goldenAssertEqual "5/3" (pround # (5 / 3 :: Term s PRational)) (pconstant @PInteger 2)
            , goldenAssertEqual "4/3" (pround # (4 / 3 :: Term s PRational)) (pconstant @PInteger 1)
            , goldenAssertEqual "-5/2" (pround # (-5 / 2 :: Term s PRational)) (pconstant @PInteger (-2))
            , goldenAssertEqual "-1/4" (pround # (-1 / 4 :: Term s PRational)) (pconstant @PInteger 0)
            ]
        , goldenGroup
            "truncate"
            [ goldenAssertEqual "5/4" (ptruncate # (5 / 4 :: Term s PRational)) (pconstant @PInteger 1)
            , goldenAssertEqual "7/4" (ptruncate # (7 / 4 :: Term s PRational)) (pconstant @PInteger 1)
            , goldenAssertEqual "1/4" (ptruncate # (1 / 4 :: Term s PRational)) (pconstant @PInteger 0)
            , goldenAssertEqual "-7/4" (ptruncate # (-7 / 4 :: Term s PRational)) (pconstant @PInteger (-1))
            ]
        , goldenGroup
            "properFraction"
            [ goldenAssertEqual "-1/2" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP (-1 / 2) 0 (-1 / 2)) (pcon PTrue)
            , goldenAssertEqual "-3/2" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP (-3 / 2) (-1) (-1 / 2)) (pcon PTrue)
            , goldenAssertEqual "-4/3" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP (-4 / 3) (-1) (-1 / 3)) (pcon PTrue)
            ]
        , goldenGroup
            "data.id"
            [ goldenAssertEqual "0.5" (0.5 :: Term s PRational) (pfromData (pdata 0.5))
            , goldenAssertEqual "2" (2 :: Term s PRational) (pfromData (pdata 2))
            , goldenAssertEqual "11/3" (11 / 3 :: Term s PRational) (pfromData (pdata $ 11 / 3))
            ]
        , goldenGroup
            "div by 0"
            [ goldenAssertFail "1/0" ((1 :: Term s PRational) / 0)
            , goldenAssertFail "recip 0" (recip (0 :: Term s PRational))
            , goldenAssertFail "1/(1-1)" ((1 :: Term s PRational) / (1 - 1))
            ]
        ]
    ]

rat :: Term s PRational -> Term s PRational
rat = id
