{-# LANGUAGE PackageImports #-}

module Plutarch.Test.Suite.Plutarch.Rational (tests) where

import Plutarch.Evaluate (evalTerm')
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.Prelude
import Plutarch.Rational (pproperFraction, preduce, ptruncate)
import Plutarch.Test.Methods (
  ppowPositiveBetter,
  pscaleIntegerBetter,
  pscaleNaturalBetter,
  pscalePositiveBetter,
 )
import Plutarch.Unsafe (punsafeCoerce)
import Test.Tasty (TestTree, testGroup)
import "plutarch-testlib" Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)

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
            [ goldenEval "+" (rat (1 / 2 + 1 / 2))
            , goldenEval "-" (rat (1 / 2 - 1 / 3))
            , goldenEval "*" (rat ((1 - 3 / 2) * (2 - 5 / 2)))
            , goldenEval "harmonic-sum" (rat (1 / 2 + 1 / 3 + 1 / 4 + 1 / 5))
            , goldenEval "multi-product" (rat (1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6))
            , goldenEval "abs" (rat (abs ((-1) / 2)))
            , goldenEval "signum" (rat (signum ((-1) / 2)))
            ]
        , goldenEval "compare" (rat (2 / 9) #< (3 / 10))
        , goldenGroup
            "round"
            [ goldenEval "5/3" (pround # (5 / 3 :: Term s PRational))
            , goldenEval "4/3" (pround # (4 / 3 :: Term s PRational))
            , goldenEval "-5/2" (pround # ((-5) / 2 :: Term s PRational))
            , goldenEval "-1/4" (pround # ((-1) / 4 :: Term s PRational))
            ]
        , goldenGroup
            "truncate"
            [ goldenEval "5/4" (ptruncate # (5 / 4 :: Term s PRational))
            , goldenEval "7/4" (ptruncate # (7 / 4 :: Term s PRational))
            , goldenEval "1/4" (ptruncate # (1 / 4 :: Term s PRational))
            , goldenEval "-7/4" (ptruncate # ((-7) / 4 :: Term s PRational))
            ]
        , goldenGroup
            "properFraction"
            [ goldenEval "-1/2" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP ((-1) / 2) 0 ((-1) / 2))
            , goldenEval "-3/2" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP ((-3) / 2) (-1) ((-1) / 2))
            , goldenEval "-4/3" (let mkP r a b = pmatch (pproperFraction # r) $ \(PPair x y) -> x #== a #&& y #== b in mkP ((-4) / 3) (-1) ((-1) / 3))
            ]
        , goldenGroup
            "data.id"
            [ goldenEval "0.5" (0.5 :: Term s PRational)
            , goldenEval "2" (2 :: Term s PRational)
            , goldenEval "11/3" (11 / 3 :: Term s PRational)
            ]
        , goldenGroup
            "div by 0"
            [ goldenEvalFail "1/0" ((1 :: Term s PRational) / 0)
            , goldenEvalFail "recip 0" (recip (0 :: Term s PRational))
            , goldenEvalFail "1/(1-1)" ((1 :: Term s PRational) / (1 - 1))
            ]
        ]
    , testGroup
        "Methods"
        [ pscalePositiveBetter sample (punsafeCoerce @_ @PInteger 10)
        , pscaleNaturalBetter sample (punsafeCoerce @_ @PInteger 10)
        , pscaleIntegerBetter sample 10
        , ppowPositiveBetter sample (punsafeCoerce @_ @PInteger 10)
        ]
    ]

rat :: Term s PRational -> Term s PRational
rat = id

sample :: forall (s :: S). Term s PRational
sample = evalTerm' NoTracing (preduce # pcon (PRational 15 (punsafeCoerce @_ @PInteger 17)))
