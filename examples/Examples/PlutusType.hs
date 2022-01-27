{-# LANGUAGE CPP #-}

module Examples.PlutusType (AB (..), swap, tests) where

import Plutarch
import Plutarch.Prelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import Utils

{- |
  A Sum type, which can be encoded as an Enum
-}
data AB (s :: S) = A | B

{- |
  AB is encoded as an Enum, using values of PInteger
  internally.
-}
instance PlutusType AB where
  type PInner AB _ = PInteger

  pcon' A = 0
  pcon' B = 1

  pmatch' x f =
    pif (x #== 0) (f A) (f B)

{- |
  Instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type.
-}
swap :: Term s AB -> Term s AB
swap x = pmatch x $ \case
  A -> pcon B
  B -> pcon A

tests :: HasTester => TestTree
tests =
  testGroup
    "PlutusType tests"
    [ testGroup
        "PlutusType examples"
        [ testCase "A encoded as 0" $ do
            pcon A `equal` (0 :: Term s PInteger)
        , testCase "B encoded as 2" $ do
            pcon B `equal` (1 :: Term s PInteger)
        , testCase "swap A == B" $ do
            swap (pcon A) `equal` pcon B
        , testCase "swap B == A" $ do
            swap (pcon B) `equal` pcon A
        ]
    , testGroup
        "PlutusType instances sanity"
        -- TODO: Add more sanity tests here!
        [ testCase "PBuiltinList" $ do
            pmatchTargetEvalBList $ pconstant [1 :: Integer, 2, 3, 4]
        ]
    ]

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}

-- | Make sure the target builtin list of 'pmatch' is not evaluated multiple times.
pmatchTargetEvalBList :: (HasTester, PLift a) => ClosedTerm (PBuiltinList a) -> Assertion
pmatchTargetEvalBList target = pmatch (ptrace (pconstant tag) target) (\case
  PCons x xs -> plet x $ \_ -> plet xs $ \_ -> pconstant ()
  PNil -> pconstant ())
#ifdef Development
  `traces` replicate 1 tag
#else
  `traces` []
#endif
  where
    tag = "evaluating"

-- FIXME: This needs to fully "evaluate" (e.g evaluate in UPLC) the `x` within the `pmatch` handler.
-- | Make sure the target of 'pmatch' is only evaluated once.
_pmatchTargetEval :: HasTester => PlutusType p => ClosedTerm p -> Assertion
_pmatchTargetEval target = pmatch (ptrace (pconstant tag) target) (\_ -> perror)
#ifdef Development
  `traces` replicate 1 tag
#else
  `traces` []
#endif
  where
    tag = "evaluating"
