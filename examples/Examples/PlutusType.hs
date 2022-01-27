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
            pmatchTargetEval $ pconstant [1 :: Integer, 2, 3, 4]
        ]
    ]

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}

-- | Make sure the target of 'pmatch' is only evaluated once.
pmatchTargetEval :: HasTester => PlutusType p => ClosedTerm p -> Assertion
pmatchTargetEval target = pmatch (ptrace (pconstant tag) target) (\x -> plet (pcon x) $ \_ -> pconstant ())
#ifdef Development
  `traces` replicate 1 tag
#else
  `traces` []
#endif
  where
    tag = "evaluating"
