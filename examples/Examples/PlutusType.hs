{-# LANGUAGE CPP #-}

module Examples.PlutusType (tests) where

import Plutarch
import Plutarch.Prelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import Utils

tests :: HasTester => TestTree
tests =
  testGroup
    "PlutusType tests"
    [ testGroup
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
