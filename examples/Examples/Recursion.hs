module Examples.Recursion (iterateN, tests) where

import Plutarch
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils

{- |
  Example of a simple recursive function with pfix
-}
iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
iterateN = pfix # plam iterateN'
  where
    iterateN' ::
      Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
      Term s PInteger ->
      Term s (a :--> a) ->
      Term s a ->
      Term s a

    iterateN' self n f x =
      pif
        (n #== 0)
        x
        (self # (n - 1) # f #$ f # x)

iterateN_comp :: String
iterateN_comp =
  "(program 1.0.0 ((\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> \\i0 -> \\i0 -> force (force ifThenElse (equalsInteger i3 0) (delay i1) (delay (i4 (subtractInteger i3 1) i2 (i2 i1)))))))"

tests :: HasTester => TestTree
tests =
  testGroup
    "Recursion examples"
    [ testCase "iterateN compilation" $ printTerm iterateN @?= iterateN_comp
    , testCase "iterateN (10) (+1) 0 == 10" $ do
        (iterateN # 10 # succ # 0) `equal` (10 :: Term s PInteger)
    , testCase "iterateN 10 (*2) 1 == 1024" $ do
        (iterateN # 10 # square # 1) `equal` (1024 :: Term s PInteger)
    ]
  where
    succ :: Term s (PInteger :--> PInteger)
    succ = plam (\x -> x + 1)

    square :: Term s (PInteger :--> PInteger)
    square = plam (\x -> x * 2)
