module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Integer (PInteger)
import Plutarch (printTerm)

main :: IO ()
main = defaultMain tests

add1 :: Term (PInteger :--> PInteger :--> PInteger)
add1 = pLam2 $ \x y -> x + y + 1

add1Hoisted :: Term (PInteger :--> PInteger :--> PInteger)
add1Hoisted = pHoistAcyclic $ pLam2 $ \x y -> x + y + 1

example1 :: Term PInteger
example1 = add1Hoisted £ 12 £ 32 + add1Hoisted £ 5 £ 4

-- loop :: Term (PInteger :--> PInteger)
-- loop = pLam $ \x -> loop £ x

-- loopHoisted :: Term (PInteger :--> PInteger)
-- loopHoisted = pHoistAcyclic $ pLam $ \x -> loop £ x

freeHoisted :: Term (PInteger :--> PInteger)
freeHoisted = pLam $ \x -> pHoistAcyclic x

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "add1" $ (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1))"
  , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 ((\\i0 -> i0) (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1)))"
  , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i0 12 32) (i0 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1)))"
  , testCase "freeHoisted" $ (printTerm freeHoisted) @?= "(program 1.0.0 ((\\i0 -> addInteger (i0 12 32) (i0 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1)))"
  ]
