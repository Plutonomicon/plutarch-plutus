module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Integer (PInteger)
import Plutarch.Either (PEither(PLeft,PRight))
import Plutarch.Bool (pIf, (£==))
import Plutarch (printTerm)

main :: IO ()
main = defaultMain tests

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = pLam2 $ \x y -> x + y + 1

add1Hoisted :: Term s (PInteger :--> PInteger :--> PInteger)
add1Hoisted = pHoistAcyclic $ pLam2 $ \x y -> x + y + 1

example1 :: Term s PInteger
example1 = add1Hoisted £ 12 £ 32 + add1Hoisted £ 5 £ 4

example2 :: Term s (PEither PInteger PInteger :--> PInteger)
example2 = pLam $ \x -> pMatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fib :: Term s (PInteger :--> PInteger)
fib = pFix £$ pLam2 $ \self n ->
  pIf (n £== 0)
    0
    $ pIf (n £== 1)
      1
      $ (self £ (n - 1)) + (self £ (n - 2))

-- FIXME: Make the below impossible using run-time checks.
-- loop :: Term (PInteger :--> PInteger)
-- loop = pLam $ \x -> loop £ x
-- loopHoisted :: Term (PInteger :--> PInteger)
-- loopHoisted = pHoistAcyclic $ pLam $ \x -> loop £ x

-- FIXME: Use property tests
tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "add1" $ (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1))"
  , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 ((\\i0 -> i0) (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1)))"
  , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i0 12 32) (i0 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i1 i0) 1)))"
  , testCase "example2" $ (printTerm example2) @?= "(program 1.0.0 (\\i0 -> i0 (\\i0 -> addInteger i0 1) (\\i0 -> subtractInteger i0 1)))"
  , testCase "pFix" $ (printTerm pFix) @?= "(program 1.0.0 ((\\i0 -> i0) (\\i0 -> (\\i0 -> i1 (\\i0 -> i1 i1 i0)) (\\i0 -> i1 (\\i0 -> i1 i1 i0)))))"
  , testCase "fib" $ (printTerm fib) @?= "(program 1.0.0 ((\\i0 -> i0 (\\i0 -> \\i0 -> force (ifThenElse (equalsInteger i0 0) (delay 0) (delay (force (ifThenElse (equalsInteger i0 1) (delay 1) (delay (addInteger (i1 (subtractInteger i0 1)) (i1 (subtractInteger i0 2)))))))))) (\\i0 -> (\\i0 -> i1 (\\i0 -> i1 i1 i0)) (\\i0 -> i1 (\\i0 -> i1 i1 i0)))))"
  ]
