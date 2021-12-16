{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch (ClosedTerm, compile, printScript, printTerm)
import Plutarch.Bool (PBool (..), pif, (£==))
import Plutarch.Builtin (PBuiltinString, phexByteStr)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.Unit
import qualified Plutus.V1.Ledger.Scripts as Scripts

main :: IO ()
main = defaultMain tests

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1

add1Hoisted :: Term s (PInteger :--> PInteger :--> PInteger)
add1Hoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

example1 :: Term s PInteger
example1 = add1Hoisted £ 12 £ 32 + add1Hoisted £ 5 £ 4

example2 :: Term s (PEither PInteger PInteger :--> PInteger)
example2 = plam $ \x -> pmatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix £$ plam $ \self n ->
    pif
      (n £== 0)
      0
      $ pif
        (n £== 1)
        1
        $ self £ (n - 1) + self £ (n - 2)

uglyDouble :: Term s (PInteger :--> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2

equal :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
equal x y =
  let Right (_, _, x') = evaluateScript $ compile x
      Right (_, _, y') = evaluateScript $ compile y
   in printScript x' @?= printScript y'

fails :: HasCallStack => ClosedTerm a -> Assertion
fails x =
  case evaluateScript $ compile x of
    Left (Scripts.EvaluationError _ _) -> mempty
    e -> assertFailure $ "Script didn't err: " <> show e

expect :: HasCallStack => (forall s. Term s PBool) -> Assertion
expect = equal (pcon PTrue :: Term s PBool)

-- FIXME: Make the below impossible using run-time checks.
-- loop :: Term (PInteger :--> PInteger)
-- loop = plam $ \x -> loop £ x
-- loopHoisted :: Term (PInteger :--> PInteger)
-- loopHoisted = phoistAcyclic $ plam $ \x -> loop £ x

-- FIXME: Use property tests
tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testCase "add1" $ (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 ((\\i0 -> i0) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i0 12 32) (i0 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example2" $ (printTerm example2) @?= "(program 1.0.0 (\\i0 -> i1 (\\i0 -> addInteger i1 1) (\\i0 -> subtractInteger i1 1)))"
    , testCase "pfix" $ (printTerm pfix) @?= "(program 1.0.0 ((\\i0 -> i0) (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1)))))"
    , testCase "fib" $ (printTerm fib) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> i0) (i0 (\\i0 -> \\i0 -> force (ifThenElse (equalsInteger i1 0) (delay 0) (delay (force (ifThenElse (equalsInteger i1 1) (delay 1) (delay (addInteger (i2 (subtractInteger i1 1)) (i2 (subtractInteger i1 2))))))))))) (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1)))))"
    , testCase "uglyDouble" $ (printTerm uglyDouble) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
    , testCase "1 + 2 == 3" $ equal (1 + 2 :: Term s PInteger) (3 :: Term s PInteger)
    , testCase "fails: perror" $ fails perror
    , testCase "() == ()" $ expect $ pmatch (pcon PUnit) (\case PUnit -> (pcon PTrue))
    , testCase "0x02af == 0x02af" $ expect $ phexByteStr "02af" £== phexByteStr "02af"
    , testCase "\"foo\" == \"foo\"" $ expect $ "foo" £== ("foo" :: Term s PBuiltinString)
    , testCase "PBuiltinByteString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = phexByteStr "152a" in (mempty <> a) £== a
        expect $ let a = phexByteStr "4141" in (a <> mempty) £== a
    , testCase "PBuiltinString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = "foo" :: Term s PBuiltinString in (mempty <> a) £== a
        expect $ let a = "bar" :: Term s PBuiltinString in (a <> mempty) £== a
    , testCase "PBuiltinByteString :: 0x12 <> 0x34 == 0x1234" $
        expect $
          (phexByteStr "12" <> phexByteStr "34") £== phexByteStr "1234"
    , testCase "PBuiltinString :: \"ab\" <> \"cd\" == \"abcd\"" $
        expect $
          ("ab" <> "cd") £== ("abcd" :: Term s PBuiltinString)
    , testCase "PBuiltinByteString mempty" $ expect $ mempty £== phexByteStr ""
    , testCase "PBuiltinString mempty" $ expect $ mempty £== ("" :: Term s PBuiltinString)
    ]
