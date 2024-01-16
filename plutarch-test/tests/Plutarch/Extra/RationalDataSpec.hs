module Plutarch.Extra.RationalDataSpec (spec) where

import Hedgehog (Gen, PropertyT, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Plutarch.Extra.RationalData ()
import Plutarch.Prelude
import Plutarch.Test (passert, pgoldenSpec, (@->), (@|))
import Plutarch.Test.Property.HaskEquiv (testPEq)
import PlutusTx.Ratio qualified as PlutusTx
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

spec :: Spec
spec = modifyMaxSuccess (const 10_000) $ do
  describe "extra.rationaldatautils" $ do
    describe "properties" $ do
      it "normalizes" . hedgehog $ propNorm
      it "x / y == a / b iff xb == ya" . hedgehog $ propEq
      it "x / y < a / b iff xb < ya" . hedgehog $ propLt
      it "x / y <= a / b iff xb <= ya" . hedgehog $ propLe
    describe "regression" . pgoldenSpec $ do
      "0/1 #< 758566050/758566050 should be true" @| regression1 @-> passert
      "758566050/758566050 #<= 1/1 should be true" @| regression2 @-> passert
      "1/1 #== 2/2 should be true" @| regression3 @-> passert

-- Helpers

-- Properties

propNorm :: PropertyT IO ()
propNorm = do
  n <- forAll genNumeratorSmall
  d <- forAll . Gen.choice $ [genDenominatorN, genDenominatorP]
  let frac = PlutusTx.unsafeRatio n d
  -- If signa match, answer must be positive
  if
    | signum n == signum d -> do
        let absFrac = PlutusTx.unsafeRatio (abs n) (abs d)
        testPEq (pconstant frac) (pconstant absFrac)
    -- If numerator is zero, we get zero no matter what
    | signum n == 0 -> do
        let zeroFrac = PlutusTx.unsafeRatio 0 1
        testPEq (pconstant frac) (pconstant zeroFrac)
    -- Otherwise, answer must be negative
    | otherwise -> do
        let negFrac = PlutusTx.unsafeRatio (negate . abs $ n) (abs d)
        testPEq (pconstant frac) (pconstant negFrac)

propEq :: PropertyT IO ()
propEq = do
  x <- forAll genNumerator
  y <- forAll genDenominator
  a <- forAll genNumerator
  b <- forAll genDenominator
  let lhsFrac = PlutusTx.unsafeRatio x y
  let rhsFrac = PlutusTx.unsafeRatio a b
  testPEq
    (pconstant lhsFrac #== pconstant rhsFrac)
    (pconstant (x * b) #== pconstant (y * a))

propLt :: PropertyT IO ()
propLt = do
  x <- forAll genNumerator
  y <- forAll genDenominator
  a <- forAll genNumerator
  b <- forAll genDenominator
  let lhsFrac = PlutusTx.unsafeRatio x y
  let rhsFrac = PlutusTx.unsafeRatio a b
  testPEq
    (pconstant lhsFrac #< pconstant rhsFrac)
    (pconstant (x * b) #< pconstant (y * a))

propLe :: PropertyT IO ()
propLe = do
  x <- forAll genNumerator
  y <- forAll genDenominator
  a <- forAll genNumerator
  b <- forAll genDenominator
  let lhsFrac = PlutusTx.unsafeRatio x y
  let rhsFrac = PlutusTx.unsafeRatio a b
  testPEq
    (pconstant lhsFrac #<= pconstant rhsFrac)
    (pconstant (x * b) #<= pconstant (y * a))

-- Generators

genNumerator :: Gen Integer
genNumerator = Gen.integral $ Range.linearFrom 0 (-1_000_000_000) 1_000_000_000

genNumeratorSmall :: Gen Integer
genNumeratorSmall = Gen.integral $ Range.linearFrom 0 (-100) 100

genDenominator :: Gen Integer
genDenominator = Gen.integral $ Range.linear 1 1_000_000_000

genDenominatorN :: Gen Integer
genDenominatorN = Gen.integral $ Range.linear (-1) (-100)

genDenominatorP :: Gen Integer
genDenominatorP = Gen.integral $ Range.linear 1 100

-- Regression cases

regression1 :: forall (s :: S). Term s PBool
regression1 = unTermCont $ do
  let lhs = pconstant $ PlutusTx.unsafeRatio 0 1
  let rhs = pconstant $ PlutusTx.unsafeRatio 758566050 758566050
  pure $ lhs #< rhs

regression2 :: forall (s :: S). Term s PBool
regression2 = unTermCont $ do
  let lhs = pconstant $ PlutusTx.unsafeRatio 758566050 758566050
  let rhs = pconstant $ PlutusTx.unsafeRatio 1 1
  pure $ lhs #<= rhs

regression3 :: forall (s :: S). Term s PBool
regression3 = unTermCont $ do
  let lhs = pconstant $ PlutusTx.unsafeRatio 1 1
  let rhs = pconstant $ PlutusTx.unsafeRatio 2 2
  pure $ lhs #== rhs
