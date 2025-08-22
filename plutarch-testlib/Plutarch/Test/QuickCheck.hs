{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.QuickCheck (
  propEval,
  propEvalFail,
  propCompileFail,
  propEvalEqual,
  propPTryFromRoundrip,
  checkHaskellEquivalent,
  checkHaskellEquivalent2,
) where

import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Kind (Type)
import Data.Text qualified as Text
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.Prelude
import Plutarch.Test.Unit (TermResult (Evaluated, FailedToCompile, FailedToEvaluate), evalTermResult)
import Plutarch.Test.Utils (precompileTerm, prettyEquals, prettyShow)
import Prettyprinter (Pretty)
import Test.QuickCheck (forAll)
import Test.QuickCheck qualified as QuickCheck
import Test.Tasty (TestName, TestTree)
import Test.Tasty.QuickCheck (
  Arbitrary,
  Negative (Negative),
  NonNegative (NonNegative),
  NonPositive (NonPositive),
  NonZero (NonZero),
  Property,
  arbitrary,
  counterexample,
  forAllShrinkShow,
  property,
  shrink,
  testProperty,
  (===),
 )

{- | Like `Plutarch.Test.Unit.testEvalFail` but generate terms

@since 1.0.0
-}
propEvalFail ::
  forall (a :: Type) (b :: S -> Type).
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> (forall (s :: S). Term s b)) ->
  TestTree
propEvalFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate _ _ -> property True
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testCompileFail` but generate terms

@since 1.0.0
-}
propCompileFail ::
  forall (a :: Type) (b :: S -> Type).
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> (forall (s :: S). Term s b)) ->
  TestTree
propCompileFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile _ -> property True
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testEvalEqual` but generate terms

@since 1.0.0
-}
propEvalEqual ::
  forall (a :: Type) (b :: S -> Type).
  (Arbitrary a, Show a) =>
  TestName ->
  -- | Actual
  (a -> (forall (s0 :: S). Term s0 b)) ->
  -- | Expected
  (a -> (forall (s1 :: S). Term s1 b)) ->
  TestTree
propEvalEqual name mkTerm mkExpected =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated actual _ -> case evalTermResult NoTracing (mkExpected input) of
        FailedToCompile err -> counterexample ("Failed to compile expected term: " <> Text.unpack err) False
        FailedToEvaluate err _ -> counterexample ("Failed to evaluate expected term: " <> show err) False
        Evaluated expected _ -> actual === expected

{- | Like `Plutarch.Test.Unit.testEval` but generate terms

@since 1.0.0
-}
propEval :: forall (a :: Type) (b :: S -> Type). (Arbitrary a, Show a) => TestName -> (a -> (forall (s :: S). Term s b)) -> TestTree
propEval name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated _ _ -> property True

-- | @since 3.1.1
propPTryFromRoundrip ::
  forall a.
  ( Show (AsHaskell a)
  , Arbitrary (AsHaskell a)
  , Typeable (AsHaskell a)
  , PLiftable a
  , PEq a
  , PIsData a
  , PTryFrom PData (PAsData a)
  ) =>
  TestTree
propPTryFromRoundrip = testProperty testName $ forAll arbitrary $ \original ->
  plift (precompileTerm (roundtripScript @a) # pconstant original)
  where
    testName :: TestName
    testName = "PTryFrom PData (PAsData " ++ show (typeRep (Proxy @(AsHaskell a))) ++ ")"

    roundtripScript ::
      forall (a :: S -> Type) (s :: S).
      (PEq a, PIsData a, PTryFrom PData (PAsData a)) =>
      Term s (a :--> PBool)
    roundtripScript = plam $ \original -> do
      let encoded = pdataImpl original
      let decoded = pfromData . unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) encoded)
      original #== decoded

-- | @since 1.0.0
checkHaskellEquivalent ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PLiftable plutarchInput
  , PLiftable plutarchOutput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , Pretty (AsHaskell plutarchOutput)
  , Eq (AsHaskell plutarchOutput)
  ) =>
  (AsHaskell plutarchInput -> AsHaskell plutarchOutput) ->
  (forall (s0 :: S). Term s0 (plutarchInput :--> plutarchOutput)) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: haskellInput) -> goHaskell input `prettyEquals` plift (pfun # pconstant input)
  where
    pfun :: forall (s1 :: S). Term s1 (plutarchInput :--> plutarchOutput)
    pfun = precompileTerm goPlutarch

-- | @since 1.0.0
checkHaskellEquivalent2 ::
  forall (plutarchInput1 :: S -> Type) (plutarchInput2 :: S -> Type) (plutarchOutput :: S -> Type).
  ( PLiftable plutarchInput1
  , Pretty (AsHaskell plutarchInput1)
  , Arbitrary (AsHaskell plutarchInput1)
  , PLiftable plutarchInput2
  , Pretty (AsHaskell plutarchInput2)
  , Arbitrary (AsHaskell plutarchInput2)
  , PLiftable plutarchOutput
  , Pretty (AsHaskell plutarchOutput)
  , Eq (AsHaskell plutarchOutput)
  ) =>
  (AsHaskell plutarchInput1 -> AsHaskell plutarchInput2 -> AsHaskell plutarchOutput) ->
  (forall (s0 :: S). Term s0 (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput)) ->
  Property
checkHaskellEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: AsHaskell plutarchInput1, input2 :: AsHaskell plutarchInput2) ->
      goHaskell input1 input2 `prettyEquals` plift (pfun # pconstant input1 # pconstant input2)
  where
    pfun :: forall (s1 :: S). Term s1 (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput)
    pfun = precompileTerm goPlutarch

-- * Orphans

-- | @since 1.0.0
deriving newtype instance Pretty a => Pretty (QuickCheck.Positive a)

-- | @since 1.0.0
deriving newtype instance Pretty a => Pretty (Negative a)

-- | @since 1.0.0
deriving newtype instance Pretty a => Pretty (NonZero a)

-- | @since 1.0.0
deriving newtype instance Pretty a => Pretty (NonNegative a)

-- | @since 1.0.0
deriving newtype instance Pretty a => Pretty (NonPositive a)
