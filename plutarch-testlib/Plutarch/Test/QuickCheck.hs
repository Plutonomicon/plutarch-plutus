{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.QuickCheck (
  propEval,
  propEvalFail,
  propCompileFail,
  propEvalEqual,
  checkHaskellEquivalent,
  checkHaskellEquivalent2,
) where

import Data.Kind (Type)
import Data.Text qualified as Text
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.Prelude
import Plutarch.Test.Unit (TermResult (Evaluated, FailedToCompile, FailedToEvaluate), evalTermResult)
import Plutarch.Test.Utils (precompileTerm, prettyEquals, prettyShow)
import Prettyprinter (Pretty)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.QuickCheck (
  Arbitrary,
  Negative (Negative),
  NonNegative (NonNegative),
  NonPositive (NonPositive),
  NonZero (NonZero),
  Positive (Positive),
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

@since WIP
-}
propEvalFail ::
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> ClosedTerm b) ->
  TestTree
propEvalFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate _ _ -> property True
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testCompileFail` but generate terms

@since WIP
-}
propCompileFail ::
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> ClosedTerm b) ->
  TestTree
propCompileFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile _ -> property True
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testEvalEqual` but generate terms

@since WIP
-}
propEvalEqual ::
  (Arbitrary a, Show a) =>
  TestName ->
  -- | Actual
  (a -> ClosedTerm b) ->
  -- | Expected
  (a -> ClosedTerm b) ->
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

@since WIP
-}
propEval :: (Arbitrary a, Show a) => TestName -> (a -> ClosedTerm b) -> TestTree
propEval name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated _ _ -> property True

-- | @since WIP
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
  ClosedTerm (plutarchInput :--> plutarchOutput) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: haskellInput) -> goHaskell input `prettyEquals` plift (pfun # pconstant input)
  where
    pfun :: ClosedTerm (plutarchInput :--> plutarchOutput)
    pfun = precompileTerm goPlutarch

-- | @since WIP
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
  ClosedTerm (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput) ->
  Property
checkHaskellEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: AsHaskell plutarchInput1, input2 :: AsHaskell plutarchInput2) ->
      goHaskell input1 input2 `prettyEquals` plift (pfun # pconstant input1 # pconstant input2)
  where
    pfun :: ClosedTerm (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput)
    pfun = precompileTerm goPlutarch

-- * Orphans

-- | @since WIP
deriving newtype instance Pretty a => Pretty (Positive a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (Negative a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonZero a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonNegative a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonPositive a)
