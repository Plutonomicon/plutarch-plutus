{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.QuickCheck (
  propEvalFail,
  propCompileFail,
  propEvalEqual,
  checkHaskellEquivalent,
  checkHaskellEquivalent2,
) where

import Data.Text qualified as Text
import Plutarch (Config (NoTracing))
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import Plutarch.Test.Unit (TermResult (Evaluated, FailedToCompile, FailedToEvaluate), evalTermResult)
import Plutarch.Test.Utils (prettyEquals, prettyShow)
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

-- | @since WIP
checkHaskellEquivalent ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput
  , Pretty (PLifted plutarchInput)
  , Arbitrary (PLifted plutarchInput)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput :--> plutarchOutput) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: PLifted haskellInput) -> goHaskell input `prettyEquals` plift (goPlutarch # pconstant input)

-- | @since WIP
checkHaskellEquivalent2 ::
  forall (plutarchInput1 :: S -> Type) (plutarchInput2 :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput1
  , Pretty (PLifted plutarchInput1)
  , Arbitrary (PLifted plutarchInput1)
  , PUnsafeLiftDecl plutarchInput2
  , Pretty (PLifted plutarchInput2)
  , Arbitrary (PLifted plutarchInput2)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput1 -> PLifted plutarchInput2 -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput) ->
  Property
checkHaskellEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: PLifted haskellInput1, input2 :: PLifted haskellInput2) ->
      goHaskell input1 input2 `prettyEquals` plift (goPlutarch # pconstant input1 # pconstant input2)

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
