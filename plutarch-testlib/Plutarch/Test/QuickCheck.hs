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
import Plutarch.Lift (PConstantDecl (PConstanted), PUnsafeLiftDecl (PLifted))
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
  forall (haskellInput :: Type) (haskellOutput :: Type).
  ( haskellInput ~ PLifted (PConstanted haskellInput)
  , PConstantDecl haskellInput
  , Pretty haskellInput
  , Arbitrary haskellInput
  , haskellOutput ~ PLifted (PConstanted haskellOutput)
  , PConstantDecl haskellOutput
  , Pretty haskellOutput
  , Eq haskellOutput
  ) =>
  (haskellInput -> haskellOutput) ->
  ClosedTerm (PConstanted haskellInput :--> PConstanted haskellOutput) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: haskellInput) -> goHaskell input `prettyEquals` plift (goPlutarch # pconstant input)

-- | @since WIP
checkHaskellEquivalent2 ::
  forall (haskellInput1 :: Type) (haskellInput2 :: Type) (haskellOutput :: Type).
  ( haskellInput1 ~ PLifted (PConstanted haskellInput1)
  , PConstantDecl haskellInput1
  , Pretty haskellInput1
  , Arbitrary haskellInput1
  , haskellInput2 ~ PLifted (PConstanted haskellInput2)
  , PConstantDecl haskellInput2
  , Pretty haskellInput2
  , Arbitrary haskellInput2
  , haskellOutput ~ PLifted (PConstanted haskellOutput)
  , PConstantDecl haskellOutput
  , Pretty haskellOutput
  , Eq haskellOutput
  ) =>
  (haskellInput1 -> haskellInput2 -> haskellOutput) ->
  ClosedTerm (PConstanted haskellInput1 :--> PConstanted haskellInput2 :--> PConstanted haskellOutput) ->
  Property
checkHaskellEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: haskellInput1, input2 :: haskellInput2) ->
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
