module Plutarch.Test.QuickCheck (propEvalFail, propCompileFail, propEvalEqual) where

import Data.Text qualified as Text
import Plutarch (Config (NoTracing))
import Plutarch.Prelude
import Plutarch.Test.Unit (TermResult (Evaluated, FailedToCompile, FailedToEvaluate), evalTermResult)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.QuickCheck (
  Arbitrary,
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
