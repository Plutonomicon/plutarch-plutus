-- | Utilities for unit testing plutarch terms
module Plutarch.Test.Unit (
  testCompileFail,
  testEval,
  testEvalFail,
  testEvalEqual,
  testEvalEqualTraces,
  TermResult (..),
  evalTermResult,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Plutarch.Evaluate (EvalError, evalScriptUnlimited)
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug),
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.Prelude
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

{- | Assert that term compiled and evaluated without errors

@since 1.0.0
-}
testEval :: TestName -> ClosedTerm a -> TestTree
testEval name term = testCase name $ do
  case evalTermResult (Tracing LogDebug DetTracing) term of
    FailedToCompile err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    FailedToEvaluate err _ -> assertFailure $ "Failed to evaluate: " <> show err
    Evaluated _ _ -> pure ()

{- | Assert that term compiled correctly but evaluated with errors

@since 1.0.0
-}
testEvalFail :: TestName -> ClosedTerm a -> TestTree
testEvalFail name term = testCase name $ do
  case evalTermResult NoTracing term of
    FailedToCompile err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    FailedToEvaluate _ _ -> pure ()
    Evaluated script _ -> assertFailure ("Evaluated, but expected failure:\n" <> script)

{- | Assert that term failed to compile

@since 1.0.0
-}
testCompileFail :: TestName -> ClosedTerm a -> TestTree
testCompileFail name term = testCase name $ do
  case evalTermResult NoTracing term of
    FailedToCompile _ -> pure ()
    FailedToEvaluate err _ -> assertFailure $ "Failed to evaluate: " <> show err
    Evaluated script _ -> assertFailure $ "Evaluated, but expected failure: " <> script

{- | Assert that term compiled and evaluated without errors and matches the expected value
note that comparison is done on AST level, not by `Eq` or `PEq`

@since 1.0.0
-}
testEvalEqual ::
  TestName ->
  -- | Actual
  ClosedTerm a ->
  -- | Expected
  ClosedTerm a ->
  TestTree
testEvalEqual name term expectedTerm = testCase name $ do
  actual <- case evalTermResult NoTracing term of
    FailedToCompile err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    FailedToEvaluate err _ -> assertFailure $ "Failed to evaluate: " <> show err
    Evaluated script _ -> pure script
  case evalTermResult NoTracing expectedTerm of
    FailedToCompile err -> assertFailure $ "Failed to compile expected term: " <> Text.unpack err
    FailedToEvaluate err _ -> assertFailure $ "Failed to evaluate expected term: " <> show err
    Evaluated expected _ -> assertEqual "" expected actual

{- | Assert that term compiled (with specified tracing level and `TracingMode.DetTracing`) and evaluated
without errors produced traces that match expected value. Note that this succeeds even if script
evaluated to error if traces still match

@since 1.0.0
-}
testEvalEqualTraces :: TestName -> ClosedTerm a -> LogLevel -> [Text] -> TestTree
testEvalEqualTraces name term traceLevel expected = testCase name $
  case evalTermResult (Tracing traceLevel DetTracing) term of
    FailedToCompile err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    FailedToEvaluate _ traces -> assertEqual "" expected traces
    Evaluated _ traces -> assertEqual "" expected traces

-- | @since 1.0.0
data TermResult
  = FailedToCompile Text
  | FailedToEvaluate EvalError [Text]
  | Evaluated String [Text]

-- | @since 1.0.0
evalTermResult :: Config -> ClosedTerm a -> TermResult
evalTermResult config term =
  case compile config term of
    Left err -> FailedToCompile err
    Right compiledTerm ->
      case evalScriptUnlimited compiledTerm of
        (Left err, _, traces) -> FailedToEvaluate err traces
        (Right evaluated, _, traces) -> Evaluated (printScript evaluated) traces
