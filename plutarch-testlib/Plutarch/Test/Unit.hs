-- | Utilities for unit testing plutarch terms
module Plutarch.Test.Unit (
  testCompileFail,
  testEval,
  testEvalFail,
  testEvalEqual,
  testEvalEqualTraces,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Plutarch (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug),
  TracingMode (DetTracing),
  compile,
  printScript,
 )
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

-- | Assert that term compiled and evaluated without errors
testEval :: TestName -> ClosedTerm a -> TestTree
testEval name term = testCase name $ do
  compiledTerm <- case compile (Tracing LogDebug DetTracing) term of
    Left err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    Right compiledTerm -> pure compiledTerm
  case evalScript compiledTerm of
    (Left err, _, _) -> assertFailure $ "Failed to eval: " <> show err
    (Right _, _, _) -> pure ()

-- | Assert that term compiled correctly but evaluated with errors
testEvalFail :: TestName -> ClosedTerm a -> TestTree
testEvalFail name term = testCase name $ do
  compiledTerm <- case compile NoTracing term of
    Left err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    Right compiledTerm -> pure compiledTerm
  case evalScript compiledTerm of
    (Left _, _, _) -> pure ()
    (Right evaluatedTerm, _, _) ->
      assertFailure ("Evaluated, but expected failure:\n" <> printScript evaluatedTerm)

-- | Assert that term failed to compile
testCompileFail :: TestName -> ClosedTerm a -> TestTree
testCompileFail name term = testCase name $ do
  case compile NoTracing term of
    Left _ -> pure ()
    Right _ -> assertFailure "Compiled, but expected failure"

{- | Assert that term compiled and evaluated without errors and matches the expected value
note that comparison is done on AST level, not by `Eq` or `PEq`
-}
testEvalEqual ::
  TestName ->
  -- | Actual
  ClosedTerm a ->
  -- | Expected
  ClosedTerm a ->
  TestTree
testEvalEqual name term expectedTerm = testCase name $ do
  compiledTerm <- case compile NoTracing term of
    Left err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    Right compiledTerm -> pure compiledTerm
  evaluatedTerm <- case evalScript compiledTerm of
    (Left err, _, _) -> assertFailure $ "Failed to eval: " <> show err
    (Right evaluatedTerm, _, _) -> pure evaluatedTerm
  compiledExpected <- case compile NoTracing expectedTerm of
    Left err -> assertFailure $ "Failed to compile expected value: " <> Text.unpack err
    Right compiledTerm -> pure compiledTerm
  evaluatedExpected <- case evalScript compiledExpected of
    (Left err, _, _) -> assertFailure $ "Failed to eval expected value: " <> show err
    (Right evaluatedTerm, _, _) -> pure evaluatedTerm
  assertEqual "" (printScript evaluatedExpected) (printScript evaluatedTerm)

{- | Assert that term compiled (with specified tracing level and `TracingMode.DetTracing`) and evaluated
without errors produced traces that match expected value. Note that this succeeds even if script
evaluated to error if traces still match
-}
testEvalEqualTraces :: TestName -> ClosedTerm a -> LogLevel -> [Text] -> TestTree
testEvalEqualTraces name term traceLevel expected = testCase name $ do
  compiledTerm <- case compile (Tracing traceLevel DetTracing) term of
    Left err -> assertFailure $ "Failed to compile: " <> Text.unpack err
    Right compiledTerm -> pure compiledTerm
  let (_, _, traces) = evalScript compiledTerm
  assertEqual "" expected traces
