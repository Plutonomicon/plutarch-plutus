{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}

module Utils (
  HasTester,
  standardTester,
  eval,
  equal,
  equalBudgeted,
  equal',
  fails,
  expect,
  throws,
  traces,
  succeeds,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Plutarch (ClosedTerm, compile, printScript)
import Plutarch.Evaluate (evaluateBudgetedScript, evaluateScript)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import qualified PlutusCore.Evaluation.Machine.ExMemory as ExMemory

-- import Shrink (shrinkScript)
import Test.Tasty.HUnit

newtype EvalImpl = EvalImpl {runEvalImpl :: forall (a :: PType). HasCallStack => ClosedTerm a -> IO Scripts.Script}
newtype EqualImpl = EqualImpl {runEqualImpl :: forall (a :: PType) (b :: PType). HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion}
newtype Equal'Impl = Equal'Impl {runEqual'Impl :: forall (a :: PType). HasCallStack => ClosedTerm a -> String -> Assertion}
newtype FailsImpl = FailsImpl {runFailsImpl :: forall (a :: PType). HasCallStack => ClosedTerm a -> Assertion}
newtype ExpectImpl = ExpectImpl {runExpectImpl :: HasCallStack => ClosedTerm PBool -> Assertion}
newtype ThrowsImpl = ThrowsImpl {runThrowsImpl :: forall (a :: PType). ClosedTerm a -> Assertion}
newtype TracesImpl = TracesImpl {runTracesImpl :: forall (a :: PType). ClosedTerm a -> [Text] -> Assertion}
newtype SucceedsImpl = SucceedsImpl {runSucceedsImpl :: ClosedTerm PUnit -> Assertion}

data Tester = Tester
  { evalImpl :: EvalImpl
  , equalImpl :: EqualImpl
  , equal'Impl :: Equal'Impl
  , failsImpl :: FailsImpl
  , expectImpl :: ExpectImpl
  , throwsImpl :: ThrowsImpl
  , tracesImpl :: TracesImpl
  , succeedsImpl :: SucceedsImpl
  }

type HasTester = (?tester :: Tester)

eval' :: HasCallStack => Scripts.Script -> IO Scripts.Script
eval' s = case evaluateScript s of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

standardTester :: Tester
standardTester =
  Tester
    { evalImpl = EvalImpl evalImpl
    , equalImpl = EqualImpl equalImpl
    , equal'Impl = Equal'Impl equal'Impl
    , failsImpl = FailsImpl failsImpl
    , expectImpl = ExpectImpl expectImpl
    , throwsImpl = ThrowsImpl throwsImpl
    , tracesImpl = TracesImpl tracesImpl
    , succeedsImpl = SucceedsImpl succeedsImpl
    }
  where
    evalImpl :: HasCallStack => ClosedTerm a -> IO Scripts.Script
    evalImpl x = eval' $ compile x

    equalImpl :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
    equalImpl x y = do
      x' <- evalImpl x
      y' <- evalImpl y
      printScript x' @?= printScript y'

    equal'Impl :: HasCallStack => ClosedTerm a -> String -> Assertion
    equal'Impl x y = do
      x' <- evalImpl x
      printScript x' @?= y

    failsImpl :: HasCallStack => ClosedTerm a -> Assertion
    failsImpl x =
      case evaluateScript $ compile x of
        Left (Scripts.EvaluationError _ _) -> mempty
        Left (Scripts.EvaluationException _ _) -> mempty
        Left e -> assertFailure $ "Script is malformed: " <> show e
        Right (_, _, s) -> assertFailure $ "Script didn't err: " <> printScript s

    expectImpl :: HasCallStack => ClosedTerm PBool -> Assertion
    expectImpl = equalImpl (pcon PTrue :: Term s PBool)

    throwsImpl :: HasCallStack => ClosedTerm a -> Assertion
    throwsImpl x =
      try @SomeException (putStrLn $ printScript $ compile x) >>= \case
        Right _ -> assertFailure "Supposed to throw"
        Left _ -> pure ()

    tracesImpl :: HasCallStack => ClosedTerm a -> [Text] -> Assertion
    tracesImpl x sl =
      case evaluateScript $ compile x of
        Left e -> assertFailure $ "Script evalImpluation failed: " <> show e
        Right (_, traceLog, _) -> traceLog @?= sl

    succeedsImpl :: HasCallStack => ClosedTerm PUnit -> Assertion
    succeedsImpl x = case evaluateScript $ compile x of
      Left e -> assertFailure $ "Script evaluation failed: " <> show e
      Right _ -> pure ()

{-
shrinkTester :: Tester
shrinkTester =
  Tester
    { evalImpl = EvalImpl evalImpl
    , equalImpl = EqualImpl equalImpl
    , equal'Impl = Equal'Impl equal'Impl
    , failsImpl = FailsImpl failsImpl
    , expectImpl = ExpectImpl expectImpl
    , throwsImpl = ThrowsImpl throwsImpl
    , tracesImpl = TracesImpl tracesImpl
    }
  where
    evalImpl :: HasCallStack => ClosedTerm a -> IO Scripts.Script
    evalImpl x = eval' . shrinkScript $ compile x

    equalImpl :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
    equalImpl x y = do
      x' <- evalImpl x
      y' <- evalImpl y
      printScript x' @?= printScript y'

    equal'Impl :: HasCallStack => ClosedTerm a -> String -> Assertion
    equal'Impl x y = do
      x' <- let ?tester = standardTester in eval x
      printScript x' @?= y

    failsImpl :: HasCallStack => ClosedTerm a -> Assertion
    failsImpl x =
      case evaluateScript . shrinkScript $ compile x of
        Left (Scripts.EvaluationError _ _) -> mempty
        Left (Scripts.EvaluationException _ _) -> mempty
        Left e -> assertFailure $ "Script is malformed: " <> show e
        Right (_, _, s) -> assertFailure $ "Script didn't err: " <> printScript s

    expectImpl :: HasCallStack => ClosedTerm PBool -> Assertion
    expectImpl = equalImpl (pcon PTrue :: Term s PBool)

    throwsImpl :: HasCallStack => ClosedTerm a -> Assertion
    throwsImpl x =
      try @SomeException (putStrLn . printScript . shrinkScript $ compile x) >>= \case
        Right _ -> assertFailure "Supposed to throw"
        Left _ -> pure ()

    tracesImpl :: HasCallStack => ClosedTerm a -> [Text] -> Assertion
    tracesImpl x sl =
      case evaluateScript . shrinkScript $ compile x of
        Left e -> assertFailure $ "Script evalImpluation failed: " <> show e
        Right (_, traceLog, _) -> traceLog @?= sl
-}

eval :: (HasCallStack, HasTester) => ClosedTerm a -> IO Scripts.Script
eval = runEvalImpl (evalImpl ?tester)
equal :: forall (a :: PType) (b :: PType). (HasCallStack, HasTester) => ClosedTerm a -> ClosedTerm b -> Assertion
equal x y = runEqualImpl (equalImpl ?tester) x y
equal' :: (HasCallStack, HasTester) => ClosedTerm a -> String -> Assertion
equal' = runEqual'Impl (equal'Impl ?tester)
fails :: (HasCallStack, HasTester) => ClosedTerm a -> Assertion
fails = runFailsImpl (failsImpl ?tester)
expect :: (HasCallStack, HasTester) => ClosedTerm PBool -> Assertion
expect = runExpectImpl (expectImpl ?tester)
throws :: (HasCallStack, HasTester) => ClosedTerm a -> Assertion
throws = runThrowsImpl (throwsImpl ?tester)
traces :: (HasCallStack, HasTester) => ClosedTerm a -> [Text] -> Assertion
traces = runTracesImpl (tracesImpl ?tester)
succeeds :: (HasCallStack, HasTester) => ClosedTerm PUnit -> Assertion
succeeds = runSucceedsImpl (succeedsImpl ?tester)

evalBudgeted :: HasCallStack => ClosedTerm a -> IO Scripts.Script
evalBudgeted x = case evaluateBudgetedScript (ExBudget maxCPU maxMemory) $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

maxCPU :: ExMemory.ExCPU
maxCPU = ExMemory.ExCPU 4000

maxMemory :: ExMemory.ExMemory
maxMemory = ExMemory.ExMemory 4000

equalBudgeted :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
equalBudgeted x y = do
  x' <- evalBudgeted x
  y' <- evalBudgeted y
  printScript x' @?= printScript y'
