-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- * Plutarch specific `Expectation` operators
  passert,
  passertNot,
  pfails,
  psucceeds,
  ptraces,
  pshouldBe,
  (#@?=),

  -- * Budget expectation
  psatisfyWithinBenchmark,

  -- * Golden testing
  (@|),
  (@\),
  (@->),
  (@:->),
  (@==),
  pgoldenSpec,
  pgoldenSpec',
  PlutarchGoldens,
  GoldenConf (..),
  GoldenTest (..),

  -- * Benchmark type for use in `(@:->)`
  Benchmark (Benchmark, exBudgetCPU, exBudgetMemory, scriptSizeBytes),
  ScriptSizeBytes,

  -- * Test runner related utilities
  noUnusedGoldens,
  noUnusedGoldens',
  hspecAndReturnForest,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Plutarch (ClosedTerm, Config (Config, tracingMode), compile, pcon, printScript, pattern DetTracing)
import Plutarch.Bool (PBool (PFalse, PTrue))
import Plutarch.Evaluate (evalScript)
import Plutarch.Script qualified as Scripts
import Plutarch.Test.Benchmark (
  Benchmark (Benchmark, exBudgetCPU, exBudgetMemory, scriptSizeBytes),
  ScriptSizeBytes,
 )
import Plutarch.Test.Golden (
  GoldenConf (GoldenConf, chosenTests, goldenBasePath),
  GoldenTest (GoldenT'Bench, GoldenT'UPLCPostEval, GoldenT'UPLCPreEval),
  PlutarchGoldens,
  TermExpectation,
  evalScriptAlwaysWithBenchmark,
  pgoldenSpec,
  pgoldenSpec',
  (@->),
  (@:->),
  (@\),
  (@|),
 )
import Plutarch.Test.Run (hspecAndReturnForest, noUnusedGoldens, noUnusedGoldens')
import Test.Hspec (Expectation, expectationFailure, shouldBe, shouldSatisfy)
import Test.Tasty.HUnit (assertFailure)

comp :: ClosedTerm a -> Scripts.Script
comp t = either (error . T.unpack) id $ compile (Config {tracingMode = DetTracing}) t

{- |
    Like `shouldBe` but but for Plutarch terms
-}
pshouldBe :: ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- eval $ comp x
  p2 <- eval $ comp y
  pscriptShouldBe p1 p2
  where
    eval :: Scripts.Script -> IO Scripts.Script
    eval s = case evalScript s of
      (Left e, _, _) -> assertFailure $ "Script evaluation failed: " <> show e
      (Right x', _, _) -> pure x'

{- |
  Like `pshouldBe` but on `Script`
-}
pscriptShouldBe :: Scripts.Script -> Scripts.Script -> Expectation
pscriptShouldBe x y =
  printScript x `shouldBe` printScript y

-- | Like `@?=` but for Plutarch terms
(#@?=) :: ClosedTerm a -> ClosedTerm b -> Expectation
(#@?=) = pshouldBe

-- | Asserts the term to be true
passert :: ClosedTerm a -> Expectation
passert p = p #@?= pcon PTrue

-- | Asserts the term to be false
passertNot :: ClosedTerm a -> Expectation
passertNot p = p #@?= pcon PFalse

-- | Asserts the term evaluates successfully without failing
psucceeds :: ClosedTerm a -> Expectation
psucceeds p =
  case evalScript $ comp p of
    (Left _, _, _) -> expectationFailure "Term failed to evaluate"
    (Right _, _, _) -> pure ()

-- | Asserts the term evaluates without success
pfails :: ClosedTerm a -> Expectation
pfails p = do
  case evalScript $ comp p of
    (Left _, _, _) -> pure ()
    (Right _, _, _) -> expectationFailure "Term succeeded"

{- | Check that the given benchmark is within certain maximum values.

  Use this to ensure that a program's benchmark doesn't exceed expected values
  (such as script size or memory budget). You will need this because,

  - `Plutarch.Test`'s golden testing uses maximum possible ExBudget for evaluating
  programs
  - You may want to check that the script size is within a certain value
-}
psatisfyWithinBenchmark :: Benchmark -> Benchmark -> Expectation
psatisfyWithinBenchmark bench maxBudget = do
  shouldSatisfy bench $ \_ ->
    exBudgetCPU bench <= exBudgetCPU maxBudget
  shouldSatisfy bench $ \_ ->
    exBudgetMemory bench <= exBudgetMemory maxBudget
  shouldSatisfy bench $ \_ ->
    scriptSizeBytes bench <= scriptSizeBytes maxBudget

-- | Asserts that the term evaluates successfully with the given trace sequence
ptraces :: ClosedTerm a -> [Text] -> Expectation
ptraces p develTraces =
  case evalScript $ comp p of
    (Left _, _, _) -> expectationFailure "Term failed to evaluate"
    (Right _, _, traceLog) -> do
      traceLog `shouldBe` develTraces

-- | Test that the Plutarch program evaluates to the given term
(@==) :: ClosedTerm a -> ClosedTerm b -> TermExpectation a
(@==) p x = p @:-> \(_, script, _) -> script `pscriptShouldBe` xScript
  where
    xScript = fst . evalScriptAlwaysWithBenchmark $ comp x

infixr 1 @==
