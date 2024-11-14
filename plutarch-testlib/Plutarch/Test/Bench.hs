{-# LANGUAGE FlexibleInstances #-}

{- | Plutarch benchmarking tools

Interface mirrors the one from @tasty-bench@ but 'bench' instead of taking @Benchmarkable@
takes 'Plutarch.ClosedTerm'
-}
module Plutarch.Test.Bench (
  BenchConfig (Optimizing, NonOptimizing),
  Plutarch.Test.Bench.defaultMain,
  bench,
  benchWithConfig,
  bcompare,
  bcompareWithin,
) where

import Data.ByteString.Short qualified as Short
import Data.SatInt (fromSatInt)
import Data.Text qualified as Text
import Plutarch (Config (NoTracing), compile)
import Plutarch.Evaluate (evalScriptUnlimited)
import Plutarch.Internal (compileOptimized)
import Plutarch.Prelude
import Plutarch.Script (Script (unScript))
import Plutarch.Test.Bench.Meta qualified as Meta
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusLedgerApi.Common (serialiseUPLC)
import Test.Tasty
import Test.Tasty.Providers (testFailed)
import Test.Tasty.Runners (Result)

-- | @since WIP
data BenchConfig
  = -- | Compile with UPLC simplifier pass and no tracing
    Optimizing
  | -- | Compile without UPLC simplifier and configurable tracing
    NonOptimizing Config

{- | Use this instead of 'Test.Tasty.defaultMain' from @Test.Tasty@ to run benchmarks to get formatted output

@since WIP
-}
defaultMain :: TestTree -> IO ()
defaultMain = Meta.defaultMain @["CPU", "MEM", "SIZE"]

{- | Create benchmark from Plutarch term without tracing and no UPLC simplifier

@since WIP
-}
bench :: TestName -> ClosedTerm a -> TestTree
bench name = benchWithConfig name (NonOptimizing NoTracing)

{- | Like 'bench' but with customizable compilation config

@since WIP
-}
benchWithConfig :: TestName -> BenchConfig -> ClosedTerm a -> TestTree
benchWithConfig name config term = Meta.bench @["CPU", "MEM", "SIZE"] name (PBenchmarkable config term) runPBenchmarkable

{- | Compare benchmarks, reporting relative CPU, MEM, and size differences

 @since WIP
-}
bcompare ::
  -- | Tasty pattern to compare as baseline
  String ->
  -- | Test or test tree to compare with baseline test
  TestTree ->
  TestTree
bcompare = bcompareWithin (-1 / 0, 1 / 0) (-1 / 0, 1 / 0) (-1 / 0, 1 / 0)

{- | Like 'bcompare' but with customizable upper and lower bounds of relative differences

@since WIP
-}
bcompareWithin ::
  -- | CPU bounds
  (Double, Double) ->
  -- | MEM bounds
  (Double, Double) ->
  -- | Size bounds
  (Double, Double) ->
  -- | Tasty pattern to compare as baseline
  String ->
  -- | Test or test tree to compare with baseline test
  TestTree ->
  TestTree
bcompareWithin cpu mem size =
  Meta.bcompareWithin @["CPU", "MEM", "SIZE"] (cpu, (mem, size))

-- * Utils

scriptSize :: Script -> Integer
scriptSize = fromIntegral . Short.length . serialiseUPLC . unScript

data PBenchmarkable where
  PBenchmarkable :: BenchConfig -> ClosedTerm a -> PBenchmarkable

runPBenchmarkable :: PBenchmarkable -> Either Result (Integer, (Integer, Integer))
runPBenchmarkable (PBenchmarkable config term) =
  case compiled of
    Left err -> Left $ testFailed $ "Failed to compile term: " <> Text.unpack err
    Right script ->
      case evalScriptUnlimited script of
        (Left err, _, _) -> Left $ testFailed $ "Failed to evaluate term: " <> show err
        (Right _, ExBudget (ExCPU cpu) (ExMemory mem), _) ->
          Right (fromSatInt cpu, (fromSatInt mem, scriptSize script))
  where
    compiled =
      case config of
        Optimizing -> compileOptimized term
        NonOptimizing pconfig -> compile pconfig term
