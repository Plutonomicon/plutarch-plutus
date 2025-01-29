{- | Utilities for golden testing

To regenerate golden tests it is enough to remove @./goldens@ directory and rerun tests
-}
module Plutarch.Test.Golden (
  GoldenTestTree,
  plutarchGolden,
  goldenGroup,
  goldenEval,
  goldenEvalFail,
) where

import Data.Aeson (ToJSON (toEncoding, toJSON), encode, object, pairs, (.=))
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Short qualified as Short
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term (
  ClosedTerm,
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.Script (Script (unScript))
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V1 (ExBudget (ExBudget), ExCPU, ExMemory)
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase)

{- | Opaque type representing tree of golden tests

@since 1.0.0
-}
data GoldenTestTree where
  GoldenTestTree :: TestName -> [GoldenTestTree] -> GoldenTestTree
  GoldenTestTreeEval :: TestName -> ClosedTerm a -> GoldenTestTree
  GoldenTestTreeEvalFail :: TestName -> ClosedTerm a -> GoldenTestTree

{- | Convert tree of golden tests into standard Tasty `TestTree`, capturing results produced
by nested golden tests

@since 1.0.0
-}
plutarchGolden ::
  TestName ->
  -- | Base file name of golden file path.
  --
  -- e.g. @"foo"@ will result in goldens:
  --
  -- * @.//goldens//foo.bench.golden@ - With execution units and size
  --
  -- * @.//goldens//foo.uplc.eval.golden@ - With AST after evaluation
  --
  -- * @.//goldens//foo.uplc.golden@ - With AST before evaluation
  FilePath ->
  [GoldenTestTree] ->
  TestTree
plutarchGolden topName goldenPath tests = testGroup topName testsWithGoldens
  where
    -- Implementation note: Because we want to collect all Benchmarks created by nested tests
    -- we cannot use plain TestTree for these (without hacks like passing some MVars around)
    -- so we have out own GoldenTestTree that when being converted to TestTree will execute
    -- all terms and collect the results. Additionally this ensures that goldens remain the same
    -- when using `--pattern` to filter tests because even though assertions won't run the
    -- scripts will still be evaluated

    (tests', benchmarks') = unzip $ map mkTest tests
    benchmarks = mconcat benchmarks'
    goldenTests =
      [ testGroup
          "Golden Files"
          [ goldenVsString
              (goldenPath <> ".bench.golden")
              ("goldens" </> goldenPath <> ".bench.golden")
              (pure $ mkBenchGoldenValue benchmarks)
          , goldenVsString
              (goldenPath <> ".uplc.golden")
              ("goldens" </> goldenPath <> ".uplc.golden")
              (pure $ mkUplcGoldenValue benchmarks)
          , goldenVsString
              (goldenPath <> ".uplc.eval.golden")
              ("goldens" </> goldenPath <> ".uplc.eval.golden")
              (pure $ mkUplcEvalGoldenValue benchmarks)
          ]
      ]
    testsWithGoldens = goldenTests <> tests'

{- | Like `Test.Tasty.testGroup` but for golden tests

Goldens in the group will be prefixed by the group name

@since 1.0.0
-}
goldenGroup :: TestName -> [GoldenTestTree] -> GoldenTestTree
goldenGroup = GoldenTestTree

{- | Like `Plutarch.Test.Unit.testEval` but will append to goldens created by enclosing `plutarchGolden`

@since 1.0.0
-}
goldenEval :: TestName -> ClosedTerm a -> GoldenTestTree
goldenEval = GoldenTestTreeEval

{- | Like `Plutarch.Test.Unit.testEvalFail` but will append to goldens created by enclosing `plutarchGolden`

@since 1.0.0
-}
goldenEvalFail :: TestName -> ClosedTerm a -> GoldenTestTree
goldenEvalFail = GoldenTestTreeEvalFail

-- Internals

mkFailed :: TestName -> (e -> String) -> Either e a -> Either (TestTree, [(TestName, Benchmark)]) a
mkFailed name showErr = either (Left . (,[]) . testCase name . assertFailure . showErr) Right

mkTest :: GoldenTestTree -> (TestTree, [(TestName, Benchmark)])
mkTest (GoldenTestTree name tests) = (testGroup name tests', benchmarks)
  where
    (tests', benchmarks') = unzip $ map mkTest tests
    benchmarks = foldMap (map (first ((name <> ".") <>))) benchmarks'
mkTest (GoldenTestTreeEval name term) = either id id $ do
  benchmark <- mkFailed name Text.unpack $ benchmarkTerm term
  _ <- mkFailed name show $ result benchmark
  pure (testCase name (pure ()), [(name, benchmark)])
mkTest (GoldenTestTreeEvalFail name term) = either id id $ do
  benchmark <- mkFailed name Text.unpack $ benchmarkTerm term
  pure $ case result benchmark of
    Left _ -> (testCase name (pure ()), [(name, benchmark)])
    Right _ ->
      ( testCase name $ assertFailure "Script did not terminate with error as expected"
      , [(name, benchmark)]
      )

benchmarkTerm :: ClosedTerm a -> Either Text Benchmark
benchmarkTerm term = do
  compiled <- compile testConfig term
  let (res, ExBudget cpu mem, _traces) = evalScript compiled
  pure $ Benchmark cpu mem (scriptSize compiled) res compiled

testConfig :: Config
testConfig = Tracing LogInfo DetTracing

scriptSize :: Script -> Int64
scriptSize = fromIntegral . Short.length . serialiseUPLC . unScript

data Benchmark = Benchmark
  { exBudgetCPU :: ExCPU
  -- ^ CPU budget used by the script.
  , exBudgetMemory :: ExMemory
  -- ^ Memory budget used by the script.
  , scriptSizeBytes :: Int64
  -- ^ Size of Plutus script in bytes
  , result :: Either EvalError Script
  , unevaluated :: Script
  }
  deriving stock (Show)

newtype PerfBenchmark = PerfBenchmark Benchmark

instance ToJSON PerfBenchmark where
  toJSON (PerfBenchmark (Benchmark cpu mem size _ _)) =
    object
      [ "exBudgetCPU" .= cpu
      , "exBudgetMemory" .= mem
      , "scriptSizeBytes" .= size
      ]
  toEncoding (PerfBenchmark (Benchmark cpu mem size _ _)) =
    pairs $
      mconcat
        [ "exBudgetCPU" .= cpu
        , "exBudgetMemory" .= mem
        , "scriptSizeBytes" .= size
        ]

mkBenchmarkValue :: (Benchmark -> ByteString) -> [(TestName, Benchmark)] -> ByteString
mkBenchmarkValue go =
  LBS.unlines
    . map
      ( \(testName, benchmark) ->
          mconcat
            [ encodeStringUtf8 testName
            , " "
            , go benchmark
            ]
      )

mkBenchGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkBenchGoldenValue = mkBenchmarkValue (encode . PerfBenchmark)

mkUplcEvalGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkUplcEvalGoldenValue = mkBenchmarkValue (either (const "program 1.0.0 error") (encodeStringUtf8 . printScript) . result)

mkUplcGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkUplcGoldenValue = mkBenchmarkValue (encodeStringUtf8 . printScript . unevaluated)

encodeStringUtf8 :: String -> ByteString
encodeStringUtf8 = LBS.fromStrict . Encoding.encodeUtf8 . Text.pack
