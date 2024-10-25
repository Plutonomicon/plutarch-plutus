module Plutarch.Test.Golden (
  plutarchGolden,
  goldenGroup,
  goldenEval,
  goldenAssertEqual,
  goldenAssertFail,
) where

import Data.Aeson (ToJSON (toEncoding, toJSON), encode, object, pairs, (.=))
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Short qualified as Short
import Data.Int (Int64)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Plutarch (ClosedTerm, Config (Tracing), LogLevel (LogInfo), Script, TracingMode (DetTracing), compile)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Internal.Other (printScript)
import Plutarch.Script (Script (unScript))
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V1 (ExBudget (ExBudget), ExCPU, ExMemory)
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest, testFailed, testPassed)

data GoldenTestTree where
  GoldenTestTree :: TestName -> [GoldenTestTree] -> GoldenTestTree
  GoldenTestTreeEval :: TestName -> ClosedTerm a -> GoldenTestTree
  GoldenTestTreeEvalFail :: TestName -> ClosedTerm a -> GoldenTestTree
  GoldenTestTreeEvalAssertEqual :: TestName -> ClosedTerm a -> ClosedTerm a -> GoldenTestTree

plutarchGolden :: TestName -> FilePath -> [GoldenTestTree] -> TestTree
plutarchGolden topName goldenPath tests = testGroup topName testsWithGoldens
  where
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

goldenGroup :: TestName -> [GoldenTestTree] -> GoldenTestTree
goldenGroup = GoldenTestTree

goldenEval :: TestName -> ClosedTerm a -> GoldenTestTree
goldenEval = GoldenTestTreeEval

goldenAssertEqual :: TestName -> ClosedTerm a -> ClosedTerm a -> GoldenTestTree
goldenAssertEqual = GoldenTestTreeEvalAssertEqual

goldenAssertFail :: TestName -> ClosedTerm a -> GoldenTestTree
goldenAssertFail = GoldenTestTreeEvalFail

mkFailed :: TestName -> (e -> String) -> Either e a -> Either (TestTree, [(TestName, Benchmark)]) a
mkFailed name showErr = either (Left . (,[]) . singleTest name . FailedTest . showErr) Right

mkTest :: GoldenTestTree -> (TestTree, [(TestName, Benchmark)])
mkTest (GoldenTestTree name tests) = (testGroup name tests', benchmarks)
  where
    (tests', benchmarks') = unzip $ map mkTest tests
    benchmarks = foldMap (map (first ((name <> ".") <>))) benchmarks'
mkTest (GoldenTestTreeEval name term) = either id id $ do
  benchmark <- mkFailed name Text.unpack $ benchmarkTerm term
  _ <- mkFailed name show $ result benchmark
  pure (singleTest name PassedTest, [(name, benchmark)])
mkTest (GoldenTestTreeEvalFail name term) = either id id $ do
  benchmark <- mkFailed name Text.unpack $ benchmarkTerm term
  pure $ case result benchmark of
    Left _ -> (singleTest name PassedTest, [(name, benchmark)])
    Right _ ->
      ( singleTest name $ FailedTest "Script did not terminate with error as expected"
      , [(name, benchmark)]
      )
mkTest (GoldenTestTreeEvalAssertEqual name term expected) = either id id $ do
  termBenchmark <- mkFailed name Text.unpack $ benchmarkTerm term
  expectedBenchmark <- mkFailed name Text.unpack $ benchmarkTerm expected
  actual <- mkFailed name show $ result termBenchmark
  expected <- mkFailed name show $ result expectedBenchmark
  pure
    ( singleTest name $ ScriptEqualTest actual expected
    , [(name, termBenchmark)]
    )

data PassedTest = PassedTest

instance IsTest PassedTest where
  testOptions = Tagged []
  run _ _ _ = pure $ testPassed ""

newtype FailedTest = FailedTest String

instance IsTest FailedTest where
  testOptions = Tagged []
  run _ (FailedTest msg) _ = pure $ testFailed msg

data ScriptEqualTest = ScriptEqualTest Script Script

instance IsTest ScriptEqualTest where
  testOptions = Tagged []
  run _ (ScriptEqualTest actualScript expectedScript) _ =
    if actualScript' == expectedScript'
      then pure $ testPassed ""
      else
        pure $
          testFailed $
            unlines
              [ "Expected: " <> expectedScript'
              , "     Got: " <> actualScript'
              ]
    where
      actualScript' = printScript actualScript
      expectedScript' = printScript expectedScript

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
            [ Char8.pack testName
            , " "
            , go benchmark
            ]
      )

mkBenchGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkBenchGoldenValue = mkBenchmarkValue (encode . PerfBenchmark)

mkUplcEvalGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkUplcEvalGoldenValue = mkBenchmarkValue (either (const "program 1.0.0 error") (Char8.pack . printScript) . result)

mkUplcGoldenValue :: [(TestName, Benchmark)] -> ByteString
mkUplcGoldenValue = mkBenchmarkValue (Char8.pack . printScript . unevaluated)
