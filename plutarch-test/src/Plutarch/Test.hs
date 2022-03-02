{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific `Expectation` operators
  passert,
  passertNot,
  pfails,
  psucceeds,
  ptraces,
  pshouldBe,
  (#@?=),
  -- | Budget expectation
  psatisfyMaxBudget,

  -- * For Development flag tests
  plutarchDevFlagDescribe,

  -- * Golden testing
  (@|),
  (@\),
  (@->),
  (@:->),
  (@==),
  pgoldenSpec,
  PlutarchGoldens,

  -- * Benchmark type for use in `(@:->)`
  Benchmark (Benchmark, exBudgetCPU, exBudgetMemory, scriptSizeBytes),
  ScriptSizeBytes,

  -- * Deprecated exports
  golden,
  goldens,
  PlutarchGolden (All, Bench, PrintTerm),
  getGoldenFilePrefix,
  goldenFilePath,
) where

import Control.Monad (when)
import qualified Data.Aeson.Text as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import Test.Syd (
  Expectation,
  Spec,
  TestDefM,
  describe,
  expectationFailure,
  getTestDescriptionPath,
  it,
  pureGoldenTextFile,
  shouldBe,
  shouldSatisfyNamed,
 )

import Plutarch
import Plutarch.Bool (PBool (PFalse, PTrue))
import Plutarch.Evaluate (evalScript)
import Plutarch.Test.Benchmark (
  Benchmark (Benchmark, exBudgetCPU, exBudgetMemory, scriptSizeBytes),
  ScriptSizeBytes,
 )
import Plutarch.Test.Golden (
  PlutarchGoldens,
  TermExpectation,
  compileD,
  evalScriptAlwaysWithBenchmark,
  pgoldenSpec,
  (@->),
  (@:->),
  (@\),
  (@|),
 )
import qualified Plutus.V1.Ledger.Scripts as Scripts

{- |
    Like `shouldBe` but but for Plutarch terms
-}
pshouldBe :: ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- fmap printScript $ eval $ compile x
  p2 <- fmap printScript $ eval $ compile y
  p1 `shouldBe` p2
  where
    eval :: Scripts.Script -> IO Scripts.Script
    eval s = case evalScript s of
      (Left e, _, _) -> expectationFailure $ "Script evaluation failed: " <> show e
      (Right x', _, _) -> pure x'

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
  case evalScript (compile p) of
    (Left _, _, _) -> expectationFailure $ "Term failed to evaluate"
    (Right _, _, _) -> pure ()

-- | Asserts the term evaluates without success
pfails :: ClosedTerm a -> Expectation
pfails p = do
  case evalScript (compile p) of
    (Left _, _, _) -> pure ()
    (Right _, _, _) -> expectationFailure $ "Term succeeded"

-- | Expects the benchmark to be within the given max budget
psatisfyMaxBudget :: Benchmark -> Benchmark -> Expectation
psatisfyMaxBudget bench maxBudget = do
  shouldSatisfyNamed bench ("cpu<=" <> show (exBudgetCPU maxBudget)) $ \_ ->
    exBudgetCPU bench <= exBudgetCPU maxBudget
  shouldSatisfyNamed bench ("mem<=" <> show (exBudgetMemory maxBudget)) $ \_ ->
    exBudgetMemory bench <= exBudgetMemory maxBudget
  shouldSatisfyNamed bench ("size<=" <> show (scriptSizeBytes maxBudget)) $ \_ ->
    scriptSizeBytes bench <= scriptSizeBytes maxBudget

{- | Asserts that the term evaluates successfully with the given trace sequence

  See also: `plutarchDevFlagDescribe`
-}
ptraces :: ClosedTerm a -> [Text] -> Expectation
ptraces p develTraces =
  case evalScript (compile p) of
    (Left _, _, _) -> expectationFailure $ "Term failed to evaluate"
    (Right _, _, traceLog) -> do
#ifdef Development 
      traceLog `shouldBe` develTraces
#else
      -- Tracing is disabled in non-developed modes, so we should expect an
      -- empty trace log.
      let noTraces = const [] develTraces
      traceLog `shouldBe` noTraces
#endif

{- | Like `describe`, but determines description from `Development` CPP flag

  Useful to create two sets of othersise identical group of tests that differ
  only by `Development` flag. This has the effect of creating two sets of golden
  tests (with different filepaths) for corresponding flag values.

  Typically meant to be used in conjunction with `ptraces`.
-}
plutarchDevFlagDescribe :: forall (outers :: [Type]) inner. TestDefM outers inner () -> TestDefM outers inner ()

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}
plutarchDevFlagDescribe m =
#ifdef Development 
  describe "dev=true" m
#else
  describe "dev=false" m
#endif
{- ORMOLU_ENABLE -}

-- | Convenient alias for `@-> pshouldBe x`
(@==) :: ClosedTerm a -> ClosedTerm b -> TermExpectation a
(@==) p x = p @-> pshouldBe x

infixr 1 @==

-- TODO: All the code below will be deleted, in favour of Golden.hs.

{- | Whether to run all or a particular golden test

  Typically you want to use `All` -- this produces printTerm and benchmark
  goldens.

  Occasionally you want `PrintTerm` because you don't care to benchmark that
  program.

  Use `Bench` to only benchmark the program.
-}
data PlutarchGolden
  = All
  | Bench
  | PrintTerm
  deriving stock (Eq, Show)

-- | Run golden tests on the given Plutarch program
{-# DEPRECATED golden "Use `pgoldenSpec` instead." #-}
golden :: PlutarchGolden -> ClosedTerm a -> Spec
golden pg p =
  goldens pg [("0", popaque p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
{-# DEPRECATED goldens "Use `pgoldenSpec` instead." #-}
goldens :: PlutarchGolden -> [(String, ClosedTerm a)] -> Spec
goldens pg ps = do
  name <- getGoldenFilePrefix
  describe "golden" $ do
    -- Golden test for UPLC
    when (hasPrintTermGolden pg) $ do
      it "uplc" $
        pureGoldenTextFile (goldenFilePath "goldens" name "uplc") $
          multiGolden ps $ \p ->
            T.pack $ printScript $ compileD p
      it "uplc.eval" $
        pureGoldenTextFile (goldenFilePath "goldens" name "uplc.eval") $
          multiGolden ps $ \p ->
            T.pack $ printScript $ fst $ evalScriptAlwaysWithBenchmark $ compileD p
    -- Golden test for Plutus benchmarks
    when (hasBenchGolden pg) $
      it "bench" $
        pureGoldenTextFile (goldenFilePath "goldens" name "bench") $
          multiGolden ps $ \p ->
            TL.toStrict $ Aeson.encodeToLazyText $ snd $ evalScriptAlwaysWithBenchmark $ compileD p
  where
    hasBenchGolden :: PlutarchGolden -> Bool
    hasBenchGolden = \case
      PrintTerm -> False
      _ -> True
    hasPrintTermGolden :: PlutarchGolden -> Bool
    hasPrintTermGolden = \case
      Bench -> False
      _ -> True

-- | Get a golden filename prefix from the test description path
getGoldenFilePrefix ::
  forall (outers :: [Type]) (inner :: Type).
  TestDefM outers inner String
getGoldenFilePrefix =
  T.unpack . T.intercalate "." . drop 1 . reverse <$> getTestDescriptionPath

-- | Get the golden file name given the basepath, an optional suffix and a name
goldenFilePath :: FilePath -> String -> String -> FilePath
goldenFilePath base name suffix =
  base
    </> (name <> "." <> suffix <> ".golden")

multiGolden :: forall a. [(String, a)] -> (a -> T.Text) -> Text
multiGolden xs f =
  T.intercalate "\n" $
    (\(s, x) -> T.pack s <> " " <> f x) <$> xs
