{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Test.Golden (
  -- * DSL
  pgoldenSpec,
  pgoldenSpec',
  (@|),
  (@\),
  (@->),
  (@:->),
  TermExpectation,
  PlutarchGoldens,

  -- * Golden key and path
  GoldenKey,
  currentGoldenKey,
  goldenKeyString,
  mkGoldenKeyFromSpecPath,
  defaultGoldenBasePath,
  goldenTestPath,

  -- * Golden config
  GoldenConf (..),
  GoldenTest (..),

  -- * Evaluation
  evalScriptAlwaysWithBenchmark,
  compileD,
) where

import Control.Monad (forM_, unless)
import Data.Aeson.Text qualified as Aeson
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))
import Test.Hspec.Golden

import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as S
import Plutarch (Config (Config, tracingMode), compile, printScript, pattern DetTracing)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Script qualified as Scripts
import Plutarch.Test.Benchmark (Benchmark, mkBenchmark, scriptSize)
import Plutarch.Test.ListSyntax (ListSyntax, listSyntaxAdd, listSyntaxAddSubList, runListSyntax)
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Core.Spec (SpecM, getSpecDescriptionPath)

data GoldenValue = GoldenValue
  { goldenValueUplcPreEval :: Text
  -- ^ Golden string for UPLC
  , goldenValueUplcPostEval :: Text
  -- ^ Golden string for evaluated UPLC
  , goldenValueBench :: Text
  -- ^ Golden string for benchmark JSON
  , goldenValueEvaluated :: Script
  -- ^ Evaluated result
  , goldenValueBenchVal :: Benchmark
  -- ^ `Benchmark` for evaluated UPLC
  , goldenValueExpectation :: Maybe (Script -> Benchmark -> Expectation)
  -- ^ User test's expectation function
  }

data GoldenConf = GoldenConf
  { chosenTests :: Set GoldenTest
  , goldenBasePath :: FilePath
  -- ^ Directory to put the goldens in.
  }
  deriving stock (Eq, Show)

instance Default GoldenConf where
  def = GoldenConf (S.fromList [minBound .. maxBound]) defaultGoldenBasePath

{- | Class of types that represent `GoldenValue`

  This class exists for syntactic sugar provided by (@->) (via `TermExpectation`).
-}
class HasGoldenValue (t :: S -> PType -> Type) where
  mkGoldenValue :: forall a. (forall s. t s a) -> GoldenValue

mkGoldenValue' :: ClosedTerm a -> Maybe (Script -> Benchmark -> Expectation) -> GoldenValue
mkGoldenValue' p mexp =
  let compiledScript = compileD p
      (evaluatedScript, bench) = evalScriptAlwaysWithBenchmark compiledScript
   in GoldenValue
        (T.pack $ printScript compiledScript)
        (T.pack $ printScript evaluatedScript)
        (TL.toStrict $ Aeson.encodeToLazyText bench)
        evaluatedScript
        bench
        mexp

-- We derive for `Term s a` only because GHC prevents us from deriving for
-- `ClosedTerm a`. In practice, this instance should be used only for closed
-- terms.
instance HasGoldenValue Term where
  mkGoldenValue p = mkGoldenValue' p Nothing

{- | A `Term` paired with its evaluation/benchmark expectation

  Example:
  >>> TermExpectation (pcon PTrue) $ \(p, _script, _benchmark) -> pshouldBe (pcon PTrue)
-}
data TermExpectation' (s :: S) a = TermExpectation (ClosedTerm a) ((ClosedTerm a, Script, Benchmark) -> Expectation)

type TermExpectation a = forall s. TermExpectation' s a

-- | Test an expectation on a golden Plutarch program
(@->) :: ClosedTerm a -> (ClosedTerm a -> Expectation) -> TermExpectation a
(@->) p f = p @:-> \(p', _, _) -> f p'

infixr 1 @->

{- | Like `@->` but also takes the evaluated script and benchmark as arguments

  Useful to do assertion checks on post-evaluation benchmark (eg: to check if
  script size is below certain threshold) -- use in conjunction with
  `psatisfyWithinBenchmark` -- or on evaluated script (ie., without
  re-evaluating the program).
-}
(@:->) :: ClosedTerm a -> ((ClosedTerm a, Script, Benchmark) -> Expectation) -> TermExpectation a
(@:->) p f = TermExpectation p (\(p', pe, b) -> f (p', pe, b))

infixr 1 @:->

instance HasGoldenValue TermExpectation' where
  mkGoldenValue (TermExpectation p f) =
    mkGoldenValue' p (Just $ \pe b -> f (p, pe, b))

-- | The key used in the .golden files containing multiple golden values
newtype GoldenKey = GoldenKey Text
  deriving newtype (Eq, Show, Ord, IsString)

goldenKeyString :: GoldenKey -> String
goldenKeyString (GoldenKey s) = T.unpack s

instance Semigroup GoldenKey where
  GoldenKey s1 <> GoldenKey s2 = GoldenKey $ s1 <> "." <> s2

currentGoldenKey :: HasCallStack => SpecM () GoldenKey
currentGoldenKey = do
  mkGoldenKeyFromSpecPath . fmap T.pack <$> getSpecDescriptionPath

mkGoldenKeyFromSpecPath :: HasCallStack => [Text] -> GoldenKey
mkGoldenKeyFromSpecPath path =
  case nonEmpty path of
    Nothing -> error "cannot use currentGoldenKey from top-level spec"
    Just anc ->
      -- hspec-discover adds a top-level entry; remove that.
      case nonEmpty (NE.drop 1 anc) of
        Nothing -> error "cannot use currentGoldenKey from top-level spec (after hspec-discover)"
        Just path ->
          sconcat $ fmap GoldenKey path

goldenPath :: FilePath -> GoldenKey -> FilePath
goldenPath baseDir (GoldenKey k) =
  baseDir </> T.unpack k <> ".golden"

type PlutarchGoldens = ListSyntax (GoldenKey, GoldenValue)

-- | Specify goldens for the given Plutarch program
(@|) :: forall t a. HasGoldenValue t => GoldenKey -> (forall s. t s a) -> PlutarchGoldens
(@|) k v = listSyntaxAdd (k, mkGoldenValue v)

infixr 0 @|

-- | Add an expectation for the Plutarch program specified with (@|)
(@\) :: GoldenKey -> PlutarchGoldens -> PlutarchGoldens
(@\) = listSyntaxAddSubList

{- | Create golden specs for pre/post-eval UPLC and benchmarks.

  A *single* golden file will be created (for each metric) for all the programs
  in the given tree.

  For example,
  ```
  pgoldenSpec $ do
    "foo" @| pconstant 42
    "bar" @\ do
      "qux" @| pconstant "Hello"
  ```

  Will create three golden files -- uplc.golden, uplc.eval.golden and
  bench.golden -- each containing three lines one for each program above.
  Hierarchy is represented by intercalating with a dot; for instance, the key
  for 'qux' will be "bar.qux".
-}
pgoldenSpec :: HasCallStack => PlutarchGoldens -> Spec
pgoldenSpec = pgoldenSpec' def

{- | Like 'pgoldenSpec' but takes a 'GoldenConf' to determine which goldens to track.

> pgoldenSpec = pgoldenSpec' def
-}
pgoldenSpec' :: HasCallStack => GoldenConf -> PlutarchGoldens -> Spec
pgoldenSpec' conf@(GoldenConf {goldenBasePath}) m = do
  base <- currentGoldenKey
  let bs = runListSyntax m
  -- Golden tests
  describe "golden" $ do
    goldenTestSpec goldenBasePath base bs `mapM_` chosenTests conf
  -- Assertion tests (if any)
  let asserts = flip mapMaybe bs $ \(k, v) -> do
        (k,) . (\f -> f (goldenValueEvaluated v) $ goldenValueBenchVal v) <$> goldenValueExpectation v
  unless (null asserts) $ do
    forM_ asserts $ \(k, v) ->
      it (goldenKeyString $ "<golden>" <> k <> "assert") v

data GoldenTest
  = -- | The unevaluated UPLC (compiled target of Plutarch term)
    GoldenT'UPLCPreEval
  | -- | The evaluated UPLC (evaluated result of Plutarch term)
    GoldenT'UPLCPostEval
  | -- | Benchmark of Plutarch term (will never fail)
    GoldenT'Bench
  deriving stock (Eq, Show, Ord, Enum, Bounded)

goldenTestKey :: GoldenTest -> GoldenKey
goldenTestKey = \case
  GoldenT'UPLCPreEval -> "uplc"
  GoldenT'UPLCPostEval -> "uplc.eval"
  GoldenT'Bench -> "bench"

defaultGoldenBasePath :: FilePath
defaultGoldenBasePath = "goldens"

goldenTestPath :: FilePath -> GoldenKey -> GoldenTest -> FilePath
goldenTestPath goldenBasePath base gt =
  goldenPath goldenBasePath $ base <> goldenTestKey gt

goldenTestVal :: GoldenTest -> GoldenValue -> Text
goldenTestVal t v = case t of
  GoldenT'UPLCPreEval -> goldenValueUplcPreEval v
  GoldenT'UPLCPostEval -> goldenValueUplcPostEval v
  GoldenT'Bench -> goldenValueBench v

goldenTestSpec :: FilePath -> GoldenKey -> [(GoldenKey, GoldenValue)] -> GoldenTest -> Spec
goldenTestSpec goldenBasePath base vals gt = do
  it (goldenKeyString $ goldenTestKey gt) $ do
    Golden
      { output = combineGoldens $ fmap (goldenTestVal gt) <$> vals
      , goldenFile = goldenTestPath goldenBasePath base gt
      , actualFile = Nothing
      , encodePretty = show
      , writeToFile = TIO.writeFile
      , readFromFile = TIO.readFile
      , failFirstTime = False
      }
  where
    -- Group multiple goldens values in the same file
    combineGoldens :: [(GoldenKey, Text)] -> Text
    combineGoldens xs =
      T.intercalate "\n" $
        (\(GoldenKey k, v) -> k <> " " <> v) <$> xs

{- | Like `evalScript` but doesn't throw `EvalError`, and returns `Benchmark`.

  On `EvalError`, this function returns `perror` as evaluated script. Plutus
  does not provide an accurate way to tell if the program evaluates to `Error` or
  not; see https://github.com/input-output-hk/plutus/issues/4270
-}
evalScriptAlwaysWithBenchmark :: Scripts.Script -> (Scripts.Script, Benchmark)
evalScriptAlwaysWithBenchmark script =
  let (res, exbudget, _traces) = evalScript script
      bench = mkBenchmark exbudget (scriptSize script)
   in ( case res of
          Left _ -> either (error "not supposed to fail") id $ compile (Config {tracingMode = DetTracing}) perror
          Right x -> x
      , bench
      )

compileD :: ClosedTerm a -> Scripts.Script
compileD t = either (error . T.unpack) id $ compile (Config {tracingMode = DetTracing}) t
