{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Test.Golden (
  -- * DSL
  pgoldenSpec,
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

  -- * Evaluation
  evalScriptAlwaysWithBenchmark,
  compileD,
) where

import Control.Monad (forM_, unless)
import qualified Data.Aeson.Text as Aeson
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))
import Test.Hspec.Golden

import qualified Data.List.NonEmpty as NE
import Plutarch (compile, printScript)
import Plutarch.Evaluate (evalScript)
import Plutarch.Internal (punsafeAsClosedTerm)
import Plutarch.Prelude
import Plutarch.Test.Benchmark (Benchmark, mkBenchmark, scriptSize)
import Plutarch.Test.ListSyntax (ListSyntax, listSyntaxAdd, listSyntaxAddSubList, runListSyntax)
import Plutus.V1.Ledger.Scripts (Script)
import qualified Plutus.V1.Ledger.Scripts as Scripts
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

{- | Class of types that represent `GoldenValue`

  This class exists for syntatic sugar provided by (@->) (via `TermExpectation`).
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
  mkGoldenValue p = mkGoldenValue' (punsafeAsClosedTerm p) Nothing

{- | A `Term` paired with its evaluation/benchmark expectation

  Example:
  >>> TermExpectation (pcon PTrue) $ \(p, _script, _benchmark) -> pshouldBe (pcon PTrue)
-}
data TermExpectation' s a = TermExpectation (Term s a) ((Term s a, Script, Benchmark) -> Expectation)

type TermExpectation a = forall s. TermExpectation' s a

-- | Test an expectation on a golden Plutarch program
(@->) :: ClosedTerm a -> (ClosedTerm a -> Expectation) -> TermExpectation a
(@->) p f = p @:-> \(p', _, _) -> f $ punsafeAsClosedTerm p'

infixr 1 @->

{- | Like `@->` but also takes the evaluated script and benchmark as arguments

  Useful to do assertion checks on post-evaluation benchmark (eg: to check if
  script size is below certain threshold) -- use in conjunction with
  `psatisfyWithinBenchmark` -- or on evaluated script (ie., without
  re-evaluating the program).
-}
(@:->) :: ClosedTerm a -> ((ClosedTerm a, Script, Benchmark) -> Expectation) -> TermExpectation a
(@:->) p f = TermExpectation p (\(p', pe, b) -> f (punsafeAsClosedTerm p', pe, b))

infixr 1 @:->

instance HasGoldenValue TermExpectation' where
  mkGoldenValue (TermExpectation p f) =
    mkGoldenValue' (punsafeAsClosedTerm p) (Just $ (\pe b -> f (p, pe, b)))

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
      case nonEmpty (NE.drop 1 . NE.reverse $ anc) of
        Nothing -> error "cannot use currentGoldenKey from top-level spec (after sydtest-discover)"
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
pgoldenSpec map = do
  base <- currentGoldenKey
  let bs = runListSyntax map
  -- Golden tests
  describe "golden" $ do
    goldenTestSpec base bs `mapM_` [minBound .. maxBound]
  -- Assertion tests (if any)
  let asserts = flip mapMaybe bs $ \(k, v) -> do
        (k,) . (\f -> f (goldenValueEvaluated v) $ goldenValueBenchVal v) <$> goldenValueExpectation v
  unless (null asserts) $ do
    forM_ asserts $ \(k, v) ->
      it (goldenKeyString $ "<golden>" <> k <> "assert") v

data GoldenTest
  = -- | The unevaluated UPLC (compiled target of Plutarch term)
    UPLCPreEval
  | -- | The evaluated UPLC (evaluated result of Plutarch term)
    UPLCPostEval
  | -- | Benchmark of Plutarch term (will never fail)
    Bench
  deriving stock (Eq, Show, Ord, Enum, Bounded)

goldenTestKey :: GoldenTest -> GoldenKey
goldenTestKey = \case
  UPLCPreEval -> "uplc"
  UPLCPostEval -> "uplc.eval"
  Bench -> "bench"

defaultGoldenBasePath :: FilePath
defaultGoldenBasePath = "goldens"

goldenTestPath :: GoldenKey -> GoldenTest -> FilePath
goldenTestPath base gt =
  goldenPath defaultGoldenBasePath $ base <> goldenTestKey gt

goldenTestVal :: GoldenTest -> GoldenValue -> Text
goldenTestVal t v = case t of
  UPLCPreEval -> goldenValueUplcPreEval v
  UPLCPostEval -> goldenValueUplcPostEval v
  Bench -> goldenValueBench v

goldenTestSpec :: GoldenKey -> [(GoldenKey, GoldenValue)] -> GoldenTest -> Spec
goldenTestSpec base vals gt = do
  it (goldenKeyString $ goldenTestKey gt) $ do
    Golden
      { output = combineGoldens $ fmap (goldenTestVal gt) <$> vals
      , goldenFile = goldenTestPath base gt
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
  does not provide an accurate way to tell if the program evalutes to `Error` or
  not; see https://github.com/input-output-hk/plutus/issues/4270
-}
evalScriptAlwaysWithBenchmark :: Scripts.Script -> (Scripts.Script, Benchmark)
evalScriptAlwaysWithBenchmark script =
  let (res, exbudget, _traces) = evalScript script
      bench = mkBenchmark exbudget (scriptSize script)
   in ( case res of
        Left _ -> compile perror
        Right x -> x
      , bench
      )

-- TODO: Make this deterministic
-- See https://github.com/Plutonomicon/plutarch/pull/297
compileD :: ClosedTerm a -> Scripts.Script
compileD = compile
