module Plutarch.Test.Golden (
  pgoldenSpec,
  (@|),
  (@\),
  (@->),
  TermExpectation,
  PlutarchGoldens,
  goldenKeyString,
  evalScriptAlwaysWithBenchmark,
  compileD,
) where

import Control.Monad (forM_, unless)
import qualified Data.Aeson.Text as Aeson
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))
import Test.Syd (
  Expectation,
  Spec,
  TestDefM,
  describe,
  getTestDescriptionPath,
  it,
  pureGoldenTextFile,
 )

import Plutarch (ClosedTerm, compile, printScript)
import Plutarch.Evaluate (evalScript)
import Plutarch.Internal (punsafeAsClosedTerm)
import Plutarch.Prelude
import Plutarch.Test.Benchmark (Benchmark, mkBenchmark, scriptSize)
import Plutarch.Test.ListSyntax (ListSyntax, listSyntaxAdd, listSyntaxAddSubList, runListSyntax)
import qualified Plutus.V1.Ledger.Scripts as Scripts

data GoldenValue = GoldenValue
  { goldenValueUplcPreEval :: Text
  , goldenValueUplcPostEval :: Text
  , goldenValueBench :: Text
  , goldenValueExpectation :: Maybe Expectation
  }

{- | Class of types that represent `GoldenValue`

  This class exists for syntatic sugar provided by (@->) (via `TermExpectation`).
-}
class HasGoldenValue (t :: S -> PType -> Type) where
  mkGoldenValue :: forall a. (forall s. t s a) -> GoldenValue

mkGoldenValue' :: ClosedTerm a -> Maybe Expectation -> GoldenValue
mkGoldenValue' p mexp =
  let compiledScript = compileD p
      (evaluatedScript, bench) = evalScriptAlwaysWithBenchmark compiledScript
   in GoldenValue
        (T.pack $ printScript compiledScript)
        (T.pack $ printScript evaluatedScript)
        (TL.toStrict $ Aeson.encodeToLazyText bench)
        mexp

-- We derive for `Term s a` only because GHC prevents us from deriving for
-- `ClosedTerm a`. In practice, this instance should be used only for closed
-- terms.
instance HasGoldenValue Term where
  mkGoldenValue p = mkGoldenValue' (punsafeAsClosedTerm p) Nothing

{- | A `Term` paired with its evaluation expectation

  Example:
  >>> TermExpectation (pcon PTrue) $ \p -> pshouldBe (pcon PTrue)
-}
data TermExpectation' s a = TermExpectation (Term s a) (Term s a -> Expectation)

type TermExpectation a = forall s. TermExpectation' s a

-- | Test an expectation on a golden Plutarch program
(@->) :: ClosedTerm a -> (ClosedTerm a -> Expectation) -> TermExpectation a
(@->) p f = TermExpectation p (\p' -> f $ punsafeAsClosedTerm p')

infixr 1 @->

instance HasGoldenValue TermExpectation' where
  mkGoldenValue (TermExpectation p f) = mkGoldenValue' (punsafeAsClosedTerm p) (Just $ f p)

-- | The key used in the .golden files containing multiple golden values
newtype GoldenKey = GoldenKey Text
  deriving newtype (Eq, Show, Ord, IsString)

goldenKeyString :: GoldenKey -> String
goldenKeyString (GoldenKey s) = T.unpack s

instance Semigroup GoldenKey where
  GoldenKey s1 <> GoldenKey s2 = GoldenKey $ s1 <> "." <> s2

goldenPath :: FilePath -> GoldenKey -> FilePath
goldenPath baseDir (GoldenKey k) =
  baseDir </> T.unpack k <> ".golden"

-- | Group multiple goldens values in the same file
combineGoldens :: [(GoldenKey, Text)] -> Text
combineGoldens xs =
  T.intercalate "\n" $
    (\(GoldenKey k, v) -> k <> " " <> v) <$> xs

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
pgoldenSpec :: HasCallStack => ListSyntax (GoldenKey, GoldenValue) -> Spec
pgoldenSpec map = do
  name <- currentGoldenKey
  let bs = runListSyntax map
      goldenPathWith k = goldenPath "goldens" $ name <> k
  describe "golden" $ do
    it "uplc" $
      pureGoldenTextFile (goldenPathWith "uplc") $
        combineGoldens $ fmap goldenValueUplcPreEval <$> bs
    it "uplc.eval" $
      pureGoldenTextFile (goldenPathWith "uplc.eval") $
        combineGoldens $ fmap goldenValueUplcPostEval <$> bs
    it "bench" $
      pureGoldenTextFile (goldenPathWith "bench") $
        combineGoldens $ fmap goldenValueBench <$> bs
  let asserts = flip mapMaybe bs $ \(k, b) -> do
        (k,) <$> goldenValueExpectation b
  unless (null asserts) $ do
    forM_ asserts $ \(k, v) ->
      it (goldenKeyString $ "<golden>" <> k <> "assert") v

currentGoldenKey :: HasCallStack => forall (outers :: [Type]) inner. TestDefM outers inner GoldenKey
currentGoldenKey = do
  fmap nonEmpty getTestDescriptionPath >>= \case
    Nothing -> error "cannot use currentGoldenKey from top-level spec"
    Just anc ->
      case nonEmpty (NE.drop 1 . NE.reverse $ anc) of
        Nothing -> error "cannot use currentGoldenKey from top-level spec (after sydtest-discover)"
        Just path ->
          pure $ sconcat $ fmap GoldenKey path

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
