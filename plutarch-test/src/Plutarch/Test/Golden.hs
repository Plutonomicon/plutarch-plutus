{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Test.Golden (
  pgoldenSpec,
  (@|),
  (@\),
  (@->),
  TermExpectation,
  goldenKeyString,
  evaluateScriptAlways,
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
import Plutarch.Benchmark (benchmarkScript')
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Internal (Term (Term, asRawTerm))
import Plutarch.Prelude
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
  GoldenValue
    (T.pack $ printScript $ compileD p)
    (T.pack $ printScript $ evaluateScriptAlways $ compileD p)
    (TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compileD p)
    mexp

-- We derive for `Term s a` only because GHC prevents us from deriving for
-- `ClosedTerm a`. In practice, this instance should be used only for closed
-- terms.
instance HasGoldenValue Term where
  mkGoldenValue p = mkGoldenValue' (unsafeClosedTerm p) Nothing

-- Because, we need a function with this signature.
unsafeClosedTerm :: Term s a -> ClosedTerm a
unsafeClosedTerm t = Term $ asRawTerm t

{- | A `Term` paired with its evaluation expectation

  Example:
  >>> TermExpectation (pcon PTrue) $ \p -> pshouldBe (pcon PTrue)
-}
data TermExpectation' s a = TermExpectation (Term s a) (Term s a -> Expectation)

type TermExpectation a = forall s. TermExpectation' s a

-- | Test an expectation on a golden Plutarch program
(@->) :: ClosedTerm a -> (ClosedTerm a -> Expectation) -> TermExpectation a
(@->) p f = TermExpectation p (\p' -> f $ unsafeClosedTerm p')

infixr 1 @->

instance HasGoldenValue TermExpectation' where
  mkGoldenValue (TermExpectation p f) = mkGoldenValue' (unsafeClosedTerm p) (Just $ f p)

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

-- | Specify goldens for the given Plutarch program
(@|) :: forall t a. HasGoldenValue t => GoldenKey -> (forall s. t s a) -> ListSyntax (GoldenKey, GoldenValue)
(@|) k v = listSyntaxAdd (k, mkGoldenValue v)

infixr 0 @|

-- | Add an expectation for the Plutarch program specified with (@|)
(@\) :: GoldenKey -> ListSyntax (GoldenKey, GoldenValue) -> ListSyntax (GoldenKey, GoldenValue)
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

{- | Like `evaluateScript` but doesn't fail. Also returns `Script`.

  All evaluation failures are treated as equivalent to a `perror`. Plutus does
  not provide an accurate way to tell if the program evalutes to `Error` or not;
  see https://github.com/input-output-hk/plutus/issues/4270
-}
evaluateScriptAlways :: Scripts.Script -> Scripts.Script
evaluateScriptAlways script =
  case evaluateScript script of
    Left _ -> compile perror
    Right (_, _, x) -> x

-- TODO: Make this deterministic
-- See https://github.com/Plutonomicon/plutarch/pull/297
compileD :: ClosedTerm a -> Scripts.Script
compileD = compile
