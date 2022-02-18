module Plutarch.Test.Golden (
  goldenSpec,
  (#>),
  (#\),
) where

import qualified Data.Aeson.Text as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import Test.Syd (
  Spec,
  TestDefM,
  describe,
  getTestDescriptionPath,
  it,
  pureGoldenTextFile,
 )

import Data.Kind (Type)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import Data.String (IsString)
import GHC.Stack (HasCallStack)
import Plutarch
import Plutarch.Benchmark (benchmarkScript')
import Plutarch.Test.Deterministic (compileD, evaluateScriptAlways)
import Plutarch.Test.ListSyntax (ListSyntax, listSyntaxAdd, listSyntaxAddSubList, runListSyntax)

data GoldenValue = GoldenValue
  { goldenValueUplcPreEval :: Text
  , goldenValueUplcPostEval :: Text
  , goldenValueBench :: Text
  }
  deriving stock (Eq)

mkGoldenValue :: ClosedTerm a -> GoldenValue
mkGoldenValue p =
  GoldenValue
    (T.pack $ printScript $ compileD p)
    (T.pack $ printScript $ evaluateScriptAlways $ compileD p)
    (TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compileD p)

{- The key used in the .golden files containing multiple golden values -}
newtype GoldenKey = GoldenKey Text
  deriving newtype (Eq, Show, Ord, IsString)

instance Semigroup GoldenKey where
  GoldenKey s1 <> GoldenKey s2 = GoldenKey $ s1 <> "." <> s2

goldenPath :: FilePath -> GoldenKey -> FilePath
goldenPath baseDir (GoldenKey k) =
  baseDir </> T.unpack k <> ".golden"

{- Group multiple goldens values in the same file -}
combineGoldens :: [(GoldenKey, Text)] -> Text
combineGoldens xs =
  T.intercalate "\n" $
    (\(GoldenKey k, v) -> k <> " " <> v) <$> xs

(#>) :: GoldenKey -> ClosedTerm a -> ListSyntax (GoldenKey, GoldenValue)
(#>) k v = listSyntaxAdd (k, mkGoldenValue v)
infixr 0 #>

(#\) :: GoldenKey -> ListSyntax (GoldenKey, GoldenValue) -> ListSyntax (GoldenKey, GoldenValue)
(#\) = listSyntaxAddSubList

goldenSpec :: HasCallStack => ListSyntax (GoldenKey, GoldenValue) -> Spec
goldenSpec map = do
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

currentGoldenKey :: HasCallStack => forall (outers :: [Type]) inner. TestDefM outers inner GoldenKey
currentGoldenKey = do
  fmap nonEmpty getTestDescriptionPath >>= \case
    Nothing -> error "cannot use currentGoldenKey from top-level spec"
    Just anc ->
      case nonEmpty (NE.drop 1 . NE.reverse $ anc) of
        Nothing -> error "cannot use currentGoldenKey from top-level spec (after sydtest-discover)"
        Just path ->
          pure $ sconcat $ fmap GoldenKey path
