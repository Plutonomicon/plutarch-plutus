module Plutarch.Test.Run (
  noUnusedGoldens,
  noUnusedGoldens',
  hspecAndReturnForest,
) where

import Control.Monad (forM_)
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Plutarch.Test.Golden (
  GoldenConf (GoldenConf, chosenTests, goldenBasePath),
  GoldenKey,
  goldenTestPath,
  mkGoldenKeyFromSpecPath,
 )
import System.Directory (listDirectory)
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath ((</>))
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (defaultConfig, evalSpec, evaluateResult, readConfig, runSpecForest)
import Test.Hspec.Core.Spec (SpecTree, Tree (Leaf, Node, NodeWithCleanup))

{- | Like `hspec`,  but returns the test forest after running the tests.

  Based on https://github.com/hspec/hspec/issues/649#issuecomment-1092423220
-}
hspecAndReturnForest :: Spec -> IO [SpecTree ()]
hspecAndReturnForest spec0 = do
  (config, spec) <- evalSpec defaultConfig spec0
  getArgs
    >>= readConfig config
    >>= withArgs [] . runSpecForest spec
    >>= evaluateResult
  pure spec

{- | Ensures that there are no unused goldens left behind.

  Use this on any `SpecTree` that internally uses `pgoldenSpec` to define the
  golden tests. These golden file paths are accumulated, and compared to the
  actual files existing on disk. If any golden file exists on disk, but is not
  tracked by the `SpecTree` this function will fail, reporting the list of
  untracked golden files.

  __Example:__

  @
  noUnusedGoldens =<< hspecAndReturnForest spec
  @
-}
noUnusedGoldens :: [SpecTree ()] -> IO ()
noUnusedGoldens = noUnusedGoldens' def

{- | Like 'noUnusedGoldens' but takes a custom path to the golden storage.

  NOTE: This relies on the assumption that the same 'GoldenConf' is used in all
'pgoldenSpec'' calls. This function will go away after
https://github.com/Plutonomicon/plutarch/issues/458
-}
noUnusedGoldens' :: GoldenConf -> [SpecTree ()] -> IO ()
noUnusedGoldens' conf@(GoldenConf {goldenBasePath}) specForest = do
  -- A second traversal here (`runSpecM`) can be obviated after
  -- https://github.com/hspec/hspec/issues/649
  let usedGoldens = goldenPathsUsedBy conf specForest
  unusedGoldens goldenBasePath usedGoldens >>= \case
    [] -> pure ()
    unused -> do
      putStrLn "ERROR: Unused golden files found lying around! Namely:"
      forM_ unused $ \fp ->
        putStrLn $ "- " <> fp
      exitWith (ExitFailure 1)

-- | Given a list of "used" goldens, return any unused golden files on disk.
unusedGoldens :: FilePath -> [FilePath] -> IO [FilePath]
unusedGoldens goldenBasePath usedGoldens' = do
  let usedGoldens = foldMap knownGoldens usedGoldens'
  allGoldens <- Set.fromList . fmap (goldenBasePath </>) <$> listDirectory goldenBasePath
  pure $
    Set.toList $
      Set.filter (not . isPrefixOf (goldenBasePath </> "FFI.")) $
        allGoldens `Set.difference` usedGoldens
  where
    knownGoldens :: FilePath -> Set FilePath
    knownGoldens fp =
      Set.fromList
        [ fp
        , -- Inject goldens for other flag values to be comprehensive in our
          -- search.
          replace "dev=true" "dev=false" fp
        , replace "dev=false" "dev=true" fp
        ]
    replace a b = T.unpack . T.replace a b . T.pack

goldenPathsUsedBy :: GoldenConf -> [SpecTree a] -> [FilePath]
goldenPathsUsedBy (GoldenConf {chosenTests, goldenBasePath}) trees = do
  flip foldMap (queryGoldens trees) $ \k ->
    flip fmap (Set.toList chosenTests) $ \t ->
      goldenTestPath goldenBasePath k t

-- | Retrieve all golden keys used by the given test tree.
queryGoldens :: [SpecTree a] -> [GoldenKey]
queryGoldens =
  -- `drop 1`, to drop the hspec-discover generated root node.
  fmap mkGoldenKeyFromSpecPath . concatMap (go [])
  where
    go ancestors = \case
      Node "golden" _children ->
        [ancestors]
      Node k children ->
        concatMap (go $ T.pack k : ancestors) children
      NodeWithCleanup _ _ trees ->
        concatMap (go ancestors) trees
      Leaf _ ->
        mempty
