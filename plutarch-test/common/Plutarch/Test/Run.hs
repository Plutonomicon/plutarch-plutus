module Plutarch.Test.Run (runPlutarchSpec) where

import Plutarch.Test.TrailSpecMonad (TrailSpec, runTrailSpec)
import Test.Hspec (hspec)

-- | Like `sydTest`, but ensures that there are no unused goldens left behind.
runPlutarchSpec :: TrailSpec -> IO ()
runPlutarchSpec spec = do
  -- TODO: unsused goldens
  -- TODO: use hedgehog runner via hspec-hedgehog
  hspec $ runTrailSpec spec

{-
  usedGoldens <- goldenPathsUsedBy <$> sydTest' spec
  unusedGoldens usedGoldens >>= \case
    [] -> pure ()
    unused -> do
      putStrLn "ERROR: Unused golden files found lying around! Namely:"
      forM_ unused $ \fp ->
        putStrLn $ "- " <> fp
      exitWith (ExitFailure 1)

-- | Given a list of "used" goldens, return any unused golden files on disk.
unusedGoldens :: [FilePath] -> IO [FilePath]
unusedGoldens usedGoldens' = do
  let usedGoldens = foldMap knownGoldens usedGoldens'
  allGoldens <- Set.fromList . fmap (defaultGoldenBasePath </>) <$> listDirectory defaultGoldenBasePath
  pure $ Set.toList $ allGoldens `Set.difference` usedGoldens
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

goldenPathsUsedBy :: [SpecTree a] -> [FilePath]
goldenPathsUsedBy trees = do
  flip foldMap (queryGoldens trees) $ \k ->
    flip fmap [minBound .. maxBound] $ \t ->
      goldenTestPath k t

-- | Retrieve all golden keys used by the given test tree.
queryGoldens :: [SpecTree a] -> [GoldenKey]
queryGoldens =
  fmap mkGoldenKeyFromSpecPath . concatMap (go [])
  where
    go ancestors = \case
      DescribeNode "golden" _children ->
        ancestors : []
      DescribeNode k children ->
        concatMap (go $ k : ancestors) children
      SubForestNode trees ->
        concatMap (go ancestors) trees
      SpecifyNode _ _ ->
        mempty
      PendingNode _ _ ->
        mempty

-- | Like `sydTest` but returns the test tree.
sydTest' :: Spec -> IO [SpecTree ()]
sydTest' spec = do
  config <- getSettings
  resultForest <- timedValue <$> sydTestResult config spec
  if shouldExitFail config resultForest
    then exitWith $ ExitFailure 1
    else pure $ void <$> resultForest

      -}
