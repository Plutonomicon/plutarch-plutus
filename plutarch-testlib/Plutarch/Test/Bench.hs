{-# LANGUAGE FlexibleInstances #-}

{- | Plutarch benchmarking tools

Interface mirrors the one from @tasty-bench@ but 'bench' instead of taking @Benchmarkable@
takes 'Plutarch.ClosedTerm'

To compare benchmark run against baseline file you need to generate it first with
@cabal run bench -- --csv baseline.csv@. Then after making modifications you can rerun the
benchmarks to compare with previous values with @cabal run bench -- --baseline baseline.csv@.
You can instruct benchmarks to fail if certain values changed by too much using @--fail-if-*@
flags. See @cabal run bench -- --help@ for all available flags. To regenreate baseline file
run with @--csv@ flag again.
-}
module Plutarch.Test.Bench (
  BenchConfig (Optimizing, NonOptimizing),
  Plutarch.Test.Bench.defaultMain,
  bench,
  benchWithConfig,
  bcompare,
  bcompareWithin,

  -- * CLI options

  -- | Benchmarks can be compared against CSV file and fail if they differ too much.
  -- Run @cabal run bench -- --help@ to see available flags and descriptions.
  -- These options are available by default when running benchmarks in 'Plutarch.Test.Bench.defaultMain'
  BaselinePath,
  CsvPath,
  FailIfMoreCpu,
  FailIfLessCpu,
  FailIfMoreMem,
  FailIfLessMem,
  FailIfBigger,
  FailIfSmaller,

  -- * Ingredients

  -- | These are used by default in 'Plutarch.Test.Bench.defaultMain'.
  -- You do not need to do anything with them unless you build your own benchmark runner
  consoleBenchReporter,
  csvReporter,
) where

import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, guard, unless, when, (>=>))
import Data.Bifunctor (first, second)
import Data.ByteString.Short qualified as Short
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (isNothing)
import Data.Monoid (All (All), Any (Any))
import Data.Proxy (Proxy (Proxy))
import Data.SatInt (fromSatInt)
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import GHC.Conc (TVar, atomically, forkIO, newTVarIO, readTVar, retry, writeTVar)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (Config (NoTracing), compile)
import Plutarch.Evaluate (evalScriptUnlimited)
import Plutarch.Internal (compileOptimized)
import Plutarch.Prelude
import Plutarch.Script (Script (unScript))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusLedgerApi.Common (serialiseUPLC)
import System.Exit (exitFailure, exitSuccess)
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  IOMode (WriteMode),
  hClose,
  hPutStrLn,
  hSetBuffering,
  openFile,
  stderr,
 )
import Test.Tasty
import Test.Tasty.Ingredients (Ingredient (TestReporter), composeReporters, tryIngredients)
import Test.Tasty.Ingredients.ConsoleReporter (MinDurationToReport (MinDurationToReport))
import Test.Tasty.Options (
  IsOption (defaultValue, optionHelp, optionName, parseValue),
  OptionDescription (Option),
  OptionSet,
  lookupOption,
  safeRead,
  setOption,
 )
import Test.Tasty.Patterns.Eval (asB, eval, withFields)
import Test.Tasty.Patterns.Types (Expr (And, StringLit))
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest, testFailed, testPassed)
import Test.Tasty.Runners (
  Ap (Ap, getApp),
  FailureReason (TestFailed),
  Outcome (Failure),
  Result (resultDescription, resultOutcome, resultShortDescription),
  Status (Done, Executing, NotStarted),
  StatusMap,
  TreeFold (foldAfter, foldGroup, foldSingle),
  consoleTestReporter,
  foldTestTree,
  formatMessage,
  installSignalHandlers,
  listingTests,
  parseExpr,
  parseOptions,
  resultSuccessful,
  testsNames,
  trivialFold,
 )
import Text.Printf (printf)

-- | @since WIP
data BenchConfig
  = -- | Compile with UPLC simplifier pass and no tracing
    Optimizing
  | -- | Compile without UPLC simplifier and configurable tracing
    NonOptimizing Config

{- | Create benchmark from Plutarch term without tracing and no UPLC simplifier

@since WIP
-}
bench :: TestName -> ClosedTerm a -> TestTree
bench name = benchWithConfig name (NonOptimizing NoTracing)

{- | Like 'bench' but with customizable compilation config

@since WIP
-}
benchWithConfig :: TestName -> BenchConfig -> ClosedTerm a -> TestTree
benchWithConfig name config term = singleTest name $ PBenchmarkable config term

{- | Compare benchmarks, reporting relative CPU, MEM, and size differences

 @since WIP
-}
bcompare ::
  -- | Tasty pattern to compare as baseline
  String ->
  -- | Test or test tree to compare with baseline test
  TestTree ->
  TestTree
bcompare = bcompareWithin (-1 / 0, 1 / 0) (-1 / 0, 1 / 0) (-1 / 0, 1 / 0)

{- | Like 'bcompare' but with customizable upper and lower bounds of relative differences

@since WIP
-}
bcompareWithin ::
  -- | CPU bounds
  (Double, Double) ->
  -- | MEM bounds
  (Double, Double) ->
  -- | Size bounds
  (Double, Double) ->
  -- | Tasty pattern to compare as baseline
  String ->
  -- | Test or test tree to compare with baseline test
  TestTree ->
  TestTree
bcompareWithin cpu mem size s = case parseExpr s of
  Nothing -> error $ "Could not parse pbcompare pattern " ++ s
  Just e -> after_ AllSucceed (And (StringLit (pbcomparePrefix ++ show (WithLoHi () cpu mem size))) e)

{- | Use this instead of 'Test.Tasty.defaultMain' from @Test.Tasty@ to run benchmarks to get formatted output

@since WIP
-}
defaultMain :: TestTree -> IO ()
defaultMain bs = do
  setLocaleEncoding utf8
  installSignalHandlers
  let pbenchIngredients = [listingTests, composeReporters consoleBenchReporter csvReporter]
  opts <- parseOptions pbenchIngredients bs
  let opts' = setOption (MinDurationToReport 1000000000000) opts
  case tryIngredients pbenchIngredients opts' bs of
    Nothing -> exitFailure
    Just act -> act >>= \x -> if x then exitSuccess else exitFailure

-- | @since WIP
consoleBenchReporter :: Ingredient
consoleBenchReporter = modifyConsoleReporter [Option (Proxy :: Proxy (Maybe BaselinePath))] $ \opts -> do
  baseline <- case lookupOption opts of
    Nothing -> pure Set.empty
    Just (BaselinePath path) ->
      Set.fromList
        . joinQuotedFields
        . lines
        <$> (readFile path >>= evaluate . force)
  pure $ \name uDepR r ->
    case uDepR of
      None -> testFailed "Failed to find pattern from `bcompare`"
      NotUnique -> testFailed "Pattern from `bcompare` is not unique"
      mDepR ->
        case safeRead (resultDescription r) of
          Nothing -> r
          Just
            ( WithLoHi
                est@(ExecutionBudget budgetCpu' budgetMem' budgetSize')
                (lowerBoundCpu, upperBoundCpu)
                (lowerBoundMem, upperBoundMem)
                (lowerBoundSize, upperBoundSize)
              ) ->
              (if isAcceptable then id else forceFail)
                r
                  { resultDescription =
                      toTableAligned
                        [ ["CPU", show budgetCpu', bcompareCpu, showSlowdown slowDownCpu]
                        , ["MEM", show budgetMem', bcompareMem, showSlowdown slowDownMem]
                        , ["SIZE", show budgetSize', bcompareSize, showSlowdown slowDownSize]
                        ]
                  }
              where
                showSlowdown s = if isNothing mSlowDown then "" else formatSlowDown s
                isAcceptable = isAcceptableVsBaseline && isAcceptableVsBcompare
                mSlowDown = compareVsBaseline baseline name est
                slowDownCpu = maybe 1 (\(cpu, _, _) -> cpu) mSlowDown
                slowDownMem = maybe 1 (\(_, mem, _) -> mem) mSlowDown
                slowDownSize = maybe 1 (\(_, _, size) -> size) mSlowDown
                isAcceptableVsBaseline =
                  slowDownCpu >= lowerBoundCpu
                    && slowDownCpu <= upperBoundCpu
                    && slowDownMem >= lowerBoundMem
                    && slowDownMem <= upperBoundMem
                    && slowDownSize >= lowerBoundSize
                    && slowDownSize <= upperBoundSize
                (isAcceptableVsBcompare, bcompareCpu, bcompareMem, bcompareSize) = case mDepR of
                  NotProvided -> (True, "", "", "" :: String)
                  Unique
                    ( WithLoHi
                        depR
                        (depLowerBoundCpu, depUpperBoundCpu)
                        (depLowerBoundMem, depUpperBoundMem)
                        (depLowerBoundSize, depUpperBoundSize)
                      ) -> case safeRead (resultDescription depR) of
                      Nothing -> (True, "", "", "")
                      Just (WithLoHi (ExecutionBudget depCpu depMem depSize) _ _ _) ->
                        let
                          ratioCpu :: Double = fromIntegral budgetCpu' / fromIntegral depCpu
                          ratioMem :: Double = fromIntegral budgetMem' / fromIntegral depMem
                          ratioSize :: Double = fromIntegral budgetSize' / fromIntegral depSize
                         in
                          ( ratioCpu >= depLowerBoundCpu
                              && ratioCpu <= depUpperBoundCpu
                              && ratioMem >= depLowerBoundMem
                              && ratioMem <= depUpperBoundMem
                              && ratioSize >= depLowerBoundSize
                              && ratioSize <= depUpperBoundSize
                          , printf "%.2fx" ratioCpu
                          , printf "%.2fx" ratioMem
                          , printf "%.2fx" ratioSize
                          )

-- | @since WIP
csvReporter :: Ingredient
csvReporter = TestReporter [Option (Proxy :: Proxy (Maybe CsvPath))] $
  \opts tree -> do
    CsvPath path <- lookupOption opts
    let names = testsNames opts tree
        namesMap = IntMap.fromDistinctAscList $ zip [0 ..] names
    pure $ \smap -> do
      case findNonUniqueElement names of
        Nothing -> pure ()
        Just name -> do
          hPutStrLn stderr $ "CSV report cannot proceed, because name '" ++ name ++ "' corresponds to two or more benchmarks. Please disambiguate them."
          exitFailure
      let augmented = IntMap.intersectionWith (,) namesMap smap
      bracket
        ( do
            h <- openFile path WriteMode
            hSetBuffering h LineBuffering
            hPutStrLn h "name,cpu,mem,size"
            pure h
        )
        hClose
        (`csvOutput` augmented)
      pure $ const $ isSuccessful smap

-- | @since WIP
newtype FailIfMoreCpu = FailIfMoreCpu Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfMoreCpu where
  defaultValue = FailIfMoreCpu (1.0 / 0.0)
  parseValue = fmap FailIfMoreCpu . parsePositivePercents
  optionName = Tagged "fail-if-more-cpu"
  optionHelp = Tagged "If a benchmark uses unacceptably more CPU than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype FailIfLessCpu = FailIfLessCpu Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfLessCpu where
  defaultValue = FailIfLessCpu (1.0 / 0.0)
  parseValue = fmap FailIfLessCpu . parsePositivePercents
  optionName = Tagged "fail-if-less-cpu"
  optionHelp = Tagged "If a benchmark uses unacceptably less CPU than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype FailIfMoreMem = FailIfMoreMem Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfMoreMem where
  defaultValue = FailIfMoreMem (1.0 / 0.0)
  parseValue = fmap FailIfMoreMem . parsePositivePercents
  optionName = Tagged "fail-if-more-cpu"
  optionHelp = Tagged "If a benchmark uses unacceptably more MEM than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype FailIfLessMem = FailIfLessMem Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfLessMem where
  defaultValue = FailIfLessMem (1.0 / 0.0)
  parseValue = fmap FailIfLessMem . parsePositivePercents
  optionName = Tagged "fail-if-less-mem"
  optionHelp = Tagged "If a benchmark uses unacceptably less MEM than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype FailIfBigger = FailIfBigger Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfBigger where
  defaultValue = FailIfBigger (1.0 / 0.0)
  parseValue = fmap FailIfBigger . parsePositivePercents
  optionName = Tagged "fail-if-bigger"
  optionHelp = Tagged "If a benchmark is unacceptably bigger than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype FailIfSmaller = FailIfSmaller Double
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    , -- | @since WIP
      Read
    )
  deriving
    ( -- | @since WIP
      Num
    , -- | @since WIP
      Fractional
    )
    via Double

-- | @since WIP
instance IsOption FailIfSmaller where
  defaultValue = FailIfSmaller (1.0 / 0.0)
  parseValue = fmap FailIfSmaller . parsePositivePercents
  optionName = Tagged "fail-if-smaller"
  optionHelp = Tagged "If a benchmark is unacceptably smaller than baseline (see --baseline), it will be reported as failed."

-- | @since WIP
newtype BaselinePath = BaselinePath FilePath
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    )

-- | @since WIP
instance IsOption (Maybe BaselinePath) where
  defaultValue = Nothing
  parseValue = Just . Just . BaselinePath
  optionName = Tagged "baseline"
  optionHelp = Tagged "File with baseline results in CSV format to compare against"

-- | @since WIP
newtype CsvPath = CsvPath FilePath
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    )

-- | @since WIP
instance IsOption (Maybe CsvPath) where
  defaultValue = Nothing
  parseValue = Just . Just . CsvPath
  optionName = Tagged "csv"
  optionHelp = Tagged "File to write results in CSV format"

-- * Internals

-- Most of this code is from `tasty-bench`

modifyConsoleReporter ::
  [OptionDescription] ->
  (OptionSet -> IO (TestName -> Unique (WithLoHi Result) -> Result -> Result)) ->
  Ingredient
modifyConsoleReporter desc' iof = TestReporter (desc ++ desc') $ \opts tree ->
  let nameSeqs = IntMap.fromDistinctAscList $ zip [0 ..] $ testNameSeqs opts tree
      namesAndDeps =
        IntMap.fromDistinctAscList $
          zip [0 ..] $
            testNamesAndDeps nameSeqs opts tree
      modifySMap =
        (iof opts >>=)
          . flip postprocessResult
          . IntMap.intersectionWith (\(a, b) c -> (a, b, c)) namesAndDeps
   in (modifySMap >=>) <$> cb opts tree
  where
    (desc, cb) = case consoleTestReporter of
      TestReporter d c -> (d, c)
      _ -> error "modifyConsoleReporter: consoleTestReporter must be TestReporter"

data ExecutionBudget = ExecutionBudget Integer Integer Integer
  deriving stock (Show, Read)

data WithLoHi a
  = WithLoHi
      !a -- payload
      !(Double, Double) -- cpu
      !(Double, Double) -- mem
      !(Double, Double) -- size
  deriving stock (Show, Read)

data PBenchmarkable where
  PBenchmarkable :: BenchConfig -> ClosedTerm a -> PBenchmarkable

instance IsTest PBenchmarkable where
  testOptions =
    Tagged
      [ Option (Proxy :: Proxy FailIfMoreCpu)
      , Option (Proxy :: Proxy FailIfLessCpu)
      , Option (Proxy :: Proxy FailIfMoreMem)
      , Option (Proxy :: Proxy FailIfLessMem)
      , Option (Proxy :: Proxy FailIfBigger)
      , Option (Proxy :: Proxy FailIfSmaller)
      ]
  run opts (PBenchmarkable config term) _progress =
    case compiled of
      Left err -> pure $ testFailed $ "Failed to compile term: " <> Text.unpack err
      Right script ->
        case evalScriptUnlimited script of
          (Left err, _, _) -> pure $ testFailed $ "Failed to evaluate term: " <> show err
          (Right _, ExBudget (ExCPU cpu) (ExMemory mem), _) ->
            pure $
              testPassed $
                show $
                  WithLoHi
                    (ExecutionBudget (fromSatInt cpu) (fromSatInt mem) (scriptSize script))
                    (1 - ifLessCpu, 1 + ifMoreCpu)
                    (1 - ifLessMem, 1 + ifMoreMem)
                    (1 - ifSmaller, 1 + ifBigger)
    where
      compiled =
        case config of
          Optimizing -> compileOptimized term
          NonOptimizing pconfig -> compile pconfig term

      FailIfLessCpu ifLessCpu = lookupOption opts
      FailIfMoreCpu ifMoreCpu = lookupOption opts
      FailIfLessMem ifLessMem = lookupOption opts
      FailIfMoreMem ifMoreMem = lookupOption opts
      FailIfBigger ifBigger = lookupOption opts
      FailIfSmaller ifSmaller = lookupOption opts

data Unique a = None | Unique !a | NotUnique | NotProvided
  deriving stock (Functor)

instance Semigroup (Unique a) where
  a <> NotProvided = a
  NotProvided <> a = a
  a <> None = a
  None <> a = a
  _ <> _ = NotUnique

instance Monoid (Unique a) where
  mempty = NotProvided
  mappend = (<>)

-- | Convert a test tree to a list of test names.
testNameSeqs :: OptionSet -> TestTree -> [Seq TestName]
testNameSeqs =
  foldTestTree
    trivialFold
      { foldSingle = const $ const . (: []) . Seq.singleton
      , foldGroup = const $ (. concat) . map . (<|)
      }

testNamesAndDeps :: IntMap (Seq TestName) -> OptionSet -> TestTree -> [(TestName, Unique (WithLoHi IntMap.Key))]
testNamesAndDeps im =
  foldTestTree
    trivialFold
      { foldSingle = const $ const . (: []) . (,NotProvided)
      , foldGroup = const $ (. concat) . map . first . (++) . (++ ".")
      , foldAfter = const foldDeps
      }
  where
    foldDeps :: DependencyType -> Expr -> [(a, Unique (WithLoHi IntMap.Key))] -> [(a, Unique (WithLoHi IntMap.Key))]
    foldDeps AllSucceed (And (StringLit xs) p)
      | pbcomparePrefix `isPrefixOf` xs
      , Just (WithLoHi () cpu mem size) <- safeRead $ drop (length pbcomparePrefix) xs =
          map $ second $ mappend $ (\x -> WithLoHi x cpu mem size) <$> findMatchingKeys im p
    foldDeps _ _ = map (second (const NotProvided))

pbcomparePrefix :: String
pbcomparePrefix = "plutarch-bench"

findMatchingKeys :: IntMap (Seq TestName) -> Expr -> Unique IntMap.Key
findMatchingKeys im p =
  foldMap (\(k, v) -> if withFields v pat == Right True then Unique k else None) $ IntMap.assocs im
  where
    pat = eval p >>= asB

postprocessResult ::
  (TestName -> Unique (WithLoHi Result) -> Result -> Result) ->
  IntMap (TestName, Unique (WithLoHi IntMap.Key), TVar Status) ->
  IO StatusMap
postprocessResult f src = do
  paired <- forM src $ \(name, mDepId, tv) -> (name,mDepId,tv,) <$> newTVarIO NotStarted
  let doUpdate = atomically $ do
        (Any anyUpdated, All allDone) <-
          getApp $ flip foldMap paired $ \(name, mDepId, newTV, oldTV) -> Ap $ do
            old <- readTVar oldTV
            case old of
              Done {} -> pure (Any False, All True)
              _ -> do
                new <- readTVar newTV
                case new of
                  Done res -> do
                    depRes <- case mDepId of
                      Unique (WithLoHi depId cpu mem size) -> case IntMap.lookup depId src of
                        Nothing -> pure None
                        Just (_, _, depTV) -> do
                          depStatus <- readTVar depTV
                          case depStatus of
                            Done dep -> pure $ Unique (WithLoHi dep cpu mem size)
                            _ -> pure NotProvided
                      None -> pure None
                      NotUnique -> pure NotUnique
                      NotProvided -> pure NotProvided
                    writeTVar oldTV (Done (f name depRes res))
                    pure (Any True, All True)
                  Executing newProgr -> do
                    let updated = case old of
                          Executing oldProgr -> oldProgr /= newProgr
                          _ -> True
                    when updated $
                      writeTVar oldTV (Executing newProgr)
                    pure (Any updated, All False)
                  NotStarted -> pure (Any False, All False)
        if anyUpdated || allDone then pure allDone else retry
      adNauseam = doUpdate >>= (`unless` adNauseam)
  _ <- forkIO adNauseam
  pure $ fmap (\(_, _, _, a) -> a) paired

parsePositivePercents :: String -> Maybe Double
parsePositivePercents xs = do
  x <- safeRead xs
  guard (x > 0)
  pure (x / 100)

joinQuotedFields :: [String] -> [String]
joinQuotedFields [] = []
joinQuotedFields (x : xs)
  | areQuotesBalanced x = x : joinQuotedFields xs
  | otherwise = case span areQuotesBalanced xs of
      (_, []) -> [] -- malformed CSV
      (ys, z : zs) -> unlines (x : ys ++ [z]) : joinQuotedFields zs
  where
    areQuotesBalanced = even . length . filter (== '"')

forceFail :: Result -> Result
forceFail r = r {resultOutcome = Failure TestFailed, resultShortDescription = "FAIL"}

formatSlowDown :: Double -> String
formatSlowDown ratio = case percents `compare` 0 of
  LT -> printf "%2i%% less than baseline" (-percents)
  EQ -> "same as baseline"
  GT -> printf "%2i%% more than baseline" percents
  where
    percents :: Int64
    percents = truncate ((ratio - 1) * 100)

compareVsBaseline :: Set String -> TestName -> ExecutionBudget -> Maybe (Double, Double, Double)
compareVsBaseline baseline name (ExecutionBudget cpu mem size) = case mOld of
  Nothing -> Nothing
  Just (oldCpu, oldMem, oldSize) ->
    Just
      (fromIntegral cpu / fromIntegral oldCpu, fromIntegral mem / fromIntegral oldMem, fromIntegral size / fromIntegral oldSize)
  where
    mOld :: Maybe (Integer, Integer, Integer)
    mOld = do
      let prefix = encodeCsv name ++ ","
      (line, furtherLines) <- Set.minView $ snd $ Set.split prefix baseline

      case Set.minView furtherLines of
        Nothing -> pure ()
        Just (nextLine, _) -> case stripPrefix prefix nextLine of
          Nothing -> pure ()
          -- If there are several lines matching prefix, skip them all.
          -- Should not normally happen, 'csvReporter' prohibits repeating test names.
          Just {} -> Nothing

      (cpuCell, ',' : rest) <- span (/= ',') <$> stripPrefix prefix line
      (memCell, ',' : rest') <- pure $ span (/= ',') rest
      let sizeCell = takeWhile (/= ',') rest'
      (,,) <$> safeRead cpuCell <*> safeRead memCell <*> safeRead sizeCell

encodeCsv :: String -> String
encodeCsv xs
  | any @[] @Char (`elem` xs) ",\"\n\r" =
      '"' : go xs -- opening quote
  | otherwise = xs
  where
    go [] = "\"" -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys) = y : go ys

scriptSize :: Script -> Integer
scriptSize = fromIntegral . Short.length . serialiseUPLC . unScript

toTableAligned :: [[String]] -> String
toTableAligned xss' = unlines $ map (intercalate " | " . zipWith (pad . padColumn) [0 ..]) xss
  where
    xss :: [[String]] = map (filter (not . null)) xss'
    pad n xs = xs ++ replicate (n - length xs) ' '
    padColumn idx = maximum $ map (length . (!! idx)) xss

findNonUniqueElement :: Ord a => [a] -> Maybe a
findNonUniqueElement = go Set.empty
  where
    go _ [] = Nothing
    go acc (x : xs)
      | x `Set.member` acc = Just x
      | otherwise = go (Set.insert x acc) xs

csvOutput :: Handle -> IntMap (TestName, TVar Status) -> IO ()
csvOutput h = traverse_ $ \(name, tv) -> do
  r <- atomically $ readTVar tv >>= \case Done r -> pure r; _ -> retry
  case safeRead (resultDescription r) of
    Nothing -> pure ()
    Just (WithLoHi est _ _ _) -> do
      msg <- formatMessage $ csvEstimate est
      hPutStrLn h (encodeCsv name ++ ',' : msg)

csvEstimate :: ExecutionBudget -> String
csvEstimate (ExecutionBudget cpu mem size) = show cpu ++ "," ++ show mem ++ "," ++ show size

isSuccessful :: StatusMap -> IO Bool
isSuccessful = go . IntMap.elems
  where
    go [] = pure True
    go (tv : tvs) = do
      b <- atomically $ readTVar tv >>= \case Done r -> pure (resultSuccessful r); _ -> retry
      if b then go tvs else pure False
