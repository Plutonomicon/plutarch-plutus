{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Here be dragons
module Plutarch.Test.Bench.Meta (
  Plutarch.Test.Bench.Meta.defaultMain,
  bcompareWithin,
  bench,
) where

import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, guard, unless, when, (>=>))
import Data.Bifunctor (first, second)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (All (All), Any (Any))
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (Tagged (Tagged))
import GHC.Conc (TVar, atomically, forkIO, newTVarIO, readTVar, retry, writeTVar)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Plutarch.Prelude
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
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest, testPassed)
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
import Type.Reflection (Typeable)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

type family Length (lst :: [Symbol]) = (r :: Nat) where
  Length '[] = Zero
  Length (_ ': rest) = Succ (Length rest)

type family LoHiRanges (len :: Nat) = r | r -> len where
  LoHiRanges (Succ Zero) = (Double, Double)
  LoHiRanges (Succ rest) = ((Double, Double), LoHiRanges rest)

type family MeasurementValues (len :: Nat) = r | r -> len where
  MeasurementValues (Succ Zero) = Integer
  MeasurementValues (Succ rest) = (Integer, MeasurementValues rest)

type family MeasurementDifference (len :: Nat) = r | r -> len where
  MeasurementDifference (Succ Zero) = Double
  MeasurementDifference (Succ rest) = (Double, MeasurementDifference rest)

data WithLoHi (len :: Nat) (a :: Type) = WithLoHi a (LoHiRanges len)

deriving stock instance (Show a, Show (LoHiRanges lst)) => Show (WithLoHi lst a)

deriving stock instance (Read a, Read (LoHiRanges lst)) => Read (WithLoHi lst a)

bench ::
  forall (lst :: [Symbol]) (a :: Type).
  IsTest (Benchmarkable lst) =>
  TestName ->
  a ->
  (a -> Either Result (MeasurementValues (Length lst))) ->
  TestTree
bench name a f = singleTest name $ Benchmarkable @a @lst a f

bcompareWithin ::
  forall (lst :: [Symbol]).
  Show (LoHiRanges (Length lst)) =>
  LoHiRanges (Length lst) ->
  -- | Tasty pattern to compare as baseline
  String ->
  -- | Test or test tree to compare with baseline test
  TestTree ->
  TestTree
bcompareWithin loHi s = case parseExpr s of
  Nothing -> error $ "Could not parse pbcompare pattern " ++ s
  Just e -> after_ AllSucceed (And (StringLit (pbcomparePrefix ++ show (WithLoHi () loHi))) e)

defaultMain ::
  forall (lst :: [Symbol]).
  ( GetRows lst
  , CsvMeasurement lst
  , CsvHeader lst
  , MkMeasurementDifference (Length lst)
  , Show (MeasurementValues (Length lst))
  , FromCsv (Length lst)
  , Read (MeasurementValues (Length lst))
  , Read (LoHiRanges (Length lst))
  ) =>
  TestTree ->
  IO ()
defaultMain bs = do
  setLocaleEncoding utf8
  installSignalHandlers
  let benchIngredients = [listingTests, composeReporters (consoleBenchReporter @lst) (csvReporter @lst)]
  opts <- parseOptions benchIngredients bs
  let opts' = setOption (MinDurationToReport 1000000000000) opts
  case tryIngredients benchIngredients opts' bs of
    Nothing -> exitFailure
    Just act -> act >>= \x -> if x then exitSuccess else exitFailure

class GetRows (lst :: [Symbol]) where
  getRows ::
    Maybe (MeasurementDifference (Length lst)) ->
    Maybe (WithLoHi (Length lst) Result) ->
    LoHiRanges (Length lst) ->
    MeasurementValues (Length lst) ->
    ([[String]], Bool)

instance KnownSymbol x => GetRows '[x] where
  getRows mSlowDown mDepR (lowerBound, upperBound) value =
    ( [[symbolVal (Proxy @x), show value, bcompare, showSlowdown slowDown]]
    , isAcceptable
    )
    where
      isAcceptable = isAcceptableVsBaseline && isAcceptableVsBcompare
      showSlowdown s = if isNothing mSlowDown then "" else formatSlowDown s
      slowDown = fromMaybe 1 mSlowDown
      isAcceptableVsBaseline = slowDown >= lowerBound && slowDown <= upperBound
      (isAcceptableVsBcompare, bcompare) = case mDepR of
        Nothing -> (True, "" :: String)
        Just (WithLoHi depR (depLowerBound, depUpperBound)) ->
          case safeRead (resultDescription depR) of
            Nothing -> (True, "")
            Just (WithLoHi (depValue :: MeasurementValues (Length '[x])) (_ :: LoHiRanges (Length '[x]))) ->
              let
                ratio :: Double = fromIntegral value / fromIntegral depValue
               in
                ( ratio >= depLowerBound && ratio <= depUpperBound
                , printf "%.2fx" ratio
                )

instance
  ( KnownSymbol x
  , GetRows (y ': rest)
  , Show (MeasurementValues (Length (y ': rest)))
  , Read (MeasurementValues (Length (y ': rest)))
  , Show (LoHiRanges (Length (y ': rest)))
  , Read (LoHiRanges (Length (y ': rest)))
  ) =>
  GetRows (x ': y ': rest)
  where
  getRows mSlowDown mDepR ((lowerBound, upperBound), loHi) (value, values) =
    ( [symbolVal (Proxy @x), show value, bcompare, showSlowdown slowDown] : otherRows
    , isAcceptable && otherAcceptable
    )
    where
      (otherRows, otherAcceptable) =
        getRows @(y ': rest) (fmap snd mSlowDown) mDepRSnd loHi values
      -- mDepRSnd = case mDepR of
      --   Nothing -> Nothing
      --   Just (WithLoHi res (_, ranges)) -> Just $ WithLoHi res ranges
      isAcceptable = isAcceptableVsBaseline && isAcceptableVsBcompare
      showSlowdown s = if isNothing mSlowDown then "" else formatSlowDown s
      slowDown = maybe 1 fst mSlowDown
      isAcceptableVsBaseline = slowDown >= lowerBound && slowDown <= upperBound
      (isAcceptableVsBcompare, bcompare, mDepRSnd) = case mDepR of
        Nothing -> (True, "" :: String, Nothing)
        Just (WithLoHi depR ((depLowerBound, depUpperBound), bounds)) ->
          case safeRead (resultDescription depR) of
            Nothing -> (True, "", Just $ WithLoHi depR bounds)
            Just (WithLoHi ((depValue, depValues) :: MeasurementValues (Length (x ': y ': rest))) ((_, depRanges) :: LoHiRanges (Length (x ': y ': rest)))) ->
              let
                ratio :: Double = fromIntegral value / fromIntegral depValue
               in
                ( ratio >= depLowerBound && ratio <= depUpperBound
                , printf "%.2fx" ratio
                , Just $ WithLoHi (depR {resultDescription = show $ WithLoHi depValues depRanges}) bounds
                )

consoleBenchReporter ::
  forall (lst :: [Symbol]).
  ( GetRows lst
  , MkMeasurementDifference (Length lst)
  , FromCsv (Length lst)
  , Read (MeasurementValues (Length lst))
  , Read (LoHiRanges (Length lst))
  ) =>
  Ingredient
consoleBenchReporter = modifyConsoleReporter [Option (Proxy :: Proxy (Maybe BaselinePath))] $ \opts -> do
  baseline <- case lookupOption opts of
    Nothing -> pure Set.empty
    Just (BaselinePath path) ->
      Set.fromList
        . joinQuotedFields
        . lines
        <$> (readFile path >>= evaluate . force)
  pure $ \name mDepR r -> case safeRead @(WithLoHi (Length lst) (MeasurementValues (Length lst))) (resultDescription r) of
    Nothing -> r
    Just
      (WithLoHi values loHi) ->
        (if isAcceptable then id else forceFail)
          r
            { resultDescription = toTableAligned table
            }
        where
          (table, isAcceptable) = getRows @lst mSlowDown mDepR loHi values
          mSlowDown = compareVsBaseline baseline name values

csvReporter ::
  forall (lst :: [Symbol]).
  ( CsvMeasurement lst
  , CsvHeader lst
  , Show (MeasurementValues (Length lst))
  , Read (MeasurementValues (Length lst))
  , Read (LoHiRanges (Length lst))
  ) =>
  Ingredient
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
            hPutStrLn h $ csvHeader @lst
            pure h
        )
        hClose
        (\x -> csvOutput @lst x augmented)
      pure $ const $ isSuccessful smap

class CsvHeader (lst :: [Symbol]) where
  csvHeader :: String

instance KnownSymbol x => CsvHeader '[x] where
  csvHeader = symbolVal (Proxy @x)

instance (KnownSymbol x, CsvHeader (y ': rest)) => CsvHeader (x ': y ': rest) where
  csvHeader = symbolVal (Proxy @x) ++ "," ++ csvHeader @(y ': rest)

modifyConsoleReporter ::
  forall (len :: Nat).
  Read (LoHiRanges len) =>
  [OptionDescription] ->
  (OptionSet -> IO (TestName -> Maybe (WithLoHi len Result) -> Result -> Result)) ->
  Ingredient
modifyConsoleReporter desc' iof = TestReporter (desc ++ desc') $ \opts tree ->
  let nameSeqs = IntMap.fromDistinctAscList $ zip [0 ..] $ testNameSeqs opts tree
      namesAndDeps =
        IntMap.fromDistinctAscList $
          zip [0 ..] $
            map (second isSingle) $
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
    isSingle (Unique a) = Just a
    isSingle _ = Nothing

newtype MeasurementOptionLess (s :: Symbol) = MeasurementOptionLess Double
  deriving stock (Show)

instance KnownSymbol s => IsOption (MeasurementOptionLess s) where
  defaultValue = MeasurementOptionLess (1.0 / 0.0)
  parseValue = fmap MeasurementOptionLess . parsePositivePercents
  optionName = Tagged (map toLower $ "fail-if-less-" <> symbolVal (Proxy @s))
  optionHelp = Tagged ("If a benchmark uses unacceptably less " <> symbolVal (Proxy @s) <> " than baseline (see --baseline), it will be reported as failed.")

newtype MeasurementOptionMore (s :: Symbol) = MeasurementOptionMore Double
  deriving stock (Show)

instance KnownSymbol s => IsOption (MeasurementOptionMore s) where
  defaultValue = MeasurementOptionMore (1.0 / 0.0)
  parseValue = fmap MeasurementOptionMore . parsePositivePercents
  optionName = Tagged (map toLower $ "fail-if-more-" <> symbolVal (Proxy @s))
  optionHelp = Tagged ("If a benchmark uses unacceptably more " <> symbolVal (Proxy @s) <> " than baseline (see --baseline), it will be reported as failed.")

class GetOptions (lst :: [Symbol]) where
  getOptions :: [OptionDescription]

instance GetOptions '[] where
  getOptions = []

instance (KnownSymbol x, GetOptions xs) => GetOptions (x ': xs) where
  getOptions =
    [ Option (Proxy :: Proxy (MeasurementOptionLess x))
    , Option (Proxy :: Proxy (MeasurementOptionMore x))
    ]
      ++ getOptions @xs

class MkLoHiRangesFromOpts (lst :: [Symbol]) where
  mkLoHiRangesFromOpts :: OptionSet -> LoHiRanges (Length lst)

instance KnownSymbol x => MkLoHiRangesFromOpts '[x] where
  mkLoHiRangesFromOpts opts = (1 - ifLess, 1 + ifMore)
    where
      MeasurementOptionLess ifLess = lookupOption @(MeasurementOptionLess x) opts
      MeasurementOptionMore ifMore = lookupOption @(MeasurementOptionMore x) opts

instance (KnownSymbol x, MkLoHiRangesFromOpts (y ': rest)) => MkLoHiRangesFromOpts (x ': y ': rest) where
  mkLoHiRangesFromOpts opts = ((1 - ifLess, 1 + ifMore), mkLoHiRangesFromOpts @(y ': rest) opts)
    where
      MeasurementOptionLess ifLess = lookupOption @(MeasurementOptionLess x) opts
      MeasurementOptionMore ifMore = lookupOption @(MeasurementOptionMore x) opts

data Benchmarkable (lst :: [Symbol]) where
  Benchmarkable :: a -> (a -> Either Result (MeasurementValues (Length lst))) -> Benchmarkable lst

instance
  ( Typeable lst
  , GetOptions lst
  , MkLoHiRangesFromOpts lst
  , Show (MeasurementValues (Length lst))
  , Show (LoHiRanges (Length lst))
  ) =>
  IsTest (Benchmarkable lst)
  where
  testOptions = Tagged $ getOptions @lst
  run opts (Benchmarkable input f) _progress =
    case f input of
      Left e -> pure e
      Right values -> pure $ testPassed $ show $ WithLoHi values $ mkLoHiRangesFromOpts @lst opts

data Unique a = None | Unique !a | NotUnique
  deriving stock (Functor)

instance Semigroup (Unique a) where
  None <> a = a
  a <> None = a
  _ <> _ = NotUnique

instance Monoid (Unique a) where
  mempty = None
  mappend = (<>)

-- | Convert a test tree to a list of test names.
testNameSeqs :: OptionSet -> TestTree -> [Seq TestName]
testNameSeqs =
  foldTestTree
    trivialFold
      { foldSingle = const $ const . (: []) . Seq.singleton
      , foldGroup = const $ (. concat) . map . (<|)
      }

testNamesAndDeps ::
  forall (len :: Nat).
  Read (LoHiRanges len) =>
  IntMap (Seq TestName) ->
  OptionSet ->
  TestTree ->
  [(TestName, Unique (WithLoHi len IntMap.Key))]
testNamesAndDeps im =
  foldTestTree
    trivialFold
      { foldSingle = const $ const . (: []) . (,mempty)
      , foldGroup = const $ (. concat) . map . first . (++) . (++ ".")
      , foldAfter = const foldDeps
      }
  where
    foldDeps ::
      DependencyType ->
      Expr ->
      [(a, Unique (WithLoHi len IntMap.Key))] ->
      [(a, Unique (WithLoHi len IntMap.Key))]
    foldDeps AllSucceed (And (StringLit xs) p)
      | pbcomparePrefix `isPrefixOf` xs
      , Just (WithLoHi () (loHiRanges :: LoHiRanges len)) <- safeRead $ drop (length pbcomparePrefix) xs =
          map $ second $ mappend $ (`WithLoHi` loHiRanges) <$> findMatchingKeys im p
    foldDeps _ _ = id

pbcomparePrefix :: String
pbcomparePrefix = "BENCH_PREFIX"

findMatchingKeys :: IntMap (Seq TestName) -> Expr -> Unique IntMap.Key
findMatchingKeys im p =
  foldMap (\(k, v) -> if withFields v pat == Right True then Unique k else mempty) $ IntMap.assocs im
  where
    pat = eval p >>= asB

postprocessResult ::
  forall (len :: Nat).
  (TestName -> Maybe (WithLoHi len Result) -> Result -> Result) ->
  IntMap (TestName, Maybe (WithLoHi len IntMap.Key), TVar Status) ->
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
                      Nothing -> pure Nothing
                      Just (WithLoHi depId loHiRanges) -> case IntMap.lookup depId src of
                        Nothing -> pure Nothing
                        Just (_, _, depTV) -> do
                          depStatus <- readTVar depTV
                          case depStatus of
                            Done dep -> pure $ Just (WithLoHi dep loHiRanges)
                            _ -> pure Nothing
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

class MkMeasurementDifference (len :: Nat) where
  mkMeasurementDifference ::
    (Integer -> Integer -> Double) ->
    MeasurementValues len ->
    MeasurementValues len ->
    MeasurementDifference len

instance MkMeasurementDifference (Succ Zero) where
  mkMeasurementDifference f = f

instance MkMeasurementDifference (Succ n) => MkMeasurementDifference (Succ (Succ n)) where
  mkMeasurementDifference f (x, xs) (y, ys) = (f x y, mkMeasurementDifference f xs ys)

compareVsBaseline ::
  forall (len :: Nat).
  ( MkMeasurementDifference len
  , FromCsv len
  ) =>
  Set String ->
  TestName ->
  MeasurementValues len ->
  Maybe (MeasurementDifference len)
compareVsBaseline baseline name values = case mOld of
  Nothing -> Nothing
  Just (oldValues :: MeasurementValues len) ->
    Just $
      mkMeasurementDifference
        (\new old -> fromIntegral new / fromIntegral old)
        values
        oldValues
  where
    mOld :: Maybe (MeasurementValues len)
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

      withoutPrefix <- stripPrefix prefix line
      fromCsv withoutPrefix

class FromCsv (len :: Nat) where
  fromCsv :: String -> Maybe (MeasurementValues len)

instance FromCsv (Succ Zero) where
  fromCsv s = safeRead (takeWhile (/= ',') s)

instance FromCsv (Succ n) => FromCsv (Succ (Succ n)) where
  fromCsv s = do
    (cell' :: String, ',' : restS) <- pure $ span (/= ',') s
    cell :: Integer <- safeRead cell'
    rest :: MeasurementValues (Succ n) <- fromCsv restS
    pure (cell, rest)

encodeCsv :: String -> String
encodeCsv xs
  | any @[] @Char (`elem` xs) ",\"\n\r" =
      '"' : go xs -- opening quote
  | otherwise = xs
  where
    go [] = "\"" -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys) = y : go ys

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

csvOutput ::
  forall (lst :: [Symbol]) (values :: Type) (loHiRanges :: Type).
  ( CsvMeasurement lst
  , values ~ MeasurementValues (Length lst)
  , Read values
  , Show values
  , Read loHiRanges
  , loHiRanges ~ LoHiRanges (Length lst)
  ) =>
  Handle ->
  IntMap (TestName, TVar Status) ->
  IO ()
csvOutput h = traverse_ $ \(name, tv) -> do
  r <- atomically $ readTVar tv >>= \case Done r -> pure r; _ -> retry
  case safeRead (resultDescription r) of
    Nothing -> pure ()
    Just (WithLoHi (values :: values) (_ :: loHiRanges)) -> do
      msg <- formatMessage $ csvMeasurement @lst values
      hPutStrLn h (encodeCsv name ++ ',' : msg)

class CsvMeasurement lst where
  csvMeasurement :: Show (MeasurementValues (Length lst)) => MeasurementValues (Length lst) -> String

instance CsvMeasurement '[s] where
  csvMeasurement (value :: Integer) = show value

instance
  (CsvMeasurement (y ': xs), Show (MeasurementValues (Succ (Length xs)))) =>
  CsvMeasurement (x ': y ': xs)
  where
  csvMeasurement (x, xs) = show x ++ "," ++ csvMeasurement @(y ': xs) xs

isSuccessful :: StatusMap -> IO Bool
isSuccessful = go . IntMap.elems
  where
    go [] = pure True
    go (tv : tvs) = do
      b <- atomically $ readTVar tv >>= \case Done r -> pure (resultSuccessful r); _ -> retry
      if b then go tvs else pure False

newtype BaselinePath = BaselinePath FilePath
  deriving stock (Eq, Ord)

instance IsOption (Maybe BaselinePath) where
  defaultValue = Nothing
  parseValue = Just . Just . BaselinePath
  optionName = Tagged "baseline"
  optionHelp = Tagged "File with baseline results in CSV format to compare against"

newtype CsvPath = CsvPath FilePath
  deriving stock (Eq, Ord)

instance IsOption (Maybe CsvPath) where
  defaultValue = Nothing
  parseValue = Just . Just . CsvPath
  optionName = Tagged "csv"
  optionHelp = Tagged "File to write results in CSV format"
