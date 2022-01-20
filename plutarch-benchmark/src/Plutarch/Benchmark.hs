{-# LANGUAGE RecordWildCards #-}

-- | Benchmark (exbudget and script size) for Plutus scripts
module Plutarch.Benchmark (
  -- | * Types
  Benchmark,
  NamedBenchmark,
  ScriptSizeBytes,
  -- | * Benchmark an arbitraty Plutus script
  benchmarkScript,
  -- | * Benchmark entrypoints
  bench,
  benchGroup,
  benchMain,
  -- | * Working with benchmark results
  decodeBenchmarks,
  diffBenchmarks,
) where

import qualified Codec.Serialise as Codec
import Control.Arrow ((&&&))
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Text.PrettyPrint.Boxes as B

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Csv (
  DefaultOrdered,
  ToField,
  ToNamedRecord,
  header,
  namedRecord,
  (.!),
  (.=),
 )
import qualified Data.Csv as Csv
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
  Script,
 )
import qualified Plutus.V1.Ledger.Api as Plutus

--------------------------------------------------------------------------------

-- | Benchmark the given script
benchmarkScript :: String -> Script -> NamedBenchmark
benchmarkScript name = NamedBenchmark . (name,) . benchmarkScript'

benchmarkScript' :: Script -> Benchmark
benchmarkScript' =
  uncurry mkBenchmark . (evalScriptCounting &&& (fromInteger . toInteger . SBS.length)) . serialiseScript
  where
    mkBenchmark :: ExBudget -> Int64 -> Benchmark
    mkBenchmark (ExBudget cpu mem) = Benchmark cpu mem . ScriptSizeBytes

    serialiseScript :: Script -> SBS.ShortByteString
    serialiseScript = SBS.toShort . LB.toStrict . Codec.serialise -- Using `flat` here breaks `evalScriptCounting`
    evalScriptCounting :: HasCallStack => Plutus.SerializedScript -> Plutus.ExBudget
    evalScriptCounting script =
      let costModel = fromJust Plutus.defaultCostModelParams
          (_logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose costModel script []
       in case e of
            Left evalErr -> error ("Eval Error: " <> show evalErr)
            Right exbudget -> exbudget

data Benchmark = Benchmark
  { exBudgetCPU :: ExCPU
  -- ^ CPU budget used by the script
  , exBudgetMemory :: ExMemory
  -- ^ Memory budget used by the script
  , scriptSizeBytes :: ScriptSizeBytes
  -- ^ Size of Plutus script in bytes
  }
  deriving stock (Show, Generic)

newtype ScriptSizeBytes = ScriptSizeBytes Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToField)

{- | A `Benchmark` with a name.

 Handy for writing CSV files with headers.
-}
newtype NamedBenchmark = NamedBenchmark (String, Benchmark)
  deriving stock (Show, Generic)

instance ToNamedRecord NamedBenchmark where
  toNamedRecord (NamedBenchmark (name, Benchmark {..})) =
    namedRecord ["name" .= name, "cpu" .= exBudgetCPU, "mem" .= exBudgetMemory, "size" .= scriptSizeBytes]

instance DefaultOrdered NamedBenchmark where
  headerOrder _ = header ["name", "cpu", "mem", "size"]

benchGroup :: String -> [[NamedBenchmark]] -> [NamedBenchmark]
benchGroup groupName bs =
  [NamedBenchmark (groupName ++ ":" ++ name, benchmark) | NamedBenchmark (name, benchmark) <- concat bs]

bench :: String -> ClosedTerm a -> [NamedBenchmark]
bench name prog =
  [coerce . benchmarkScript name $ compile prog]

decodeBenchmarks :: LB.ByteString -> Either String [NamedBenchmark]
decodeBenchmarks =
  let (#!) :: Num a => Vector Csv.Field -> Int -> Csv.Parser a
      (#!) v f = fmap fromInteger . Csv.parseField $ v ! f
   in fmap Vector.toList
        <$> Csv.decodeWithP
          ( \case
              v
                | length v == 4 ->
                    fmap NamedBenchmark $
                      (,) <$> v .! 0 <*> (Benchmark <$> v #! 1 <*> v #! 2 <*> v #! 3)
              _ | otherwise -> mzero
          )
          Csv.defaultDecodeOptions
          Csv.HasHeader

diffBenchmarks :: [NamedBenchmark] -> [NamedBenchmark] -> B.Box
diffBenchmarks old new =
  let oldMap = Map.fromList (coerce old)
      newMap = Map.fromList (coerce new)

      percentageDiff old new =
        let larger = max old new

            pctChange =
              softRound (fromInteger (toInteger new - toInteger old) / fromInteger (toInteger larger) * 100)

            softRound n =
              fromInteger @Double (round @Double @Integer n * 10) / 10
            plus = if pctChange > 0 then "+" else ""
         in plus <> show pctChange <> "%"
      showDiff old new tag =
        if old == new
          then
            [ B.text $ show new <> "(" <> tag <> ")"
            , B.text ""
            ]
          else
            [ B.text $ show new <> "(" <> tag <> ")"
            , B.text $ percentageDiff old new
            ]

      rows =
        Map.elems $
          Map.mapMaybeWithKey
            ( \k (Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes size)) ->
                case oldMap Map.!? k of
                  Nothing ->
                    Just $
                      mconcat
                        [
                          [ B.text k
                          ]
                        , showDiff cpu cpu "cpu"
                        , showDiff mem mem "mem"
                        , showDiff size size "size"
                        ]
                  Just (Benchmark (ExCPU oldCpu) (ExMemory oldMem) (ScriptSizeBytes oldSize)) ->
                    if oldCpu /= cpu || oldMem /= mem || oldSize /= size
                      then
                        Just $
                          mconcat
                            [
                              [ B.text k
                              ]
                            , showDiff oldCpu cpu "cpu"
                            , showDiff oldMem mem "mem"
                            , showDiff oldSize size "size"
                            ]
                      else Nothing
            )
            newMap
      alignments =
        -- Align all but the first column to the right, because they represent numeric values.
        B.left : repeat B.right
   in case rows of
        [] -> B.text "Benchmark results are identical."
        _ -> B.hsep 2 B.left . fmap (uncurry B.vcat) $ zip alignments (List.transpose rows)

renderBudgetTable :: [NamedBenchmark] -> B.Box
renderBudgetTable bs =
  let rows =
        [ [ B.text name
          , B.text $ show cpu <> "(cpu)"
          , B.text $ show mem <> "(mem)"
          , B.text $ show sz <> "(bytes)"
          ]
        | NamedBenchmark (name, Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes sz)) <- bs
        ]
      alignments =
        -- Align all but the first column to the right, because they represent numeric values.
        B.left : repeat B.right
   in B.hsep 2 B.left . fmap (uncurry B.vcat) $ zip alignments (List.transpose rows)

benchMain :: [NamedBenchmark] -> IO ()
benchMain benchmarks = do
  let csv = Csv.encodeDefaultOrderedByName benchmarks
  BSL.writeFile "bench.csv" csv
  putStrLn "Wrote to bench.csv:"
  putStrLn . B.render $ renderBudgetTable benchmarks
