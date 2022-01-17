{-# LANGUAGE RecordWildCards #-}

-- | Benchmark (exbudget and script size) for Plutus scripts
module Benchmark (
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
) where

import qualified Codec.Serialise as Codec
import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BSL
import qualified Text.PrettyPrint.Boxes as B

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Csv (
  DefaultOrdered,
  ToField,
  ToNamedRecord,
  header,
  namedRecord,
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
  { -- | CPU budget used by the script
    exBudgetCPU :: ExCPU
  , -- | Memory budget used by the script
    exBudgetMemory :: ExMemory
  , -- | Size of Plutus script in bytes
    scriptSizeBytes :: ScriptSizeBytes
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
  [benchmarkScript name $ compile prog]

benchMain :: [NamedBenchmark] -> IO ()
benchMain benchmarks = do
  let csv = Csv.encodeDefaultOrderedByName benchmarks
  BSL.writeFile "bench.csv" csv
  putStrLn "Wrote to bench.csv:"
  putStrLn $ B.render . renderBudgetTable $ benchmarks
  where
    -- Renders a neat tabular representation of the benchmarks
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
