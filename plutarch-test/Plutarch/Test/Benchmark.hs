module Plutarch.Test.Benchmark (
  -- * Working with `Benchmark` type
  Benchmark (Benchmark, exBudgetCPU, exBudgetMemory, scriptSizeBytes),
  ScriptSizeBytes,

  -- * Producing benchmark values
  mkBenchmark,
  scriptSize,
) where

import Data.Aeson (ToJSON)
import Data.ByteString.Short qualified as SBS
import Data.Int (Int64)
import GHC.Generics (Generic)
import Plutarch.Script (Script (unScript))
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V1 (ExBudget (ExBudget), ExCPU, ExMemory)

data Benchmark = Benchmark
  { exBudgetCPU :: ExCPU
  -- ^ CPU budget used by the script.
  , exBudgetMemory :: ExMemory
  -- ^ Memory budget used by the script.
  , scriptSizeBytes :: ScriptSizeBytes
  -- ^ Size of Plutus script in bytes
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

mkBenchmark :: ExBudget -> ScriptSizeBytes -> Benchmark
mkBenchmark (ExBudget cpu mem) = Benchmark cpu mem

newtype ScriptSizeBytes = ScriptSizeBytes Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON)

serialiseScriptShort :: Script -> SBS.ShortByteString
serialiseScriptShort = serialiseUPLC . unScript

scriptSize :: Script -> ScriptSizeBytes
scriptSize = ScriptSizeBytes . fromIntegral . SBS.length . serialiseScriptShort
