module Plutarch.Test.Benchmark (
  Benchmark,
  ScriptSizeBytes,
  mkBenchmark,
  scriptSize,
) where

import Codec.Serialise (serialise)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Int (Int64)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (ExBudget (ExBudget), ExCPU, ExMemory, Script)

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

scriptSize :: Script -> ScriptSizeBytes
scriptSize = ScriptSizeBytes . fromIntegral . SBS.length . serialiseScriptShort

serialiseScriptShort :: Script -> SBS.ShortByteString
serialiseScriptShort = SBS.toShort . LB.toStrict . serialise
