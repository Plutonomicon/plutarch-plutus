module Plutarch.Api.Internal.Scripts
  ( serialiseScript
  , hashScriptWithPrefix
  , datumHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Codec.Serialise (serialise)
import Crypto.Hash.Algorithms 
  ( Blake2b_224 (Blake2b_224)
  , Blake2b_256 (Blake2b_256)
  )
import Crypto.Hash (hashWith)
import Data.ByteArray (convert)

import qualified PlutusTx
import qualified PlutusTx.Builtins as PlutusTx
import qualified Plutus.V1.Ledger.Scripts as Plutus

-- | Serialise a Script to (Lazy) ByteString
serialiseScript :: Plutus.Script -> Lazy.ByteString
serialiseScript = serialise

-- | Hash a Script with the given version prefix
hashScriptWithPrefix :: ByteString -> Plutus.Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr = 
  Plutus.ScriptHash $ 
    PlutusTx.toBuiltin $ convert @_ @ByteString $ hashWith Blake2b_224 $ 
      prefix <> (Lazy.toStrict $ serialiseScript scr)

-- | Hash a Plutus Datum
datumHash :: (PlutusTx.ToData d) => d -> Plutus.DatumHash
datumHash d = 
  Plutus.DatumHash $
    PlutusTx.toBuiltin $ convert @_ @ByteString $ hashWith Blake2b_256 $
      Lazy.toStrict $ serialise $ PlutusTx.toData d






