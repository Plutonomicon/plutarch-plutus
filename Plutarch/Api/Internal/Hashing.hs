module Plutarch.Api.Internal.Hashing (
  hashScriptWithPrefix,
  hashData,
  hashLedgerBytes,
) where

import Codec.Serialise (serialise)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (
  Blake2b_224 (Blake2b_224),
  Blake2b_256 (Blake2b_256),
  HashAlgorithm,
 )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx.Builtins as PlutusTx

_plutusHashWith :: HashAlgorithm alg => alg -> ByteString -> PlutusTx.BuiltinByteString
_plutusHashWith alg = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith alg

hashBlake2b_224 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_224 = _plutusHashWith Blake2b_224

hashBlake2b_256 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_256 = _plutusHashWith Blake2b_256

-- | Hash a Script with the given version prefix
hashScriptWithPrefix :: ByteString -> Plutus.Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash
    . hashBlake2b_224
    $ prefix <> Lazy.toStrict (serialise scr)

-- | Hash Plutus 'Data'.
hashData :: Plutus.Data -> PlutusTx.BuiltinByteString
hashData = hashBlake2b_256 . Lazy.toStrict . serialise

-- | Hash 'LedgerBytes'.
hashLedgerBytes :: Plutus.LedgerBytes -> PlutusTx.BuiltinByteString
hashLedgerBytes = hashBlake2b_224 . Plutus.fromBuiltin . Plutus.getLedgerBytes
