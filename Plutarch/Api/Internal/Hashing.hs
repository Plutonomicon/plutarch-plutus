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
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (fromShort)

import Plutarch.Script (Script (unScript))
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.Builtins qualified as PlutusTx

_plutusHashWith :: HashAlgorithm alg => alg -> ByteString -> PlutusTx.BuiltinByteString
_plutusHashWith alg = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith alg

hashBlake2b_224 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_224 = _plutusHashWith Blake2b_224

hashBlake2b_256 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_256 = _plutusHashWith Blake2b_256

-- | Hash a Script with the given version prefix
hashScriptWithPrefix :: ByteString -> Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash
    . hashBlake2b_224
    $ prefix <> (fromShort . serialiseUPLC . unScript $ scr)

-- | Hash Plutus 'Data'.
hashData :: Plutus.Data -> PlutusTx.BuiltinByteString
hashData = hashBlake2b_256 . toStrict . serialise

-- | Hash 'LedgerBytes'.
hashLedgerBytes :: Plutus.LedgerBytes -> PlutusTx.BuiltinByteString
hashLedgerBytes = hashBlake2b_224 . Plutus.fromBuiltin . Plutus.getLedgerBytes
