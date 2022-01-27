module Plutarch.Crypto (
  psha2_256,
  psha3_256,
  pblake2b_256,
  pverifySignature,
) where

import Plutarch (
  Term,
  type (:-->),
 )
import Plutarch.Unsafe (punsafeBuiltin)

-- import Plutarch.Api.V1 (PDatumHash, PPubKey (..), PPubKeyHash (..), PSignature (..))
import Plutarch.Bool (PBool)
import Plutarch.ByteString (PByteString)
import qualified PlutusCore as PLC

-- | Hash a 'PByteString' using SHA-256.
psha2_256 :: Term s (PByteString :--> PByteString)
psha2_256 = punsafeBuiltin PLC.Sha2_256

-- | Hash a 'PByteString' using SHA3-256.
psha3_256 :: Term s (PByteString :--> PByteString)
psha3_256 = punsafeBuiltin PLC.Sha3_256

-- | Hash a 'PByteString' using Blake2B-256.
pblake2b_256 :: Term s (PByteString :--> PByteString)
pblake2b_256 = punsafeBuiltin PLC.Blake2b_256

-- | Verify the signature against the public key and message.
pverifySignature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifySignature = punsafeBuiltin PLC.VerifySignature
