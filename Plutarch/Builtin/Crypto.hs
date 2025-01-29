module Plutarch.Builtin.Crypto (
  -- ** from V1
  psha2_256,
  psha3_256,
  pblake2b_224,
  pblake2b_256,
  pkeccak_256,
  pripemd_160,
  pverifySignature,
  pverifyEd25519Signature,

  -- ** from V2
  pverifyEcdsaSecp256k1Signature,
  pverifySchnorrSecp256k1Signature,
) where

import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Internal.Term (Term, (:-->))
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

-- | Hash a 'PByteString' using SHA-256.
psha2_256 :: Term s (PByteString :--> PByteString)
psha2_256 = punsafeBuiltin PLC.Sha2_256

-- | Hash a 'PByteString' using SHA3-256.
psha3_256 :: Term s (PByteString :--> PByteString)
psha3_256 = punsafeBuiltin PLC.Sha3_256

{- | Hash a 'PByteString' using Blake2B-224.

@since 1.9.0
-}
pblake2b_224 :: Term s (PByteString :--> PByteString)
pblake2b_224 = punsafeBuiltin PLC.Blake2b_224

-- | Hash a 'PByteString' using Blake2B-256.
pblake2b_256 :: Term s (PByteString :--> PByteString)
pblake2b_256 = punsafeBuiltin PLC.Blake2b_256

{- | Hash a 'PByteString' using Keccak-256.

@since 1.9.0
-}
pkeccak_256 :: Term s (PByteString :--> PByteString)
pkeccak_256 = punsafeBuiltin PLC.Keccak_256

{- | Hash a 'PByteString' using Ripemd_160.

@since 1.9.0
-}
pripemd_160 :: Term s (PByteString :--> PByteString)
pripemd_160 = punsafeBuiltin PLC.Ripemd_160

-- | Verify the signature against the public key and message.
pverifySignature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifySignature = pverifyEd25519Signature
{-# DEPRECATED pverifySignature "use one of the Ed25519, Schnorr- or ECDSA Secp256k1 signature verification functions" #-}

{- | Verify an ED25519 signature
   arguments are in this order: pubkey, message, signature
-}
pverifyEd25519Signature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

{- | Verify an ECDSA SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pverifyEcdsaSecp256k1Signature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEcdsaSecp256k1Signature = punsafeBuiltin PLC.VerifyEcdsaSecp256k1Signature

{- | Verify a Schnorr SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pverifySchnorrSecp256k1Signature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifySchnorrSecp256k1Signature = punsafeBuiltin PLC.VerifySchnorrSecp256k1Signature
