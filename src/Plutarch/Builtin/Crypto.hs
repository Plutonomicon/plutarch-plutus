module Plutarch.Builtin.Crypto (
  pbuiltinSha2_256,
  pbuiltinSha3_256,
  pbuiltinBlake2b_224,
  pbuiltinBlake2b_256,
  pbuiltinKeccak_256,
  pbuiltinRipemd_160,
  pbuiltinVerifyEd25519Signature,
  pbuiltinVerifyEcdsaSecp256k1Signature,
  pbuiltinVerifySchnorrSecp256k1Signature,
) where

import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Internal.Term (Term, punsafeBuiltin, (:-->))
import PlutusCore qualified as PLC

-- | Hash a 'PByteString' using SHA-256.
pbuiltinSha2_256 :: Term s (PByteString :--> PByteString)
pbuiltinSha2_256 = punsafeBuiltin PLC.Sha2_256

-- | Hash a 'PByteString' using SHA3-256.
pbuiltinSha3_256 :: Term s (PByteString :--> PByteString)
pbuiltinSha3_256 = punsafeBuiltin PLC.Sha3_256

{- | Hash a 'PByteString' using Blake2B-224.

@since 1.9.0
-}
pbuiltinBlake2b_224 :: Term s (PByteString :--> PByteString)
pbuiltinBlake2b_224 = punsafeBuiltin PLC.Blake2b_224

-- | Hash a 'PByteString' using Blake2B-256.
pbuiltinBlake2b_256 :: Term s (PByteString :--> PByteString)
pbuiltinBlake2b_256 = punsafeBuiltin PLC.Blake2b_256

{- | Hash a 'PByteString' using Keccak-256.

@since 1.9.0
-}
pbuiltinKeccak_256 :: Term s (PByteString :--> PByteString)
pbuiltinKeccak_256 = punsafeBuiltin PLC.Keccak_256

{- | Hash a 'PByteString' using Ripemd_160.

@since 1.9.0
-}
pbuiltinRipemd_160 :: Term s (PByteString :--> PByteString)
pbuiltinRipemd_160 = punsafeBuiltin PLC.Ripemd_160

{- | Verify an ED25519 signature
   arguments are in this order: pubkey, message, signature
-}
pbuiltinVerifyEd25519Signature ::
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pbuiltinVerifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

{- | Verify an ECDSA SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pbuiltinVerifyEcdsaSecp256k1Signature ::
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pbuiltinVerifyEcdsaSecp256k1Signature = punsafeBuiltin PLC.VerifyEcdsaSecp256k1Signature

{- | Verify a Schnorr SECP256k1 signature
   arguments are in this order: pubkey, message, signature
-}
pbuiltinVerifySchnorrSecp256k1Signature ::
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pbuiltinVerifySchnorrSecp256k1Signature =
  punsafeBuiltin PLC.VerifySchnorrSecp256k1Signature
