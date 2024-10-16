module Plutarch.Bitwise (
  pshiftByteString,
  protateByteString,
  pcountSetBits,
  pfindFirstSetBit,
  preadBit,
  pwriteBits,
  pexpModInteger,
) where

import Plutarch (
  Term,
  type (:-->),
 )
import Plutarch.Bool (PBool)
import Plutarch.Builtin (PBuiltinList)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{-
BITWISE
-}

{- | Shift a 'PByteString', as per
[CIP-123](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
@since 1.9.0
-}
pshiftByteString :: Term s (PByteString :--> PInteger :--> PByteString)
pshiftByteString = punsafeBuiltin PLC.ShiftByteString

{- | Rotate a 'PByteString', as per
[CIP-123](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
@since 1.9.0
-}
protateByteString :: Term s (PByteString :--> PInteger :--> PByteString)
protateByteString = punsafeBuiltin PLC.RotateByteString

{- | Count the set bits in a 'PByteString', as per
[CIP-123](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
@since 1.9.0
-}
pcountSetBits :: Term s (PByteString :--> PInteger)
pcountSetBits = punsafeBuiltin PLC.CountSetBits

{- | Find the lowest index of a set bit in a 'PByteString', as per
[CIP-123](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).

If given a 'PByteString' which consists only of zero bytes (including the empty
'PByteString', this returns @-1@.
@since 1.9.0
-}
pfindFirstSetBit :: Term s (PByteString :--> PInteger)
pfindFirstSetBit = punsafeBuiltin PLC.FindFirstSetBit

{-
LOGICAL
-}

preadBit :: Term s (PByteString :--> PInteger :--> PBool)
preadBit = punsafeBuiltin PLC.ReadBit

pwriteBits :: Term s (PByteString :--> PBuiltinList PInteger :--> PBuiltinList PBool :--> PByteString)
pwriteBits = punsafeBuiltin PLC.WriteBits

pexpModInteger :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger
