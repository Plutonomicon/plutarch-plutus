module Plutarch.Bitwise (
  pintegerToByteString,
  pbyteStringToInteger,
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
CONVERSION
-}

{- | Convert a 'PInteger' into a 'PByteString', as described in
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
The first argument indicates the endianness of the conversion and the third
argument is the integer to be converted, which must be non-negative.  The
second argument must also be non-negative and it indicates the required width
of the output.  If the width is zero then the output is the smallest
bytestring which can contain the converted input (and in this case, the
integer 0 encodes to the empty bytestring).  If the width is nonzero then the
output bytestring will be padded to the required width with 0x00 bytes (on
the left for big-endian conversions and on the right for little-endian
conversions); if the input integer is too big to fit into a bytestring of the
specified width then the conversion will fail.  Conversion will also fail if
the specified width is greater than 8192 or the input integer is too big to
fit into a bytestring of length 8192.
@since 1.9.0
-}
pintegerToByteString :: Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteString = punsafeBuiltin PLC.IntegerToByteString

{- | Convert a 'PByteString' to a 'PInteger', as described in
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
The first argument indicates the endianness of the conversion and the second
is the bytestring to be converted.  There is no limitation on the size of
the bytestring.  The empty bytestring is converted to the integer 0.
@since
-}
pbyteStringToInteger :: Term s (PBool :--> PByteString :--> PInteger)
pbyteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

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
