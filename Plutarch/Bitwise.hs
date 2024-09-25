module Plutarch.Bitwise (
  pintegerToByteString,
  pbyteStringToInteger,
  pshiftByteString,
  protateByteString,
  pcountSetBits,
  pfindFirstSetBit,
  pandByteString,
  porByteString,
  pxorByteString,
  pcomplementByteString,
  preadBit,
  pwriteBits,
  preplicateByte,
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

pintegerToByteString :: Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteString = punsafeBuiltin PLC.IntegerToByteString

pbyteStringToInteger :: Term s (PBool :--> PByteString :--> PInteger)
pbyteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

{-
BITWISE
-}

pshiftByteString :: Term s (PByteString :--> PInteger :--> PByteString)
pshiftByteString = punsafeBuiltin PLC.ShiftByteString

protateByteString :: Term s (PByteString :--> PInteger :--> PByteString)
protateByteString = punsafeBuiltin PLC.RotateByteString

pcountSetBits :: Term s (PByteString :--> PInteger)
pcountSetBits = punsafeBuiltin PLC.CountSetBits

pfindFirstSetBit :: Term s (PByteString :--> PInteger)
pfindFirstSetBit = punsafeBuiltin PLC.FindFirstSetBit

{-
LOGICAL
-}

pandByteString :: Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pandByteString = punsafeBuiltin PLC.AndByteString

porByteString :: Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
porByteString = punsafeBuiltin PLC.OrByteString

pxorByteString :: Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pxorByteString = punsafeBuiltin PLC.XorByteString

pcomplementByteString :: Term s (PByteString :--> PByteString)
pcomplementByteString = punsafeBuiltin PLC.ComplementByteString

preadBit :: Term s (PByteString :--> PInteger :--> PBool)
preadBit = punsafeBuiltin PLC.ReadBit

pwriteBits :: Term s (PByteString :--> PBuiltinList PInteger :--> PBuiltinList PBool :--> PByteString)
pwriteBits = punsafeBuiltin PLC.WriteBits

preplicateByte :: Term s (PInteger :--> PInteger :--> PByteString)
preplicateByte = punsafeBuiltin PLC.ReplicateByte

pexpModInteger :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger
