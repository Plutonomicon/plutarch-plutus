-- Needed for multiple safety constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.BuiltinFun (
  -- * Integers
  paddInteger,
  psubtractInteger,
  pmultiplyInteger,
  pdivideInteger,
  pquotientInteger,
  premainderInteger,
  pmodInteger,
  pequalsInteger,
  plessThanEqualsInteger,
  plessThanInteger,
  pexpModInteger,

  -- * Bytestrings
  pappendByteString,
  pconsByteString,
  psliceByteString,
  plengthOfByteString,
  pindexByteString,
  pequalsByteString,
  plessThanByteString,
  plessThanEqualsByteString,
  preplicateByte,

  -- * Cryptography and hashing
  psha2_256,
  psha3_256,
  pblake2b_256,
  pkeccak_256,
  pblake2b_224,
  pripemd_160,
  pverifyEd25519Signature,
  pverifyEcdsaSecp256k1Signature,
  pverifySchnorrSecp256k1Signature,

  -- * Strings
  pappendString,
  pequalsString,
  pencodeUtf8,
  pdecodeUtf8,

  -- * Data
  pconstrData,
  pmapData,
  plistData,
  piData,
  pbData,
  punConstrData,
  punMapData,
  punListData,
  punIData,
  punBData,
  pequalsData,
  pserialiseData,
  pchooseData,

  -- * Miscellaneous monomorphized constructors
  pmkPairData,

  -- * BLS12-381 operations

  -- ** @G1@
  pbls12_381_G1_add,
  pbls12_381_G1_neg,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_equal,
  pbls12_381_G1_hashToGroup,
  pbls12_381_G1_compress,
  pbls12_381_G1_uncompress,
  pbls12_381_G1_multiScalarMul,

  -- ** @G2@
  pbls12_381_G2_add,
  pbls12_381_G2_neg,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_equal,
  pbls12_381_G2_hashToGroup,
  pbls12_381_G2_compress,
  pbls12_381_G2_uncompress,
  pbls12_381_G2_multiScalarMul,

  -- ** Pairing
  pbls12_381_millerLoop,
  pbls12_381_mulMlResult,
  pbls12_381_finalVerify,

  -- * Conversions
  pintegerToByteString,
  pbyteStringToInteger,
  plistToArray,

  -- * Logical
  pandByteString,
  porByteString,
  pxorByteString,
  pcomplementByteString,
  preadBit,
  pwriteBits,

  -- * Bitwise
  pshiftByteString,
  protateByteString,
  pcountSetBits,
  pfindFirstSetBit,

  -- * Value
  pinsertCoin,
  plookupCoin,
  punionValue,
  pvalueContains,
  pvalueData,
  punValueData,
  pscaleValue,

  -- * Bool
  pifThenElse,

  -- * Tracing
  ptrace,

  -- * Pairs
  pfstPair,
  psndPair,

  -- * Lists
  pchooseList,
  pmkCons,
  pheadList,
  ptailList,
  pnullList,
  pdropList,

  -- * Arrays
  plengthOfArray,
  pindexArray,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, pforce, punsafeBuiltin)
import Plutarch.Primitive.Apply (PCanRepresent)
import Plutarch.Primitive.Array (PBArray)
import Plutarch.Primitive.BLS (
  PBLS12_381_G1_Element,
  PBLS12_381_G2_Element,
  PBLS12_381_MlResult,
 )
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (PByte, PInteger, PNatural)
import Plutarch.Primitive.Pair (PBPair)
import Plutarch.Primitive.String (PString)
import Plutarch.Primitive.Value (PBValue)
import PlutusCore qualified as PLC

-- | @since wip
paddInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
paddInteger = punsafeBuiltin PLC.AddInteger

-- | @since wip
psubtractInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
psubtractInteger = punsafeBuiltin PLC.SubtractInteger

-- | @since wip
pmultiplyInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

-- | @since wip
pdivideInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdivideInteger = punsafeBuiltin PLC.DivideInteger

-- | @since wip
pquotientInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquotientInteger = punsafeBuiltin PLC.QuotientInteger

-- | @since wip
premainderInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
premainderInteger = punsafeBuiltin PLC.RemainderInteger

-- | @since wip
pmodInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmodInteger = punsafeBuiltin PLC.ModInteger

-- | @since wip
pequalsInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pequalsInteger = punsafeBuiltin PLC.EqualsInteger

-- | @since wip
plessThanInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
plessThanInteger = punsafeBuiltin PLC.LessThanInteger

-- | @since wip
plessThanEqualsInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
plessThanEqualsInteger = punsafeBuiltin PLC.LessThanEqualsInteger

-- | @since wip
pexpModInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PNatural :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger

-- | @since wip
pappendByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pappendByteString = punsafeBuiltin PLC.AppendByteString

-- | @since wip
pconsByteString ::
  forall (s :: S).
  Term s (PByte :--> PByteString :--> PByteString)
pconsByteString = punsafeBuiltin PLC.ConsByteString

-- | @since wip
psliceByteString ::
  forall (s :: S).
  Term s (PNatural :--> PNatural :--> PByteString :--> PByteString)
psliceByteString = punsafeBuiltin PLC.SliceByteString

-- | @since wip
plengthOfByteString ::
  forall (s :: S).
  Term s (PByteString :--> PNatural)
plengthOfByteString = punsafeBuiltin PLC.LengthOfByteString

-- | @since wip
pindexByteString ::
  forall (s :: S).
  Term s (PByteString :--> PNatural :--> PByte)
pindexByteString = punsafeBuiltin PLC.IndexByteString

-- | @since wip
pequalsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pequalsByteString = punsafeBuiltin PLC.EqualsByteString

-- | @since wip
plessThanByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
plessThanByteString = punsafeBuiltin PLC.LessThanByteString

-- | @since wip
plessThanEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
plessThanEqualsByteString = punsafeBuiltin PLC.LessThanEqualsByteString

-- | @since wip
preplicateByte :: forall (s :: S). Term s (PNatural :--> PByte :--> PByteString)
preplicateByte = punsafeBuiltin PLC.ReplicateByte

-- | @since wip
psha2_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
psha2_256 = punsafeBuiltin PLC.Sha2_256

-- | @since wip
psha3_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
psha3_256 = punsafeBuiltin PLC.Sha3_256

-- | @since wip
pblake2b_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
pblake2b_256 = punsafeBuiltin PLC.Blake2b_256

-- | @since wip
pkeccak_256 :: forall (s :: S). Term s (PByteString :--> PByteString)
pkeccak_256 = punsafeBuiltin PLC.Keccak_256

-- | @since wip
pblake2b_224 :: forall (s :: S). Term s (PByteString :--> PByteString)
pblake2b_224 = punsafeBuiltin PLC.Blake2b_224

-- | @since wip
pripemd_160 :: forall (s :: S). Term s (PByteString :--> PByteString)
pripemd_160 = punsafeBuiltin PLC.Ripemd_160

-- | @since wip
pverifyEd25519Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

-- | @since wip
pverifyEcdsaSecp256k1Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEcdsaSecp256k1Signature = punsafeBuiltin PLC.VerifyEcdsaSecp256k1Signature

-- | @since wip
pverifySchnorrSecp256k1Signature ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifySchnorrSecp256k1Signature = punsafeBuiltin PLC.VerifySchnorrSecp256k1Signature

-- | @since wip
pappendString ::
  forall (s :: S).
  Term s (PString :--> PString)
pappendString = punsafeBuiltin PLC.AppendString

-- | @since wip
pequalsString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PBool)
pequalsString = punsafeBuiltin PLC.EqualsString

-- | @since wip
pencodeUtf8 ::
  forall (s :: S).
  Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | @since wip
pdecodeUtf8 ::
  forall (s :: S).
  Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

-- | @since wip
piData ::
  forall (s :: S).
  Term s (PInteger :--> PAsData PInteger)
piData = punsafeBuiltin PLC.IData

-- | @since wip
pbData ::
  forall (s :: S).
  Term s (PByteString :--> PAsData PByteString)
pbData = punsafeBuiltin PLC.BData

-- | @since wip
pconstrData ::
  forall (s :: S).
  Term s (PInteger :--> PBList PData :--> PData)
pconstrData = punsafeBuiltin PLC.ConstrData

-- | @since wip
plistData ::
  forall (a :: S -> Type) (s :: S).
  PData `PCanRepresent` a =>
  Term s (PBList a :--> PAsData (PBList (PAsData a)))
plistData = punsafeBuiltin PLC.ListData

-- | @since wip
pmapData ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PData `PCanRepresent` a, PData `PCanRepresent` b) =>
  Term s (PBList (PBPair a b) :--> PAsData (PBList (PAsData (PBPair (PAsData a) (PAsData b)))))
pmapData = punsafeBuiltin PLC.MapData

-- | @since wip
punIData :: forall (s :: S). Term s (PData :--> PInteger)
punIData = punsafeBuiltin PLC.UnIData

-- | @since wip
punBData :: forall (s :: S). Term s (PData :--> PByteString)
punBData = punsafeBuiltin PLC.UnBData

-- | @since wip
punConstrData :: forall (s :: S). Term s (PData :--> PBPair PInteger (PBList PData))
punConstrData = punsafeBuiltin PLC.UnConstrData

-- | @since wip
punListData :: forall (s :: S). Term s (PData :--> PBList PData)
punListData = punsafeBuiltin PLC.UnListData

-- | @since wip
punMapData :: forall (s :: S). Term s (PData :--> PBList (PBPair PData PData))
punMapData = punsafeBuiltin PLC.UnMapData

-- | @since wip
pequalsData ::
  forall (a :: S -> Type) (s :: S).
  PData `PCanRepresent` a =>
  Term s (a :--> a :--> PBool)
pequalsData = punsafeBuiltin PLC.EqualsData

-- | @since wip
pserialiseData :: forall (s :: S). Term s (PData :--> PByteString)
pserialiseData = punsafeBuiltin PLC.SerialiseData

-- | @since wip
pchooseData ::
  forall (a :: S -> Type) (s :: S).
  Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
pchooseData = pforce . punsafeBuiltin $ PLC.ChooseData

-- | @since wip
pmkPairData ::
  forall (s :: S).
  Term s (PData :--> PData :--> PBPair PData PData)
pmkPairData = punsafeBuiltin PLC.MkPairData

-- | @since wip
pbls12_381_G1_add ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

-- | @since wip
pbls12_381_G1_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

-- | @since wip
pbls12_381_G1_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

-- | @since wip
pbls12_381_G1_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBool)
pbls12_381_G1_equal = punsafeBuiltin PLC.Bls12_381_G1_equal

-- | @since wip
pbls12_381_G1_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G1_Element)
pbls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

-- | @since wip
pbls12_381_G1_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PByteString)
pbls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

-- | @since wip
pbls12_381_G1_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G1_Element)
pbls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

-- | @since wip
pbls12_381_G1_multiScalarMul ::
  forall (s :: S).
  Term s (PBList PInteger :--> PBList PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbls12_381_G1_multiScalarMul = punsafeBuiltin PLC.Bls12_381_G1_multiScalarMul

-- | @since wip
pbls12_381_G2_add ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

-- | @since wip
pbls12_381_G2_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

-- | @since wip
pbls12_381_G2_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

-- | @since wip
pbls12_381_G2_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBool)
pbls12_381_G2_equal = punsafeBuiltin PLC.Bls12_381_G2_equal

-- | @since wip
pbls12_381_G2_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G2_Element)
pbls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

-- | @since wip
pbls12_381_G2_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PByteString)
pbls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

-- | @since wip
pbls12_381_G2_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G2_Element)
pbls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

-- | @since wip
pbls12_381_G2_multiScalarMul ::
  forall (s :: S).
  Term s (PBList PInteger :--> PBList PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbls12_381_G2_multiScalarMul = punsafeBuiltin PLC.Bls12_381_G2_multiScalarMul

-- | @since wip
pbls12_381_millerLoop ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_MlResult)
pbls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

-- | @since wip
pbls12_381_mulMlResult ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBLS12_381_MlResult)
pbls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

-- | @since wip
pbls12_381_finalVerify ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBool)
pbls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify

-- | @since wip
pintegerToByteString ::
  forall (s :: S).
  Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteString = punsafeBuiltin PLC.IntegerToByteString

-- | @since wip
pbyteStringToInteger ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PInteger)
pbyteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

-- | @since wip
plistToArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBArray a)
plistToArray = pforce . punsafeBuiltin $ PLC.ListToArray

-- | @since wip
pandByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pandByteString = punsafeBuiltin PLC.AndByteString

-- | @since wip
porByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
porByteString = punsafeBuiltin PLC.OrByteString

-- | @since wip
pxorByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pxorByteString = punsafeBuiltin PLC.XorByteString

-- | @since wip
pcomplementByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pcomplementByteString = punsafeBuiltin PLC.ComplementByteString

-- | @since wip
preadBit ::
  forall (s :: S).
  Term s (PByteString :--> PNatural :--> PBool)
preadBit = punsafeBuiltin PLC.ReadBit

-- | @since wip
pwriteBits ::
  forall (s :: S).
  Term s (PByteString :--> PBList PInteger :--> PBool :--> PByteString)
pwriteBits = punsafeBuiltin PLC.WriteBits

-- | @since wip
pshiftByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
pshiftByteString = punsafeBuiltin PLC.ShiftByteString

-- | @since wip
protateByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
protateByteString = punsafeBuiltin PLC.RotateByteString

-- | @since wip
pcountSetBits ::
  forall (s :: S).
  Term s (PByteString :--> PNatural)
pcountSetBits = punsafeBuiltin PLC.CountSetBits

-- | @since wip
pfindFirstSetBit ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pfindFirstSetBit = punsafeBuiltin PLC.FindFirstSetBit

-- | @since wip
pinsertCoin ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PBValue :--> PBValue)
pinsertCoin = punsafeBuiltin PLC.InsertCoin

-- | @since wip
plookupCoin ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBValue :--> PInteger)
plookupCoin = punsafeBuiltin PLC.LookupCoin

-- | @since wip
punionValue ::
  forall (s :: S).
  Term s (PBValue :--> PBValue :--> PBValue)
punionValue = punsafeBuiltin PLC.UnionValue

-- | @since wip
pvalueContains ::
  forall (s :: S).
  Term s (PBValue :--> PBValue :--> PBool)
pvalueContains = punsafeBuiltin PLC.ValueContains

-- | @since wip
pvalueData ::
  forall (s :: S).
  Term s (PBValue :--> PAsData PBValue)
pvalueData = punsafeBuiltin PLC.ValueData

-- | @since wip
punValueData ::
  forall (s :: S).
  Term s (PData :--> PBValue)
punValueData = punsafeBuiltin PLC.UnValueData

-- | @since wip
pscaleValue ::
  forall (s :: S).
  Term s (PInteger :--> PBValue :--> PBValue)
pscaleValue = punsafeBuiltin PLC.ScaleValue

-- | @since wip
pifThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pifThenElse = pforce . punsafeBuiltin $ PLC.IfThenElse

-- | @since wip
ptrace ::
  forall (a :: S -> Type) (s :: S).
  Term s (PString :--> a :--> a)
ptrace = pforce . punsafeBuiltin $ PLC.Trace

-- | @since wip
pfstPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBPair a b :--> a)
pfstPair = pforce . pforce . punsafeBuiltin $ PLC.FstPair

-- | @since wip
psndPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBPair a b :--> b)
psndPair = pforce . pforce . punsafeBuiltin $ PLC.SndPair

-- | @since wip
pchooseList ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBList a :--> b :--> b :--> b)
pchooseList = pforce . pforce . punsafeBuiltin $ PLC.ChooseList

-- | @since wip
pmkCons ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PBList a :--> PBList a)
pmkCons = pforce $ punsafeBuiltin PLC.MkCons

-- | @since wip
pheadList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> a)
pheadList = pforce $ punsafeBuiltin PLC.HeadList

-- | @since wip
ptailList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBList a)
ptailList = pforce $ punsafeBuiltin PLC.TailList

-- | @since wip
pnullList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBList a :--> PBool)
pnullList = pforce $ punsafeBuiltin PLC.NullList

-- | @since wip
pdropList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PNatural :--> PBList a :--> PBList a)
pdropList = pforce $ punsafeBuiltin PLC.DropList

-- | @since wip
plengthOfArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBArray a :--> PNatural)
plengthOfArray = pforce $ punsafeBuiltin PLC.LengthOfArray

-- | @since wip
pindexArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBArray a :--> PNatural :--> a)
pindexArray = pforce $ punsafeBuiltin PLC.IndexArray
