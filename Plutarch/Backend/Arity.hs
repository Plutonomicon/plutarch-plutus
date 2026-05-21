module Plutarch.Backend.Arity (
  Arity (..),
  getBuiltinArity,
) where

import PlutusCore qualified as PLC

data Arity = NoArity | Arity Word
  deriving stock (Eq, Show)

getBuiltinArity :: PLC.DefaultFun -> Arity
getBuiltinArity =
  Arity . \case
    -- Integers
    PLC.AddInteger -> 2
    PLC.SubtractInteger -> 2
    PLC.MultiplyInteger -> 2
    PLC.DivideInteger -> 2
    PLC.QuotientInteger -> 2
    PLC.RemainderInteger -> 2
    PLC.ModInteger -> 2
    PLC.EqualsInteger -> 2
    PLC.LessThanEqualsInteger -> 2
    PLC.LessThanInteger -> 2
    -- Bytestrings
    PLC.AppendByteString -> 2
    PLC.ConsByteString -> 2
    PLC.SliceByteString -> 3
    PLC.LengthOfByteString -> 1
    PLC.IndexByteString -> 2
    PLC.EqualsByteString -> 2
    PLC.LessThanByteString -> 2
    PLC.LessThanEqualsByteString -> 2
    -- Cryptography and hashes
    PLC.Sha2_256 -> 1
    PLC.Sha3_256 -> 1
    PLC.Blake2b_256 -> 1
    PLC.VerifyEd25519Signature -> 3
    PLC.VerifyEcdsaSecp256k1Signature -> 3
    PLC.VerifySchnorrSecp256k1Signature -> 3
    -- Strings
    PLC.AppendString -> 2
    PLC.EqualsString -> 2
    PLC.EncodeUtf8 -> 1
    PLC.DecodeUtf8 -> 1
    -- Data
    PLC.ConstrData -> 2
    PLC.MapData -> 1
    PLC.ListData -> 1
    PLC.IData -> 1
    PLC.BData -> 1
    PLC.UnConstrData -> 1
    PLC.UnMapData -> 1
    PLC.UnListData -> 1
    PLC.UnIData -> 1
    PLC.UnBData -> 1
    PLC.EqualsData -> 2
    PLC.SerialiseData -> 1
    -- Misc monomorphized constructors
    PLC.MkPairData -> 2
    PLC.MkNilData -> 1
    PLC.MkNilPairData -> 1
    -- BLS operations
    -- G1
    PLC.Bls12_381_G1_add -> 2
    PLC.Bls12_381_G1_neg -> 1
    PLC.Bls12_381_G1_scalarMul -> 2
    PLC.Bls12_381_G1_equal -> 2
    PLC.Bls12_381_G1_hashToGroup -> 2
    PLC.Bls12_381_G1_compress -> 1
    PLC.Bls12_381_G1_uncompress -> 1
    -- G2
    PLC.Bls12_381_G2_add -> 2
    PLC.Bls12_381_G2_neg -> 1
    PLC.Bls12_381_G2_scalarMul -> 2
    PLC.Bls12_381_G2_equal -> 2
    PLC.Bls12_381_G2_hashToGroup -> 2
    PLC.Bls12_381_G2_compress -> 1
    PLC.Bls12_381_G2_uncompress -> 1
    -- Pairing
    PLC.Bls12_381_millerLoop -> 2
    PLC.Bls12_381_mulMlResult -> 2
    PLC.Bls12_381_finalVerify -> 2
    -- Keccak, Blake
    PLC.Keccak_256 -> 1
    PLC.Blake2b_224 -> 1
    -- Conversions
    PLC.IntegerToByteString -> 3
    PLC.ByteStringToInteger -> 2
    -- Logical
    PLC.AndByteString -> 3
    PLC.OrByteString -> 3
    PLC.XorByteString -> 3
    PLC.ComplementByteString -> 1
    PLC.ReadBit -> 2
    PLC.WriteBits -> 3
    PLC.ReplicateByte -> 2
    -- Bitwise
    PLC.ShiftByteString -> 2
    PLC.RotateByteString -> 2
    PLC.CountSetBits -> 1
    PLC.FindFirstSetBit -> 1
    -- Ripemd
    PLC.Ripemd_160 -> 1
    -- Expmod
    PLC.ExpModInteger -> 3
    -- Multi-scalar mult
    PLC.Bls12_381_G1_multiScalarMul -> 2
    PLC.Bls12_381_G2_multiScalarMul -> 2
    -- Value
    PLC.InsertCoin -> 4
    PLC.LookupCoin -> 3
    PLC.UnionValue -> 2
    PLC.ValueContains -> 2
    PLC.ValueData -> 1
    PLC.UnValueData -> 1
    PLC.ScaleValue -> 2
    -- Bool
    PLC.IfThenElse -> 3
    -- Unit
    PLC.ChooseUnit -> 2
    -- Tracing
    PLC.Trace -> 2
    -- Pairs
    PLC.FstPair -> 1
    PLC.SndPair -> 1
    -- Lists
    PLC.ChooseList -> 3
    PLC.MkCons -> 2
    PLC.HeadList -> 1
    PLC.TailList -> 1
    PLC.NullList -> 1
    -- Data
    PLC.ChooseData -> 6
    -- Drop
    PLC.DropList -> 2
    -- Arrays
    PLC.LengthOfArray -> 1
    PLC.ListToArray -> 1
    PLC.IndexArray -> 2
