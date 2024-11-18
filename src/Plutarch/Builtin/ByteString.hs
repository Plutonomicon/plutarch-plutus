{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Builtin.ByteString (
  -- * Type
  PByteString (..),

  -- * Builtins

  -- ** Manipulation
  pbuiltinAppendByteString,
  pbuiltinConsByteString,
  pbuiltinSliceByteString,
  pbuiltinLengthOfByteString,
  pbuiltinIndexByteString,
  pbuiltinEqualsByteString,
  pbuiltinLessThanByteString,
  pbuiltinLessThanEqualsByteString,

  -- ** Conversion
  pbuiltinByteStringToInteger,
  pbuiltinIntegerToByteString,

  -- ** Logical
  pbuiltinAndByteString,
  pbuiltinOrByteString,
  pbuiltinXorByteString,
  pbuiltinComplementByteString,
  pbuiltinReadBit,
  -- pbuiltinWriteBits,
  pbuiltinReplicateByte,

  -- ** Bitwise
  pbuiltinShiftByteString,
  pbuiltinRotateByteString,
  pbuiltinCountSetBits,
  pbuiltinFindFirstSetBit,
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (
  S,
  Term,
  punsafeBuiltin,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC

{- | A Plutus bytestring.

@since WIP
-}
newtype PByteString (s :: S) = PByteString (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

-- | @since WIP
instance Semigroup (Term s PByteString) where
  {-# INLINEABLE (<>) #-}
  x <> y = pbuiltinAppendByteString # x # y

-- | @since WIP
instance Monoid (Term s PByteString) where
  -- This is morally `pconstant ""`, but we can't depend on the module defining
  -- this function, as it _also_ contains the instance(s) required for PString
  -- to work, which would create a cyclic dependency.
  --
  -- This is a temporary hack that _should_ be obviated by PLiftable, but I'm
  -- putting this in here until this gets resolved.
  --
  -- Koz
  {-# INLINEABLE mempty #-}
  mempty = punsafeConstantInternal $ PLC.someValue @ByteString @PLC.DefaultUni ""

-- | @since WIP
pbuiltinAppendByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString)
pbuiltinAppendByteString = punsafeBuiltin PLC.AppendByteString

-- | @since WIP
pbuiltinConsByteString ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PByteString)
pbuiltinConsByteString = punsafeBuiltin PLC.ConsByteString

-- | @since WIP
pbuiltinSliceByteString ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
pbuiltinSliceByteString = punsafeBuiltin PLC.SliceByteString

-- | @since WIP
pbuiltinLengthOfByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pbuiltinLengthOfByteString = punsafeBuiltin PLC.LengthOfByteString

-- | @since WIP
pbuiltinIndexByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger)
pbuiltinIndexByteString = punsafeBuiltin PLC.IndexByteString

-- | @since WIP
pbuiltinEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinEqualsByteString = punsafeBuiltin PLC.EqualsByteString

-- | @since WIP
pbuiltinLessThanByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinLessThanByteString = punsafeBuiltin PLC.LessThanByteString

-- | @since WIP
pbuiltinLessThanEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinLessThanEqualsByteString = punsafeBuiltin PLC.LessThanEqualsByteString

-- | @since WIP
pbuiltinByteStringToInteger ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PInteger)
pbuiltinByteStringToInteger = punsafeBuiltin PLC.ByteStringToInteger

-- | @since WIP
pbuiltinIntegerToByteString ::
  forall (s :: S).
  Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pbuiltinIntegerToByteString = punsafeBuiltin PLC.IntegerToByteString

-- | @since WIP
pbuiltinAndByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pbuiltinAndByteString = punsafeBuiltin PLC.AndByteString

-- | @since WIP
pbuiltinOrByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pbuiltinOrByteString = punsafeBuiltin PLC.OrByteString

-- | @since WIP
pbuiltinXorByteString ::
  forall (s :: S).
  Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pbuiltinXorByteString = punsafeBuiltin PLC.XorByteString

-- | @since WIP
pbuiltinComplementByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pbuiltinComplementByteString = punsafeBuiltin PLC.ComplementByteString

-- | @since WIP
pbuiltinReadBit ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PBool)
pbuiltinReadBit = punsafeBuiltin PLC.ReadBit

-- TODO: WriteBits

-- | @since WIP
pbuiltinReplicateByte ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString)
pbuiltinReplicateByte = punsafeBuiltin PLC.ReplicateByte

-- | @since WIP
pbuiltinShiftByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
pbuiltinShiftByteString = punsafeBuiltin PLC.ShiftByteString

-- | @since WIP
pbuiltinRotateByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
pbuiltinRotateByteString = punsafeBuiltin PLC.RotateByteString

-- | @since WIP
pbuiltinCountSetBits ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pbuiltinCountSetBits = punsafeBuiltin PLC.CountSetBits

-- | @since WIP
pbuiltinFindFirstSetBit ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pbuiltinFindFirstSetBit = punsafeBuiltin PLC.FindFirstSetBit
