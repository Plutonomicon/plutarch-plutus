module Plutarch.Primitive.ByteString (
  -- * Type
  PByteString,

  -- * Functions
  pappendByteString,
  pconsByteString,
  psliceByteString,
  plengthOfByteString,
  pindexByteString,
  pequalsByteString,
  plessThanByteString,
  plessThanEqualsByteString,
) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeBuiltin)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PByte, PNatural)
import PlutusCore qualified as PLC

-- | @since wip
data PByteString (s :: S)

type role PByteString nominal

-- | @since wip
instance PlutarchType PByteString where
  type PRepresentation PByteString = PByteString

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
