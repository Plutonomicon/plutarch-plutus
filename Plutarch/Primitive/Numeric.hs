module Plutarch.Primitive.Numeric (
  -- * Types
  PInteger,
  PNatural,
  PPositive,
  PByte,

  -- * Primitive operations
  paddInteger,
  psubtractInteger,
  pmultiplyInteger,
  pdivideInteger,
  pquotientInteger,
  premainderInteger,
  pmodInteger,
  pequalsInteger,
  plessThanInteger,
  plessThanEqualsInteger,
) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeBuiltin)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Function ((:-->))
import PlutusCore qualified as PLC

-- | @since wip
data PInteger (s :: S)

type role PInteger nominal

-- | @since wip
instance PlutarchType PInteger where
  type PRepresentation PInteger = PInteger

-- | @since wip
data PNatural (s :: S)

type role PNatural nominal

-- | @since wip
instance PlutarchType PNatural where
  type PRepresentation PNatural = PInteger

-- | @since wip
data PPositive (s :: S)

type role PPositive nominal

-- | @since wip
instance PlutarchType PPositive where
  type PRepresentation PPositive = PNatural

-- | @since wip
data PByte (s :: S)

type role PByte nominal

-- | @since wip
instance PlutarchType PByte where
  type PRepresentation PByte = PNatural

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
