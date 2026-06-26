module Plutarch.Primitive.Numeric (
  -- * Types
  PInteger,
  PNatural,
  PPositive,
  PByte,
) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

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
