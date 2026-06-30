{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Numeric (
  -- * Types
  PInteger,
  PNatural,
  PPositive,
  PByte,
) where

import Data.Word (Word8)
import Numeric.Natural (Natural)
import Plutarch.Backend.S (S)
import Plutarch.Numeric.Positive (Positive)
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )

-- | @since wip
data PInteger (s :: S)

type role PInteger nominal

-- | @since wip
deriving via (PlutarchTypeRep PInteger PInteger) instance PlutarchType PInteger

-- | @since wip
deriving via (PLiftableDirect PInteger Integer) instance PLiftable PInteger

-- | @since wip
data PNatural (s :: S)

type role PNatural nominal

-- | @since wip
instance PlutarchType PNatural where
  type PRepresentation PNatural = PInteger

-- | @since wip
deriving via
  (PLiftableDirect PNatural Natural)
  instance
    PLiftable PNatural

-- | @since wip
data PPositive (s :: S)

type role PPositive nominal

-- | @since wip
instance PlutarchType PPositive where
  type PRepresentation PPositive = PNatural

-- | @since wip
deriving via
  (PLiftableDirect PPositive Positive)
  instance
    PLiftable PPositive

-- | @since wip
data PByte (s :: S)

type role PByte nominal

-- | @since wip
instance PlutarchType PByte where
  type PRepresentation PByte = PNatural

-- | @since wip
deriving via
  (PLiftableDirect PByte Word8)
  instance
    PLiftable PByte
