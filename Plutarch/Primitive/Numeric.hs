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
import Plutarch.Backend.Term (punsafeConstant)
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  PlutarchTypeRep (PlutarchTypeRep),
  pcoerce,
 )
import Plutarch.Primitive.Liftable (
  LiftError (ByteOutOfBounds, UnexpectedNegative),
  PLiftable (
    PAsHaskell,
    PAsPlutus,
    haskToRepr,
    plutToRepr,
    reprToHask,
    reprToPlut
  ),
  PLiftableDirect (PLiftableDirect),
 )
import PlutusCore qualified as PLC

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
instance PLiftable PNatural where
  type PAsHaskell PNatural = Natural
  type PAsPlutus PNatural = Integer
  haskToRepr = fromIntegral
  reprToHask x = case signum x of
    (-1) -> Left . UnexpectedNegative $ x
    _ -> Right . fromIntegral $ x
  reprToPlut = punsafeConstant . PLC.someValue @Integer
  plutToRepr t = plutToRepr (pcoerce t)

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
instance PLiftable PByte where
  type PAsHaskell PByte = Word8
  type PAsPlutus PByte = Integer
  haskToRepr = fromIntegral
  reprToHask i
    | 0 <= i && i < 256 = Right . fromIntegral $ i
    | otherwise = Left . ByteOutOfBounds $ i
  reprToPlut = punsafeConstant . PLC.someValue @Integer
  plutToRepr t = plutToRepr (pcoerce (pcoerce t))
