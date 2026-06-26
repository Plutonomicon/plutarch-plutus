{-# LANGUAGE UndecidableInstances #-}

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

import Data.Word (Word8)
import Numeric.Natural (Natural)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  punsafeBuiltin,
  punsafeConstant,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  PlutarchTypeRep (PlutarchTypeRep),
  pcoerce,
 )
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Function ((:-->))
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
