{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.BLS (
  PBLS12_381_G1_Element,
  PBLS12_381_G2_Element,
  PBLS12_381_MlResult,
) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS.G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as BLS.Pairing

-- | @since wip
data PBLS12_381_G1_Element (s :: S)

type role PBLS12_381_G1_Element nominal

-- | @since wip
deriving via
  (PlutarchTypeRep PBLS12_381_G1_Element PBLS12_381_G1_Element)
  instance
    PlutarchType PBLS12_381_G1_Element

-- | @since wip
deriving via
  (PLiftableDirect PBLS12_381_G1_Element BLS.G1.Element)
  instance
    PLiftable PBLS12_381_G1_Element

-- | @since wip
data PBLS12_381_G2_Element (s :: S)

type role PBLS12_381_G2_Element nominal

-- | @since wip
deriving via
  (PlutarchTypeRep PBLS12_381_G2_Element PBLS12_381_G2_Element)
  instance
    PlutarchType PBLS12_381_G2_Element

-- | @since wip
deriving via
  (PLiftableDirect PBLS12_381_G2_Element BLS.G2.Element)
  instance
    PLiftable PBLS12_381_G2_Element

-- | @since wip
data PBLS12_381_MlResult (s :: S)

type role PBLS12_381_MlResult nominal

-- | @since wip
deriving via
  (PlutarchTypeRep PBLS12_381_MlResult PBLS12_381_MlResult)
  instance
    PlutarchType PBLS12_381_MlResult

-- | @since wip
deriving via
  (PLiftableDirect PBLS12_381_MlResult BLS.Pairing.MlResult)
  instance
    PLiftable PBLS12_381_MlResult
