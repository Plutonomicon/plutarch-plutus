{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Value (PBValue) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )
import PlutusCore.Value (Value)

-- | @since wip
data PBValue (s :: S)

type role PBValue nominal

-- | @since wip
deriving via (PlutarchTypeRep PBValue PBValue) instance PlutarchType PBValue

-- | @since wip
deriving via (PLiftableDirect PBValue Value) instance PLiftable PBValue
