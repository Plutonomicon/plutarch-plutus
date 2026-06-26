{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Data (
  -- * Types
  PData,
  PAsData,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )
import PlutusTx qualified as PTx

-- | @since wip
data PData (s :: S)

type role PData nominal

-- | @since wip
deriving via (PlutarchTypeRep PData PData) instance PlutarchType PData

-- | @since wip
deriving via (PLiftableDirect PData PTx.Data) instance PLiftable PData

-- | @since wip
data PAsData (a :: S -> Type) (s :: S)

type role PAsData nominal nominal

-- | @since wip
instance PlutarchType (PAsData a) where
  type PRepresentation (PAsData _) = PData
