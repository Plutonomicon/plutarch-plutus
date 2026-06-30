{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Array (
  PBArray,
) where

import Data.Kind (Type)
import Data.Vector.Strict (Vector)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Liftable (
  AsHaskell,
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )

-- | @since wip
data PBArray (a :: S -> Type) (s :: S)

type role PBArray representational nominal

-- | @since wip
instance PlutarchType a => PlutarchType (PBArray a) where
  type PRepresentation (PBArray a) = PBArray (PRepresentation a)

-- | @since wip
deriving via
  (PLiftableDirect (PBArray a) (Vector (AsHaskell a)))
  instance
    PLiftable a => PLiftable (PBArray a)
