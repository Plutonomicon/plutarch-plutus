{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Pair (
  PBPair (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Liftable (
  AsHaskell,
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )

-- | @since wip
data PBPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBPair (Term s a) (Term s b)

-- | @since wip
instance (PlutarchType a, PlutarchType b) => PlutarchType (PBPair a b) where
  type PRepresentation (PBPair a b) = PBPair (PRepresentation a) (PRepresentation b)

-- | @since wip
deriving via
  (PLiftableDirect (PBPair a b) (AsHaskell a, AsHaskell b))
  instance
    (PLiftable a, PLiftable b) => PLiftable (PBPair a b)
