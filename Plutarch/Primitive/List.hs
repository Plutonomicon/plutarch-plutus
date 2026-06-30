{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.List (
  PBList (..),
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
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since wip
instance PlutarchType a => PlutarchType (PBList a) where
  type PRepresentation (PBList a) = PBList (PRepresentation a)

-- | @since wip
deriving via
  (PLiftableDirect (PBList a) [AsHaskell a])
  instance
    PLiftable a => PLiftable (PBList a)
