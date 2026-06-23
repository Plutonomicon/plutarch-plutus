module Plutarch.Primitive.Pair (
  PBPair (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

-- | @since wip
data PBPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBPair (Term s a) (Term s b)

-- | @since wip
instance (PlutarchType a, PlutarchType b) => PlutarchType (PBPair a b) where
  type PRepresentation (PBPair a b) = PBPair (PRepresentation a) (PRepresentation b)
