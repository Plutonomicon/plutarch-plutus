module Plutarch.Primitive.Array (
  PBArray,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

-- | @since wip
data PBArray (a :: S -> Type) (s :: S)

type role PBArray representational nominal

-- | @since wip
instance PlutarchType a => PlutarchType (PBArray a) where
  type PRepresentation (PBArray a) = PBArray (PRepresentation a)
