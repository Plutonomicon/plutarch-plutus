module Plutarch.Primitive.List (
  PBList (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

-- | @since wip
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since wip
instance PlutarchType a => PlutarchType (PBList a) where
  type PRepresentation (PBList a) = PBList (PRepresentation a)
