module Plutarch.Primitive.List (
  PBList (..),
  pmkCons,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, pforce, punsafeBuiltin)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Function ((:-->))
import PlutusCore qualified as PLC

-- | @since wip
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since wip
instance PlutarchType a => PlutarchType (PBList a) where
  type PRepresentation (PBList a) = PBList (PRepresentation a)

-- | @since wip
pmkCons ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PBList a :--> PBList a)
pmkCons = pforce $ punsafeBuiltin PLC.MkCons
