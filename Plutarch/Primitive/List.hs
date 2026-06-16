module Plutarch.Primitive.List (
  PBList (..),
  pmkCons,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeBuiltin)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Representation (PRepresentation)
import PlutusCore qualified as PLC

-- | @since wip
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since wip
type instance PRepresentation (PBList a) = PBList (PRepresentation a)

-- | @since wip
pmkCons ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PBList a :--> PBList a)
pmkCons = punsafeBuiltin PLC.MkCons
