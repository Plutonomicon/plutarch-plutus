{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Newtype (
  DeriveAsNewtype (DeriveAsNewtype, unDeriveAsNewtype),
) where

import Data.Kind (Type)
import GHC.Exts (Any)
import Generics.SOP
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint

import Plutarch.Internal.PlutusType (
  PContravariant',
  PCovariant',
  PInner,
  PVariant',
  PlutusType,
  pcon',
  pmatch',
 )
import Plutarch.Internal.Term (S, Term)

newtype DeriveAsNewtype (a :: S -> Type) s = DeriveAsNewtype {unDeriveAsNewtype :: a s}

type family UnTermSingle (x :: Type) :: S -> Type where
  UnTermSingle (Term _ a) = a

class (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt
instance (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt

instance
  forall (a :: S -> Type) (pt :: S -> Type).
  ( pt ~ UnTermSingle (Head (Head (Code (a Any))))
  , forall s. H s a pt
  ) =>
  PlutusType (DeriveAsNewtype a)
  where
  type PInner (DeriveAsNewtype a) = UnTermSingle (Head (Head (Code (a Any))))
  type PCovariant' (DeriveAsNewtype a) = PCovariant' a
  type PContravariant' (DeriveAsNewtype a) = PContravariant' a
  type PVariant' (DeriveAsNewtype a) = PVariant' a

  -- This breaks without type signature because of (s :: S) needs to be bind.
  pcon' :: forall s. DeriveAsNewtype a s -> Term s (PInner (DeriveAsNewtype a))
  pcon' (DeriveAsNewtype x) =
    case unZ $ unSOP (from x :: SOP I '[ '[Term s pt]]) of
      (I x) :* Nil -> x :: Term s pt

  pmatch' :: forall s b. Term s (PInner (DeriveAsNewtype a)) -> (DeriveAsNewtype a s -> Term s b) -> Term s b
  pmatch' x f =
    f (DeriveAsNewtype $ to ((SOP $ Z $ I x :* Nil) :: SOP I '[ '[Term s pt]]))
