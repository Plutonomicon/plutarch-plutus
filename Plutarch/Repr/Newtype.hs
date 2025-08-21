{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Newtype (
  DeriveNewtypePlutusType (DeriveNewtypePlutusType, unDeriveNewtypePlutusType),
) where

import Data.Kind (Type)
import GHC.Exts (Any)
import Generics.SOP (
  Code,
  I (I),
  NP (Nil, (:*)),
  NS (Z),
  SOP (SOP),
 )
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint (Head)
import Plutarch.Internal.PlutusType (
  PInner,
  PlutusType,
  pcon',
  pmatch',
 )
import Plutarch.Internal.Term (S, Term)

-- | @since 1.10.0
newtype DeriveNewtypePlutusType (a :: S -> Type) s = DeriveNewtypePlutusType
  { unDeriveNewtypePlutusType :: a s
  -- ^ @since 1.10.0
  }

-- Helpers

type family UnTermSingle (x :: Type) :: S -> Type where
  UnTermSingle (Term _ a) = a

class (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt
instance (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt

instance
  forall (a :: S -> Type) (pt :: S -> Type).
  ( pt ~ UnTermSingle (Head (Head (Code (a Any))))
  , forall s. H s a pt
  ) =>
  PlutusType (DeriveNewtypePlutusType a)
  where
  type PInner (DeriveNewtypePlutusType a) = UnTermSingle (Head (Head (Code (a Any))))

  -- This breaks without type signature because of (s :: S) needs to be bind.
  pcon' :: forall s. DeriveNewtypePlutusType a s -> Term s (PInner (DeriveNewtypePlutusType a))
  pcon' (DeriveNewtypePlutusType x) =
    case SOP.unZ $ SOP.unSOP (SOP.from x :: SOP I '[ '[Term s pt]]) of
      (I x) :* Nil -> x :: Term s pt

  pmatch' :: forall s b. Term s (PInner (DeriveNewtypePlutusType a)) -> (DeriveNewtypePlutusType a s -> Term s b) -> Term s b
  pmatch' x f =
    f (DeriveNewtypePlutusType $ SOP.to ((SOP $ Z $ I x :* Nil) :: SOP I '[ '[Term s pt]]))
