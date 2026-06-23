{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Needed for safety constraints on coercions
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Apply (
  -- * Type class
  PlutarchType (..),

  -- * Constraints
  PCanRepresent,

  -- * Functions
  (#),
  (#$),
  pcoerce,
  pgeneralize,
  punsafeSpecialize,

  -- * Helpers
  PlutarchTypeRep (..),
) where

import Data.Kind (Constraint, Type)
import GHC.TypeError (
  ErrorMessage (ShowType, Text, (:<>:)),
  TypeError,
 )
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, papp, punsafeCoerce)
import Plutarch.Primitive.Function ((:-->))

-- | @since wip
class PlutarchType (PRepresentation a) => PlutarchType (a :: S -> Type) where
  type PRepresentation a :: S -> Type
  papply ::
    forall (b :: S -> Type) (s :: S).
    Term s (PRepresentation a :--> b) -> Term s a -> Term s b
  papply f x = papp (punsafeCoerce f) (pcoerce x)

-- | @since wip
instance (PlutarchType a, PlutarchType b) => PlutarchType (a :--> b) where
  type PRepresentation (a :--> b) = (a :--> PRepresentation b)

{- | Describes that a given type @a@ can be used to represent another given type
@b@. This may be for one of three reasons:

* @a ~ b@
* @a ~ PRepresentation b@
* There exists some type @c@ such that @PCanRepresent a c@ and @c ~
  PRepresentation b@

You cannot define instances of this directly: for safety, these are derived
exclusively from 'PRepresentation' instances.

This defines a relation termed /representability/. This relation is a partial
order, and thus follows the following laws:

* /Reflexivity:/ any type can represent itself;
* /Anti-symmetry:/ if two types can represent each other, they must be the
  same type;
* /Transitivity:/ if @a `PCanRepresent` b@ and @b `PCanRepresent` c@, then @a
  `PCanRepresent` c@.

@since wip
-}
type family PCanRepresent (a :: S -> Type) (b :: S -> Type) :: Constraint where
  PCanRepresent a b = (CanRepresent' a b ~ Representable, CanRepresentErrorHelper a b (CanRepresent' a b))

-- | @since wip
(#) ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s (a :--> b) -> Term s a -> Term s b
f # x = papply (punsafeCoerce f) x

infixl 8 #

-- | @since wip
(#$) ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s (a :--> b) -> Term s a -> Term s b
(#$) = (#)

infixr 0 #$

{- | Any Plutarch type can \'forget\' any additional structure it may have, and
revert to being its direct representation. This is safe, and has zero runtime cost.

@since wip
-}
pcoerce ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a => Term s a -> Term s (PRepresentation a)
pcoerce = punsafeCoerce

{- | As 'pcoerce', but can \'look through\' any number of \'layers\' of
representations.

@since wip
-}
pgeneralize ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S).
  b `PCanRepresent` a => Term s a -> Term s b
pgeneralize = punsafeCoerce

{- | Given a direct representation of some type @a@, declare unconditionally
that it is a term of type @a@. This is not safe: @a@ may impose additional
structure beyond what its direct representation requires, which can break
Plutarch type system guarantees. It is zero runtime cost, and thus exists for
performance: check carefully that you aren't violating any structural
requirements of @a@!

@since wip
-}
punsafeSpecialize ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a => Term s (PRepresentation a) -> Term s a
punsafeSpecialize = punsafeCoerce

-- | @since wip
newtype PlutarchTypeRep (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PlutarchTypeRep (b s)

-- | @since wip
instance PlutarchType b => PlutarchType (PlutarchTypeRep a b) where
  type PRepresentation (PlutarchTypeRep a b) = b

-- Helpers

-- Defines `PCanRepresent` using something we can match on.
type family CanRepresent' (a :: S -> Type) (b :: S -> Type) :: RepresentationRelation where
  CanRepresent' a a = Representable
  CanRepresent' a b = Helper a b (PRepresentation b)

-- Helper needed to define properly transitive representation relation
type data RepresentationRelation = Representable | NotRepresentable

-- 'Loop helper' for `CanRepresent'`, as otherwise, type family resolution would
-- never end.
type family Helper (a :: S -> Type) (b :: S -> Type) (bi :: S -> Type) :: RepresentationRelation where
  Helper _ b b = NotRepresentable
  Helper a _ bi = CanRepresent' a bi

-- Helper to allow us to provide more useful error messages, instead of type
-- family soup
type family CanRepresentErrorHelper (a :: S -> Type) (b :: S -> Type) (r :: RepresentationRelation) :: Constraint where
  CanRepresentErrorHelper a b NotRepresentable =
    TypeError
      ( 'Text "\""
          ':<>: 'ShowType b
          ':<>: 'Text "\" cannot represent \""
          ':<>: 'ShowType a
          ':<>: 'Text "\""
      )
  CanRepresentErrorHelper _ _ Representable = ()
