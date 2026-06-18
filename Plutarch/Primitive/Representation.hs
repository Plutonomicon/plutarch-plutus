{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | Plutarch contains a notion of representability, to provide an additional
layer of both performance and safety atop \'raw\' UPLC terms (and their
implied types).

@since wip
-}
module Plutarch.Primitive.Representation (
  -- * Type families and constraints
  PRepresentation,
  PCanRepresent,
  PFundamental,
  PIsFundamental,
  PIsNotFundamental,
) where

import Data.Kind (Constraint, Type)
import GHC.TypeError (
  ErrorMessage (ShowType, Text, (:<>:)),
  TypeError,
 )
import Plutarch.Backend.S (S)

{- | The direct representation of a type. Being an instance of this type family
specifies that a given Plutarch type can be represented /somehow/ on the
chain, and the given /other/ Plutarch type specifies what that representation
should be.

As part of this logic, for any type instance @a@, it follows that we can
always convert it to something of type @'PRepresentation' a@ without any
runtime cost. Put another way, @a@ is allowed to be \'stricter\' with what
terms it allows than @'PRepresentation' a@, but not the other way around.

@since wip
-}
type family PRepresentation (a :: S -> Type) :: S -> Type

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

{- | For any Plutarch type @a@, @'PFundamental' a@ represents its \'most
fundamental representation\'. Specifically, it is the type @b@ such that the
following both hold:

* @b `PCanRepresent` a@; and
* There is no type @c@ different to @b@ such that @c `PCanRepresent` b@.

@since wip
-}
type family PFundamental (a :: S -> Type) :: S -> Type where
  PFundamental a = PFundamental' (PRepresentation a) a

{- | Holds for fundamental types only.

@since wip
-}
type family PIsFundamental (a :: S -> Type) :: Constraint where
  PIsFundamental a = PIsFundamental' a (PFundamental a)

{- | Holds only for types that are /not/ fundamental: that is, those types @a@
where @PFundamental a@ is different from @a@.

@since wip
-}
type family PIsNotFundamental (a :: S -> Type) :: Constraint where
  PIsNotFundamental a = PIsNotFundamental' a (PFundamental a)

-- Helpers

-- Error helper for `PIsNotFundamental`
type family PIsNotFundamental' (a :: S -> Type) (b :: S -> Type) :: Constraint where
  PIsNotFundamental' a a = TypeError ('Text "\"" ':<>: 'ShowType a ':<>: 'Text "\" is fundamental.")
  -- Note (Koz, 16/06/2026): Because type families match in order, the holes here
  -- _must_ be of different types, as if they were the same, the previous
  -- line would have caught it.
  PIsNotFundamental' _ _ = ()

-- Error helper for `PIsFundamental`
type family PIsFundamental' (a :: S -> Type) (b :: S -> Type) :: Constraint where
  PIsFundamental' a a = ()
  -- Note (Koz, 16/06/2026): Because type families match in order, the hole here
  -- _must_ be a different type to `a`, as if they were the same, the previous
  -- line would have caught it.
  PIsFundamental' a _ = TypeError ('Text "\"" ':<>: 'ShowType a ':<>: 'Text "\" is not fundamental.")

-- Recursion helper for `PFundamental`
type family PFundamental' (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  PFundamental' a a = a
  -- Note (Koz, 16/06/2026): Because type families match in order, the hole here
  -- _must_ be a different type to `a`, as if they were the same, the previous
  -- line would have caught it.
  PFundamental' a _ = PFundamental' (PRepresentation a) a

-- Helper needed to define properly transitive representation relation
type data RepresentationRelation = Representable | NotRepresentable

-- 'Loop helper' for `CanRepresent'`, as otherwise, type family resolution would
-- never end.
type family Helper (a :: S -> Type) (b :: S -> Type) (bi :: S -> Type) :: RepresentationRelation where
  Helper _ b b = NotRepresentable
  Helper a _ bi = CanRepresent' a bi

-- Defines `PCanRepresent` using something we can match on.
type family CanRepresent' (a :: S -> Type) (b :: S -> Type) :: RepresentationRelation where
  CanRepresent' a a = Representable
  CanRepresent' a b = Helper a b (PRepresentation b)

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
