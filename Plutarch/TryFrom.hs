{-# LANGUAGE UndecidableInstances #-}

module Plutarch.TryFrom (
  PTryFrom (..),
  ptryFrom,
  PSubtype,
  pupcast,
  pupcastF,
  pdowncastF,
  POp (..),
  POpArrow (..),
) where

import Data.Coerce (Coercible)
import Data.Kind (Constraint)
import Data.Proxy (Proxy)

import Plutarch.Internal (punsafeCoerce)
import Plutarch.Internal.Other (
  DerivePNewtype,
  PInner,
  POpaque,
  PType,
  Term,
  (:-->),
 )

import Plutarch.Reducible (Reducible (Reduce))

class PSubtypeDecl (a :: PType) (b :: PType)

-- Not `OVERLAPPABLE` or `OVERLAPPING` on purpose
instance {-# OVERLAPS #-} PSubtypeDecl a a

-- FIXME: Relax subtyping constraint to a @c@ rather than all @c@.
-- This is currently not really possible because a type must have
-- exactly one super type and no more.
instance {-# OVERLAPS #-} (PSubtypeDecl a b, PInner c POpaque ~ b) => PSubtypeDecl a c

{- | @PSubtype a b@ constitutes a subtyping relation between @a@ and @b@.
 This concretely means that `\(x :: Term s b) -> punsafeCoerce x :: Term s a`
 is legal and sound.

 You can not make an instance for this yourself.
 You must use the 'PInner' type family of 'PlutusType' to get this instance.

 Caveat: Only @PInner a POpaque@ is considered unfortunately, as otherwise
 getting GHC to figure out the relation with multiple supertypes is quite hard.

 Subtyping is transitive.
-}
type PSubtype :: PType -> PType -> Constraint
type PSubtype = PSubtypeDecl

{- |
@PTryFrom a b@ represents a subtyping relationship between @a@ and @b@,
and a way to go from @a@ to @b@.
Laws:
- @(punsafeCoerce . fst) <$> tcont (ptryFrom x) ≡ pure x@
-}
class PSubtype a b => PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  ptryFrom' :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r

ptryFrom :: forall b a s r. PTryFrom a b => Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
ptryFrom = ptryFrom'

instance
  ( PTryFrom a b
  , (forall s. Coercible (c s) (Term s b))
  ) =>
  PTryFrom a (DerivePNewtype c b)
  where
  type PTryFromExcess a (DerivePNewtype c b) = PTryFromExcess a b
  ptryFrom' opq f = ptryFrom @b @a opq $ \(inn, exc) -> f (punsafeCoerce inn, exc)

pupcast :: PSubtype a b => Term s b -> Term s a
pupcast = punsafeCoerce

-- FIXME: Add safe way of deriving using `PlutusType`
class PUnsafeContravariantDecl (a :: PType -> PType)
type PContravariant = PUnsafeContravariantDecl

-- FIXME: Add safe way of deriving using `PlutusType`
class PUnsafeCovariantDecl (a :: PType -> PType)
type PCovariant = PUnsafeCovariantDecl -- Really just PFunctor

newtype POpArrow b a s = POpArrow (Term s a -> Term s b)
newtype POp b a s = POp ((a :--> b) s)

instance PUnsafeContravariantDecl (POpArrow b)
instance PUnsafeContravariantDecl (POp b)
instance PUnsafeCovariantDecl ((:-->) a)

pupcastF :: forall a b (p :: PType -> PType) s. (PSubtype a b, PCovariant p) => Proxy p -> Term s (p b) -> Term s (p a)
pupcastF _ = punsafeCoerce

pdowncastF :: forall a b (p :: PType -> PType) s. (PSubtype a b, PContravariant p) => Proxy p -> Term s (p a) -> Term s (p b)
pdowncastF _ = punsafeCoerce
