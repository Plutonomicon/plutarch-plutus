{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.TryFrom (
  PTryFrom (..),
  ptryFrom,
  PSubtypeRelation (..),
  PSubtype,
  PSubtype',
  pupcast,
  pupcastF,
  pdowncastF,
  ptryFromInfo,
  ptryFromDebug,
) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Plutarch.Internal (PType, S, Term, punsafeCoerce)
import Plutarch.Internal.PlutusType (PContravariant, PCovariant, PInner)
import Plutarch.Internal.Witness (witness)
import Plutarch.Reducible (Reduce)
import Plutarch.String (PString)
import Plutarch.Trace (ptraceDebugError, ptraceInfoError)

import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)

data PSubtypeRelation
  = PSubtypeRelation
  | PNoSubtypeRelation

type family Helper (a :: PType) (b :: PType) (bi :: PType) :: PSubtypeRelation where
  Helper _ b b = 'PNoSubtypeRelation
  Helper a _ bi = PSubtype' a bi

type family PSubtype' (a :: PType) (b :: PType) :: PSubtypeRelation where
  PSubtype' a a = 'PSubtypeRelation
  PSubtype' a b = Helper a b (PInner b)

{- | @PSubtype a b@ constitutes a subtyping relation between @a@ and @b@.
 This concretely means that `\(x :: Term s b) -> punsafeCoerce x :: Term s a`
 is legal and sound.

 You can not make an instance for this yourself.
 You must use the 'PInner' type family of 'PlutusType' to get this instance.

 Caveat: Only @PInner a POpaque@ is considered unfortunately, as otherwise
 getting GHC to figure out the relation with multiple supertypes is quite hard.

 Subtyping is transitive.
-}
type family PSubtypeHelper (a :: PType) (b :: PType) (r :: PSubtypeRelation) :: Constraint where
  PSubtypeHelper a b 'PNoSubtypeRelation =
    TypeError
      ( 'Text "\""
          ':<>: 'ShowType b
          ':<>: 'Text "\""
          ':<>: 'Text " is not a subtype of "
          ':<>: 'Text "\""
          ':<>: 'ShowType a
          ':<>: 'Text "\""
      )
  PSubtypeHelper _ _ 'PSubtypeRelation = ()

type family PSubtype (a :: PType) (b :: PType) :: Constraint where
  PSubtype a b = (PSubtype' a b ~ 'PSubtypeRelation, PSubtypeHelper a b (PSubtype' a b))

{- |
@PTryFrom a b@ represents a subtyping relationship between @a@ and @b@,
and a way to go from @a@ to @b@.
Laws:
- @(punsafeCoerce . fst) <$> tcont (ptryFrom x) ≡ pure x@

@since 1.5.0
-}
class PSubtype a b => PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  type PTryFromExcess a b = PTryFromExcess a (PInner b)

  -- | @since 1.6.0
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s a ->
    Term s r ->
    (Term s b -> Reduce (PTryFromExcess a b s) -> Term s r) ->
    Term s r
  default ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    (PTryFrom a (PInner b), PTryFromExcess a b ~ PTryFromExcess a (PInner b)) =>
    Term s a ->
    Term s r ->
    (Term s b -> Reduce (PTryFromExcess a b s) -> Term s r) ->
    Term s r
  ptryFrom' opq _ f = ptryFrom @(PInner b) @a opq (\(inn, exc) -> f (punsafeCoerce inn) exc)

{-# DEPRECATED ptryFrom "Use ptryFromInfo or ptryFromDebug" #-}

-- | @since 1.5.0
ptryFrom ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S) (r :: S -> Type).
  PTryFrom a b =>
  Term s a ->
  ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) ->
  Term s r
ptryFrom x f = ptryFrom' x (ptraceInfoError "ptryFrom failed") (curry f)

-- | @since 1.6.0
ptryFromInfo ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S) (r :: S -> Type).
  PTryFrom a b =>
  Term s a ->
  Term s PString ->
  (Term s b -> Reduce (PTryFromExcess a b s) -> Term s r) ->
  Term s r
ptryFromInfo x msg = ptryFrom' x (ptraceInfoError msg)

-- | @since 1.6.0
ptryFromDebug ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S) (r :: S -> Type).
  PTryFrom a b =>
  Term s a ->
  Term s PString ->
  (Term s b -> Reduce (PTryFromExcess a b s) -> Term s r) ->
  Term s r
ptryFromDebug x msg = ptryFrom' x (ptraceDebugError msg)

pupcast :: forall a b s. PSubtype a b => Term s b -> Term s a
pupcast = let _ = witness (Proxy @(PSubtype a b)) in punsafeCoerce

pupcastF :: forall a b (p :: PType -> PType) s. (PSubtype a b, PCovariant p) => Proxy p -> Term s (p b) -> Term s (p a)
pupcastF _ =
  let _ = witness (Proxy @(PSubtype a b))
      _ = witness (Proxy @(PCovariant p))
   in punsafeCoerce

pdowncastF :: forall a b (p :: PType -> PType) s. (PSubtype a b, PContravariant p) => Proxy p -> Term s (p a) -> Term s (p b)
pdowncastF _ =
  let _ = witness (Proxy @(PSubtype a b))
      _ = witness (Proxy @(PContravariant p))
   in punsafeCoerce
