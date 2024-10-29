{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Bool (
  PSBool (..),
  pmatchStrict,
  pstrue,
  psfalse,
  psif,
  psif',
  psnot,
  psand,
  psand',
  psor,
  psor',
) where

import Plutarch.Internal.Builtin (
  plam,
  pto,
 )
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.Term (
  PType,
  S,
  Term,
  pdelay,
  pforce,
  (#),
  (:-->),
 )

-- | 'PInner' of 'PSBool'.
newtype PSBoolRaw (a :: PType) (s :: S) = PSBoolRaw (Term s (a :--> a :--> a))

instance PlutusType (PSBoolRaw a) where
  type PInner (PSBoolRaw a) = a :--> a :--> a
  pcon' (PSBoolRaw x) = x
  pmatch' x f = f (PSBoolRaw x)

-- | Scott-encoded bool.
data PSBool (s :: S)
  = PSTrue
  | PSFalse
  deriving stock (Eq, Ord, Show)

instance PlutusType PSBool where
  type PInner PSBool = PForall PSBoolRaw
  pcon' PSTrue = pcon $ PForall $ pcon $ PSBoolRaw $ plam const
  pcon' PSFalse = pcon $ PForall $ pcon $ PSBoolRaw $ plam (const id)
  pmatch' x' f =
    pmatch x' $ \(PForall raw) ->
      pmatch raw $ \(PSBoolRaw x) ->
        pforce $ x # pdelay (f PSTrue) # pdelay (f PSFalse)

-- | Strict version of 'pmatch' for 'PSBool'.
pmatchStrict ::
  forall (r :: PType) (s :: S).
  Term s PSBool ->
  (PSBool s -> Term s r) ->
  Term s r
pmatchStrict x' f =
  pmatch (pto x') $ \(PForall raw) ->
    pmatch raw $ \(PSBoolRaw x) ->
      x # f PSTrue # f PSFalse

pstrue :: forall (s :: S). Term s PSBool
pstrue = pcon PSTrue

psfalse :: forall (s :: S). Term s PSBool
psfalse = pcon PSFalse

-- | Strict @if@ on Scott-encoded bool.
psif' :: forall (s :: S) (a :: PType). Term s PSBool -> Term s a -> Term s a -> Term s a
psif' b t f = pmatchStrict b \case
  PSTrue -> t
  PSFalse -> f

-- | Lazy @if@ on Scott-encoded bool.
psif :: forall (s :: S) (a :: PType). Term s PSBool -> Term s a -> Term s a -> Term s a
psif b t f = pforce $ psif' b (pdelay t) (pdelay f)

-- | @not@ on Scott-encoded bool.
psnot :: forall (s :: S). Term s PSBool -> Term s PSBool
psnot b = psif' b psfalse pstrue

psand' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand' a b = psif' a b psfalse

psand :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand a b = psif a b psfalse

psor' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor' a = psif' a pstrue

psor :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor a = psif a pstrue
