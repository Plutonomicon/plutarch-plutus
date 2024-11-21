{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Bool (
  pif,
  pif',
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
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

import Plutarch.Builtin.Bool (PBool (PFalse, PTrue))
import Plutarch.Internal.Other (
  pto,
 )
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.Term (
  PDelayed,
  PType,
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{- | Strict version of 'pif'.
 Emits slightly less code.
-}
pif' :: Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false

-- | Boolean negation for 'PBool' terms.
pnot :: Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x -> pif' # x # pcon PFalse # pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

-- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pand :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # phoistAcyclic (pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand' :: Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # pcon PFalse

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
por :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # phoistAcyclic (pdelay $ pcon PTrue)

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por' :: Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # pcon PTrue

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
