{-# LANGUAGE AllowAmbiguousTypes #-}
-- ptryFromC uses ptryFrom, but is deprecated itself, but GHC can't figure that
-- out.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Plutarch.TermCont (
  TC.hashOpenTerm,
  TC.TermCont (TermCont),
  TC.runTermCont,
  TC.unTermCont,
  TC.tcont,
  pletC,
  pmatchC,
  pletFieldsC,
  ptraceC,
  pguardC,
  pguardC',
  ptryFromC,
  ptryFromInfoC,
  ptryFromDebugC,
) where

import Data.Kind (Type)
import Plutarch.Bool (PBool, pif)
import Plutarch.DataRepr (HRec, PDataFields, PFields, pletFields)
import Plutarch.DataRepr.Internal.Field (
  BindFields,
  Bindings,
  BoundTerms,
 )
import Plutarch.Internal (S, Term, plet)
import Plutarch.Internal.PlutusType (PlutusType, pmatch)
import Plutarch.Internal.TermCont
import Plutarch.Internal.TermCont qualified as TC
import Plutarch.Reducible (Reduce)
import Plutarch.String (PString)
import Plutarch.Trace (ptraceInfo, ptraceInfoError)
import Plutarch.TryFrom (
  PTryFrom (PTryFromExcess),
  ptryFrom,
  ptryFromDebug,
  ptryFromInfo,
 )

-- | Like `plet` but works in a `TermCont` monad
pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

-- | Like `pletFields` but works in a `TermCont` monad.
pletFieldsC ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ PFields a
  , bs ~ Bindings ps fs
  , BindFields ps bs
  ) =>
  Term s a ->
  TermCont @b s (HRec (BoundTerms ps bs s))
pletFieldsC x = tcont $ pletFields @fs x

{- | Like `ptrace` but works in a `TermCont` monad.

=== Example ===

@
foo :: Term s PUnit
foo = unTermCont $ do
  ptraceC "returning unit!"
  pure $ pconstant ()
@
-}
ptraceC :: Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptraceInfo s (f ())

{- | Trace a message and raise error if 'cond' is false. Otherwise, continue.

=== Example ===

@
onlyAllow42 :: Term s (PInteger :--> PUnit)
onlyAllow42 = plam $ \i -> unTermCont $ do
  pguardC "expected 42" $ i #== 42
  pure $ pconstant ()
@
-}
pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceInfoError s

{- | Stop computation and return given term if 'cond' is false. Otherwise, continue.

=== Example ===

@
is42 :: Term s (PInteger :--> PBool)
is42 = plam $ \i -> unTermCont $ do
  pguardC' (pconstant False) $ i #== 42
  pure $ pconstant True
@
-}
pguardC' :: Term s a -> Term s PBool -> TermCont @a s ()
pguardC' r cond = tcont $ \f -> pif cond (f ()) r

{-# DEPRECATED ptryFromC "Use ptryFromInfoC or ptryFromDebugC" #-}

{- | 'TermCont' producing version of 'ptryFrom'.

@since 1.5.0
-}
ptryFromC :: forall b r a s. PTryFrom a b => Term s a -> TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromC = tcont . ptryFrom

{- | 'TermCont' version of 'ptryFromInfo'.

@since 1.6.0
-}
ptryFromInfoC ::
  forall (b :: S -> Type) (r :: S -> Type) (a :: S -> Type) (s :: S).
  PTryFrom a b =>
  Term s a ->
  Term s PString ->
  TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromInfoC x msg = tcont $ \cb -> ptryFromInfo x msg (curry cb)

{- | 'TermCont' version of 'ptryFromDebug'.

@since 1.6.0
-}
ptryFromDebugC ::
  forall (b :: S -> Type) (r :: S -> Type) (a :: S -> Type) (s :: S).
  PTryFrom a b =>
  Term s a ->
  Term s PString ->
  TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromDebugC x msg = tcont $ \cb -> ptryFromDebug x msg (curry cb)
