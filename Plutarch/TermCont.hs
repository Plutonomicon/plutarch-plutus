{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Plutarch.TermCont (
  TC.hashOpenTerm,
  TC.TermCont (TermCont),
  TC.runTermCont,
  TC.unTermCont,
  TC.tcont,
  TC.pfindPlaceholder,
  pletC,
  pmatchC,
  pletFieldsC,
  ptraceC,
  pguardC,
  pguardC',
  ptryFromC,
  pexpectJustC,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.String (PString)
import Plutarch.DataRepr (HRec, PDataFields, PFields, pletFields)
import Plutarch.DataRepr.Internal.Field (
  BindFields,
  Bindings,
  BoundTerms,
 )
import Plutarch.Internal.PlutusType (PlutusType, pmatch)
import Plutarch.Internal.Term (S, Term, plet)
import Plutarch.Internal.TermCont (TermCont, tcont)
import Plutarch.Internal.TermCont qualified as TC
import Plutarch.Internal.TryFrom (PTryFrom (PTryFromExcess), ptryFrom)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Reducible (Reduce)
import Plutarch.Trace (ptraceInfo, ptraceInfoError)

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
{-# DEPRECATED pletFieldsC "Use the new mechanism instead" #-}

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

-- | 'TermCont' producing version of 'ptryFrom'.
ptryFromC :: forall b r a s. PTryFrom a b => Term s a -> TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromC = tcont . ptryFrom

{- | Escape with a particular value on expecting 'PJust'. For use in monadic context.

@since 1.10.0
-}
pexpectJustC ::
  forall (a :: S -> Type) (r :: S -> Type) (s :: S).
  Term s r ->
  Term s (PMaybe a) ->
  TermCont @r s (Term s a)
pexpectJustC escape ma = tcont $ \f ->
  pmatch ma $ \case
    PJust v -> f v
    PNothing -> escape
