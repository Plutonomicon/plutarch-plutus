{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Bool (
  -- * Type
  PBool (..),

  -- * Builtin
  pbuiltinIfThenElse,

  -- * Functions
  pif',
  pif,
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
  pcond,
  ptrue,
  pfalse,
) where

import Data.Kind (Type)
import {-# SOURCE #-} Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  PDelayed,
  RawTerm (RCase),
  S,
  Term (Term),
  TermResult (TermResult),
  asRawTerm,
  pdelay,
  pforce,
  phoistAcyclic,
  punsafeBuiltin,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC

{- | Builtin Plutus boolean.

@since 1.10.0
-}
data PBool (s :: S) = PTrue | PFalse
  deriving stock
    ( -- | @since 1.10.0
      Show
    )

ptrue :: Term (s :: S) PBool
ptrue = punsafeConstantInternal $ PLC.someValue True

pfalse :: Term (s :: S) PBool
pfalse = punsafeConstantInternal $ PLC.someValue False

-- | @since 1.10.0
pbuiltinIfThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> PDelayed a)
pbuiltinIfThenElse = punsafeBuiltin PLC.IfThenElse

{- | Strict if-then-else. Emits slightly less code than the lazy version.

@since 1.10.0
-}
{-# DEPRECATED pif' "Use pif instead" #-}
pif' ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

{- | Lazy if-then-else.

@since 1.10.0
-}
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif cond ifT ifF = Term $ \level -> do
  TermResult condRaw depsCond <- asRawTerm cond level
  TermResult handleFalse depsFalse <- asRawTerm ifF level
  TermResult handleTrue depsTrue <- asRawTerm ifT level
  let allDeps = depsCond <> depsFalse <> depsTrue
  pure . TermResult (RCase condRaw [handleFalse, handleTrue]) $ allDeps

{- | Boolean negation.

@since 1.10.0
-}
pnot ::
  forall (s :: S).
  Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x ->
  pif' # x # pfalse # ptrue

{- | Lazy AND for terms.

@since 1.10.0
-}
(#&&) :: forall (s :: S). Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

infixr 3 #&&

{- | Lazy OR for terms.

@since 1.10.0
-}
(#||) :: forall (s :: S). Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

infixr 2 #||

{- | Hoisted lazy AND at the Plutarch level.

@since 1.10.0
-}
pand :: forall (s :: S). Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # phoistAcyclic (pdelay pfalse)

{- | As 'pand', but strict.

@since 1.10.0
-}
pand' :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # pfalse

{- | Hoisted lazy OR at the Plutarch level.

@since 1.10.0
-}
por :: forall (s :: S). Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # phoistAcyclic (pdelay ptrue)

{- | As 'por', but strict.

@since 1.10.0
-}
por' :: Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # ptrue

{- | Essentially multi-way 'pif'. More precisely, given a list of
condition-action pairs, and an \'action of last resort\', construct a
left-to-right \'chain\' of @pif@s, using the conditions to determine which
action gets taken. The \'action of last resort\' finishes the \'chain\'. For
example:

> pcond [(cond1, act1), (cond2, act2)] act3

does the same thing as

> pif cond1 act1 (pif cond2 act2 act3)

@since 1.10.0
-}
pcond ::
  forall (a :: S -> Type) (s :: S).
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pcond conds lastResort = case conds of
  [] -> lastResort
  (cond, action) : conds' -> pif cond action (pcond conds' lastResort)
