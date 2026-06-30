{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Bool (
  PBool,
  pfalse,
  ptrue,
  pif,
  pnot,
  pand,
  por,
  pcond,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  punsafeCase,
  punsafeConstant,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (PLiftable, PLiftableDirect (PLiftableDirect))
import PlutusCore qualified as PLC

-- | @since wip
data PBool (s :: S)

type role PBool nominal

-- | @since wip
deriving via (PlutarchTypeRep PBool PBool) instance PlutarchType PBool

-- | @since wip
deriving via (PLiftableDirect PBool Bool) instance PLiftable PBool

-- | @since wip
pfalse :: forall (s :: S). Term s PBool
pfalse = punsafeConstant $ PLC.someValue False

-- | @since wip
ptrue :: forall (s :: S). Term s PBool
ptrue = punsafeConstant $ PLC.someValue True

{- | The canonical if-then-else construct. The first argument is the condition,
second argument is what should happen when the condition is true, third
argument is what should happen when the condition is false.

= Note

Unlike the @IfThenElse@ builtin, this is non-strict. More precisely, only the
branch that gets taken will be evaluated.

@since wip
-}
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif cond ifT ifF =
  let tAsSome = toSomeTerm ifT
      fAsSome = toSomeTerm ifF
      -- This is backwards in UPLC `case`s: false first.
      handlers = NEVector.cons fAsSome $ NEVector.singleton tAsSome
   in punsafeCase cond handlers

{- | Boolean negation.

@since wip
-}
pnot ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool
pnot x = pif x pfalse ptrue

{- | Boolean conjunction. Non-strict in its second argument.

@since wip
-}
pand ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pand x y = pif x y pfalse

{- | Boolean disjunction. Non-strict in its second argument.

@since wip
-}
por ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
por x = pif x ptrue

{- | A multi-way 'pif'. The first argument is the default if no condition
matches. The second argument is a list of condition-action pairs, which will
be tried in order.

This operation is non-strict. Conditions will be evaluated only until one
evaluates true, while only the action associated with a condition that
evaluates true will be evaluated.

@since wip
-}
pcond ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  [(Term s PBool, Term s a)] ->
  Term s a
pcond = foldl' (\acc (cond, res) -> pif cond res acc)
