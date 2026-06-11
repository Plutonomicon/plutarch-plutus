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
import Plutarch.Backend.Term (
  S,
  Term,
  punsafeCase,
  punsafeConstant,
  toSomeTerm,
 )
import PlutusCore qualified as PLC

data PBool (s :: S)

pfalse :: forall (s :: S). Term s PBool
pfalse = punsafeConstant $ PLC.someValue False

ptrue :: forall (s :: S). Term s PBool
ptrue = punsafeConstant $ PLC.someValue True

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

pnot ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool
pnot x = pif x pfalse ptrue

pand ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pand x y = pif x y pfalse

por ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
por x = pif x ptrue

pcond ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  [(Term s PBool, Term s a)] ->
  Term s a
pcond = foldl' (\acc (cond, res) -> pif cond res acc)
