{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Multiplicative (
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  (#*),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, plam', punsafeCoerce)
import Plutarch.Helpers.Numeric (pexpBySquaring, pione, pizero)
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  (#),
 )
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (
  pequalsInteger,
  pmultiplyInteger,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (
  PInteger,
  PNatural,
  PPositive,
 )

-- | @since wip
class PlutarchType a => PMultiplicativeSemigroup (a :: S -> Type) where
  pmultiply :: forall (s :: S). Term s (a :--> a :--> a)
  default pmultiply ::
    forall (s :: S).
    PMultiplicativeSemigroup (PRepresentation a) => Term s (a :--> a :--> a)
  pmultiply = plam' $ \x -> plam' $ \y -> punsafeCoerce (pmultiply # pcoerce x # pcoerce y)
  ppowPositive :: forall (s :: S). Term s (a :--> PPositive :--> a)
  ppowPositive = punsafeCoerce . pexpBySquaring @a $ pmultiply

-- | @since wip
instance PMultiplicativeSemigroup PInteger where
  pmultiply = pmultiplyInteger

-- | @since wip
instance PMultiplicativeSemigroup PNatural

-- | @since wip
instance PMultiplicativeSemigroup PPositive

-- | @since wip
class PMultiplicativeSemigroup a => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a
  default pone ::
    forall (s :: S).
    PMultiplicativeMonoid (PRepresentation a) => Term s a
  pone = punsafeCoerce (pone @(PRepresentation a))
  ppowNatural :: forall (s :: S). Term s (a :--> PNatural :--> a)
  ppowNatural = plam' $ \x -> plam' $ \n ->
    pif
      (pequalsInteger # pizero # pcoerce n)
      pone
      (ppowPositive # x # punsafeCoerce n)

-- | @since wip
instance PMultiplicativeMonoid PInteger where
  pone = pione

-- | @since wip
instance PMultiplicativeMonoid PNatural

-- | @since wip
instance PMultiplicativeMonoid PPositive

-- | @since wip
(#*) ::
  forall (a :: S -> Type) (s :: S).
  PMultiplicativeSemigroup a =>
  Term s a -> Term s a -> Term s a
x #* y = pmultiply # x # y

infix 6 #*
