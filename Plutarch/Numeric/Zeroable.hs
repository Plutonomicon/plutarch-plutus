{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Zeroable (
  -- * Type class
  PZeroable (..),

  -- * Type
  PNZInteger,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeCoerce)
import Plutarch.Numeric.Additive (PAdditiveMonoid)
import Plutarch.Numeric.Helpers (pizero)
import Plutarch.Numeric.Multiplicative (PMultiplicativeMonoid, PMultiplicativeSemigroup)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation), pcoerce, (#))
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (pequalsInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger, PNatural, PPositive)

{- | = Laws

1. @ptoNonZero x x pcoerce@ @=@ @x@

@since wip
-}
class
  (PAdditiveMonoid a, PlutarchType (PNonZero a), PRepresentation (PNonZero a) ~ a) =>
  PZeroable (a :: S -> Type)
  where
  type PNonZero a :: S -> Type
  ptoNonZero ::
    forall (r :: S -> Type) (s :: S).
    Term s a ->
    Term s r ->
    Term s (PNonZero a :--> r) ->
    Term s r

-- | @since wip
instance PZeroable PNatural where
  type PNonZero PNatural = PPositive
  ptoNonZero x whenZero whenNot =
    pif
      (pequalsInteger # pcoerce x # pizero)
      whenZero
      (whenNot # punsafeCoerce x)

-- | @since wip
data PNZInteger (s :: S)

type role PNZInteger nominal

-- | @since wip
instance PlutarchType PNZInteger where
  type PRepresentation PNZInteger = PInteger

-- | @since wip
instance PMultiplicativeSemigroup PNZInteger

-- | @since wip
instance PMultiplicativeMonoid PNZInteger

-- | @since wip
instance PZeroable PInteger where
  type PNonZero PInteger = PNZInteger
  ptoNonZero x whenZero whenNot =
    pif
      (pequalsInteger # x # pizero)
      whenZero
      (whenNot # punsafeCoerce x)
