{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Zeroable (
  -- * Type classes
  PAbs (..),
  PZeroable (..),

  -- * Type
  PNZInteger,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, plam', punsafeCoerce)
import Plutarch.Numeric.Additive (PAdditiveMonoid, pnegate)
import Plutarch.Numeric.Helpers (pizero)
import Plutarch.Numeric.Multiplicative (PMultiplicativeMonoid, PMultiplicativeSemigroup)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation), pcoerce, (#))
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (pequalsInteger, plessThanEqualsInteger)
import Plutarch.Primitive.Eq (PEq)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger, PNatural, PPositive)
import Plutarch.Primitive.Ord (POrd)

{- | = Laws

1. @'pabs' # x #* y@ @=@ @('pabs' # x) #* ('pabs' # y)@
2. @x #* x@ @=@ @('pabs' # x) #* ('pabs' # x)@

Additionally, if @a@ is an 'AdditiveGroup', the following law must hold:

3. @'pabs' # x@ @=@ @'pabs' # ('pnegate' # x)@

@since wip
-}
class PMultiplicativeSemigroup a => PAbs (a :: S -> Type) where
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs ::
    forall (s :: S).
    PAbs (PRepresentation a) =>
    Term s (a :--> a)
  pabs = punsafeCoerce (pabs @(PRepresentation a))

-- | @since wip
instance PAbs PInteger where
  pabs = plam' $ \x ->
    pif
      (plessThanEqualsInteger # x # pizero)
      (pnegate # x)
      x

-- | @since wip
instance PAbs PNatural where
  pabs = plam' id

-- | @since wip
instance PAbs PPositive

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
instance PAbs PNZInteger

-- | @since wip
instance PEq PNZInteger

-- | @since wip
instance POrd PNZInteger

-- | @since wip
instance PZeroable PInteger where
  type PNonZero PInteger = PNZInteger
  ptoNonZero x whenZero whenNot =
    pif
      (pequalsInteger # x # pizero)
      whenZero
      (whenNot # punsafeCoerce x)
