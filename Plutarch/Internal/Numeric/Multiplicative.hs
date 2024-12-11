{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Numeric.Multiplicative (
  -- * Type classes
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Integer (
  PInteger,
  pconstantInteger,
  pmultiplyInteger,
 )
import Plutarch.Internal.Numeric.Additive (
  PPositive,
  pbySquaringDefault,
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeDowncast)

{- | = Laws

@since WIP
-}
class PMultiplicativeSemigroup (a :: S -> Type) where
  {-# INLINEABLE (#*) #-}
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#*) ::
    forall (s :: S).
    PMultiplicativeSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #* y = punsafeDowncast $ pto x #* pto y
  {-# INLINEABLE ppowPositive #-}
  ppowPositive ::
    forall (s :: S).
    Term s (a :--> PPositive :--> a)
  ppowPositive = phoistAcyclic $ plam $ \x p ->
    pbySquaringDefault (#*) # x # p

-- | @since WIP
infix 6 #*

-- | @since WIP
instance PMultiplicativeSemigroup PPositive where
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeCoerce $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PInteger where
  {-# INLINEABLE (#*) #-}
  x #* y = pmultiplyInteger # x # y

{- | = Laws

@since WIP
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a

-- | @since WIP
instance PMultiplicativeMonoid PPositive where
  {-# INLINEABLE pone #-}
  pone = punsafeCoerce $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PInteger where
  {-# INLINEABLE pone #-}
  pone = pconstantInteger 1
