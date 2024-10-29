{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  -- * Type classes
  PNum (..),
  PIntegral (..),
) where

import Data.Kind (Type)
import Plutarch.Internal.Builtin (
  PInteger,
  pbuiltinAddInteger,
  pbuiltinDivideInteger,
  pbuiltinModInteger,
  pbuiltinMultiplyInteger,
  pbuiltinQuotientInteger,
  pbuiltinRemainderInteger,
  pbuiltinSubtractInteger,
  pif,
  plam,
  pto,
  punsafeDowncast,
 )
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Ord ((#<=))
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Lift (pconstant)

{- | An analogue to 'Num' in Haskell. It is essentially equally lawless;
whatever \'laws\' apply to 'Num' should also apply to this type class's
instances.

@since WIP
-}
class PNum (a :: S -> Type) where
  (#+) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#+) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #+ y = punsafeDowncast $ pto x #+ pto y
  (#-) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#-) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #- y = punsafeDowncast $ pto x #- pto y
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#*) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #* y = punsafeDowncast $ pto x #* pto y
  pnegate :: forall (s :: S). Term s (a :--> a)
  default pnegate :: PNum (PInner a) => Term s (a :--> a)
  pnegate = punsafeCoerce (pnegate :: Term s (PInner a :--> PInner a))
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs :: PNum (PInner a) => Term s (a :--> a)
  pabs = punsafeCoerce (pabs :: Term s (PInner a :--> PInner a))
  psignum :: forall (s :: S). Term s (a :--> a)
  default psignum :: PNum (PInner a) => Term s (a :--> a)
  psignum = punsafeCoerce (psignum :: Term s (PInner a :--> PInner a))
  pfromInteger :: forall (s :: S). Integer -> Term s a
  default pfromInteger :: PNum (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- prohibit mixing arithmetic operators such as
-- (2 #+ 3 #* 4) without explicit precedence.
infix 6 #+
infix 6 #-
infix 6 #*

-- | @since WIP
instance PNum a => Num (Term s a) where
  (+) = (#+)
  (-) = (#-)
  (*) = (#*)
  abs x = pabs # x
  negate x = pnegate # x
  signum x = psignum # x
  fromInteger = pfromInteger

-- | @since WIP
instance PNum PInteger where
  x #+ y = pbuiltinAddInteger # x # y
  x #- y = pbuiltinSubtractInteger # x # y
  x #* y = pbuiltinMultiplyInteger # x # y
  pabs = phoistAcyclic $ plam \x -> pif (x #<= -1) (negate x) x
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #<= 0)
        (-1)
        1
  pfromInteger = pconstant

-- | @since WIP
class PIntegral (a :: S -> Type) where
  pdiv :: forall (s :: S). Term s (a :--> a :--> a)
  default pdiv :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pdiv = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pdiv # pto x # pto y
  pmod :: Term s (a :--> a :--> a)
  default pmod :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pmod = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pmod # pto x # pto y
  pquot :: Term s (a :--> a :--> a)
  default pquot :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pquot = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pquot # pto x # pto y
  prem :: Term s (a :--> a :--> a)
  default prem :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  prem = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ prem # pto x # pto y

-- | @since WIP
instance PIntegral PInteger where
  pdiv = pbuiltinDivideInteger
  pmod = pbuiltinModInteger
  pquot = pbuiltinQuotientInteger
  prem = pbuiltinRemainderInteger
