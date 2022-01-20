{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Deriver.PNewtype (DerivePNewType (DerivePNewType)) where

import Data.Coerce (Coercible, coerce)
import Plutarch (PInner, PlutusType, pcon', pmatch', punsafeCoerce)
import Plutarch.Builtin (PAsData, PIsData, pdata, pfromData)
import Plutarch.Prelude

{- | Facilitates deriving 'PlutusType' and 'PIsData' for newtypes.

For any newtype represented as-
> newtype PFoo (s :: S) = PFoo (Term s PBar)

where 'PBar' has a 'PIsData' instance, you can derive 'PlutusType' and 'PIsData' using-
> deriving (PlutusType, PIsData) via (DerivePNewType PFoo PBar)

This will make 'PFoo' simply be represnted as 'PBar' under the hood.
-}
newtype DerivePNewType a (b :: PType) s = DerivePNewType (a s)

instance (forall (s :: S). Coercible (a s) (Term s b)) => PlutusType (DerivePNewType a b) where
  type PInner (DerivePNewType a b) _ = b
  pcon' (DerivePNewType t) = ptypeInner t
  pmatch' x f = f . DerivePNewType $ ptypeOuter x

instance (forall (s :: S). Coercible (a s) (Term s b), PIsData b) => PIsData (DerivePNewType a b) where
  pfromData x = pcon . DerivePNewType $ ptypeOuter target
    where
      target :: Term _ b
      target = pfromData $ pinnerData x
  pdata x = pouterData . pdata $ pto x

ptypeInner :: forall (x :: PType) y s. Coercible (x s) (Term s y) => x s -> Term s y
ptypeInner = coerce

ptypeOuter :: forall (x :: PType) y s. Coercible (x s) (Term s y) => Term s y -> x s
ptypeOuter = coerce

pinnerData :: Term s (PAsData a) -> Term s (PAsData (PInner a b))
pinnerData = punsafeCoerce

pouterData :: Term s (PAsData (PInner a b)) -> Term s (PAsData a)
pouterData = punsafeCoerce
