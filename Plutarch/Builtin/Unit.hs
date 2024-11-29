{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Builtin.Unit where

import Plutarch.Internal.Term (S, Term, plet, punsafeConstantInternal)
import PlutusCore qualified as PLC

data PUnit (s :: S) = PUnit

punit :: Term s PUnit
punit = punsafeConstantInternal $ PLC.someValue ()

instance Semigroup (Term s PUnit) where
  x <> y = plet x $ \_ -> plet y $ const punit

instance Monoid (Term s PUnit) where
  mempty = punit
