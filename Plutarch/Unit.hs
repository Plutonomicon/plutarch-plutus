{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Unit (PUnit (..)) where

import Plutarch.Bool (
  PEq,
  POrd,
  PPartialOrd,
  (#<),
  (#<=),
  (#==),
 )
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue))
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable,
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch')
import Plutarch.Internal.Term (Term, plet)
import Plutarch.Show (PShow (pshow'))

data PUnit s = PUnit

instance PlutusType PUnit where
  type PInner PUnit = PUnit
  pcon' PUnit = pconstant ()
  pmatch' x f = plet x \_ -> f PUnit

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PUnit ())
  instance
    PLiftable PUnit

instance PEq PUnit where
  x #== y = plet x \_ -> plet y \_ -> pcon PTrue

instance PPartialOrd PUnit where
  x #<= y = plet x \_ -> plet y \_ -> pcon PTrue
  x #< y = plet x \_ -> plet y \_ -> pcon PFalse

instance POrd PUnit

instance Semigroup (Term s PUnit) where
  x <> y = plet x \_ -> plet y \_ -> pcon PUnit

instance Monoid (Term s PUnit) where
  mempty = pcon PUnit

instance PShow PUnit where
  pshow' _ x = plet x (const "()")
