{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Unit (PUnit (..)) where

import Plutarch.Builtin.Bool (PBool (PFalse, PTrue))
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable,
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.Ord (POrd (pmax, pmin, (#<), (#<=), (#>=)))
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

-- | @since WIP
instance POrd PUnit where
  {-# INLINEABLE (#<=) #-}
  x #<= y = plet x $ \_ -> plet y $ \_ -> pcon PTrue
  {-# INLINEABLE (#<) #-}
  x #< y = plet x $ \_ -> plet y $ \_ -> pcon PFalse
  {-# INLINEABLE (#>=) #-}
  (#>=) = (#<=)
  {-# INLINEABLE pmax #-}
  pmax x y = plet x $ \_ -> plet y $ const x
  {-# INLINEABLE pmin #-}
  pmin = pmax

instance Semigroup (Term s PUnit) where
  x <> y = plet x \_ -> plet y \_ -> pcon PUnit

instance Monoid (Term s PUnit) where
  mempty = pcon PUnit

instance PShow PUnit where
  pshow' _ x = plet x (const "()")
