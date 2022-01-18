{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Unit (PUnit (..)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'), Term, pcon)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, (#<), (#<=), (#==))
import Plutarch.Lift (
  DerivePConstantViaCoercible (DerivePConstantViaCoercible),
  PConstant,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )

data PUnit s = PUnit
instance PUnsafeLiftDecl PUnit where type PLifted PUnit = ()
deriving via (DerivePConstantViaCoercible () PUnit ()) instance (PConstant ())

instance PlutusType PUnit where
  type PInner PUnit _ = PUnit
  pcon' PUnit = pconstant ()
  pmatch' _ f = f PUnit

instance PEq PUnit where
  _ #== _ = pcon PTrue

instance POrd PUnit where
  _ #<= _ = pcon PTrue
  _ #< _ = pcon PFalse

instance Semigroup (Term s PUnit) where
  _ <> _ = pcon PUnit

instance Monoid (Term s PUnit) where
  mempty = pcon PUnit
