{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Unit (PUnit (..)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'), Term, pcon)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, (#<), (#<=), (#==))
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Show (PShow (pshow'))

data PUnit s = PUnit

instance PUnsafeLiftDecl PUnit where type PLifted PUnit = ()
deriving via (DerivePConstantDirect () PUnit) instance PConstantDecl ()

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

instance PShow PUnit where
  pshow' _ _ = "()"
