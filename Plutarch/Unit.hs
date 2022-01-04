module Plutarch.Unit (PUnit (..)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'), Term, pcon)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, (#<), (#<=), (#==))
import Plutarch.Lift

data PUnit s = PUnit

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

type instance PDefaultUniType PUnit = ()
