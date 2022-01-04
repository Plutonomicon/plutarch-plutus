module Plutarch.Unit (PUnit (..)) where

import Plutarch (POpaque, PlutusType (PInner, pcon', pmatch'), Term, pcon, punsafeConstant)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, (#<), (#<=), (#==))
import Plutarch.Lift
import qualified PlutusCore as PLC

data PUnit s = PUnit

instance PlutusType PUnit where
  type PInner PUnit _ = POpaque
  pcon' PUnit = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniUnit ()
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

instance PDefaultUni PUnit where
  type PDefaultUniType PUnit = ()
