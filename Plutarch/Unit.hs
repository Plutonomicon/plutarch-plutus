module Plutarch.Unit (PUnit (..)) where

import Plutarch (POpaque, PlutusType (PInner, pcon', pmatch'), punsafeConstant)
import qualified PlutusCore as PLC

data PUnit s = PUnit

instance PlutusType PUnit where
  type PInner PUnit _ = POpaque
  pcon' PUnit = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniUnit ()
  pmatch' _ f = f PUnit
