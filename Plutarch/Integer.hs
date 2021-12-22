module Plutarch.Integer (PInteger) where

import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq, POrd, pif, (#<), (#<=), (#==))
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PInteger s

instance PEq PInteger where
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance POrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance Num (Term s PInteger) where
  x + y = punsafeBuiltin PLC.AddInteger # x # y
  x - y = punsafeBuiltin PLC.SubtractInteger # x # y
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y
  abs x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
  negate x = 0 - x
  signum x' = plet x' $ \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #<= 0)
        (-1)
        1
  fromInteger n = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniInteger n
