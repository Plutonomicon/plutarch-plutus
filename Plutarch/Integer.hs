module Plutarch.Integer (PInteger(..)) where

import Plutarch.Prelude
import Plutarch.Bool (PEq, (£<), (£==), (£<=), POrd, pIf)
import Plutarch (PlutusType(PInner, pCon', pMatch'))
import qualified PlutusCore as PLC
import Plutarch.Opaque (POpaque)

data PInteger = PInteger (Term POpaque)

instance PlutusType PInteger where
  type PInner PInteger _ = POpaque
  pCon' (PInteger x) = x
  pMatch' x f = f (PInteger x)

instance PEq PInteger where
  x £== y = pUnsafeBuiltin PLC.EqualsInteger £ x £ y

instance POrd PInteger where
  x £<= y = pUnsafeBuiltin PLC.LessThanEqualsInteger £ x £ y
  x £< y = pUnsafeBuiltin PLC.LessThanInteger £ x £ y

instance Num (Term PInteger) where
  x + y = pUnsafeBuiltin PLC.AddInteger £ x £ y
  x - y = pUnsafeBuiltin PLC.SubtractInteger £ x £ y
  x * y = pUnsafeBuiltin PLC.MultiplyInteger £ x £ y
  abs x' = pLet x' $ \x -> pIf (x £<= -1) (negate x) x
  negate x = 0 - x
  signum x' = pLet x' $ \x ->
    pIf (x £== 0)
      0
      $ pIf (x £<= 0)
        (-1)
        1
  fromInteger n = pUnsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniInteger n

