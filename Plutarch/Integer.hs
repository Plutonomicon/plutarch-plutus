{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Integer (PInteger, PIntegral (..)) where

import Plutarch (punsafeBuiltin)
import Plutarch.Bool (PEq, POrd, pif, (#<), (#<=), (#==))
import Plutarch.Lift
import Plutarch.Prelude
import qualified PlutusCore as PLC

-- | Plutus BuiltinInteger
data PInteger s
  deriving (PLift) via PBuiltinType PInteger Integer

class PIntegral a where
  pdiv :: Term s (a :--> a :--> a)
  pmod :: Term s (a :--> a :--> a)
  pquot :: Term s (a :--> a :--> a)
  prem :: Term s (a :--> a :--> a)

instance PIntegral PInteger where
  pdiv = punsafeBuiltin PLC.DivideInteger
  pmod = punsafeBuiltin PLC.ModInteger
  pquot = punsafeBuiltin PLC.QuotientInteger
  prem = punsafeBuiltin PLC.RemainderInteger

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
  fromInteger = pconstant
