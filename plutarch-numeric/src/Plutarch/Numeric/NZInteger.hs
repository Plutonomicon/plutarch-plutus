module Plutarch.Numeric.NZInteger (
  PNZInteger(..)
  ) where

import Plutarch (S, Term, (#))
import Plutarch.Integer (PInteger)
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC
import Plutarch.Bool (PEq ((#==)), POrd ((#<=), (#<)))

-- | @since 1.0
newtype PNZInteger (s :: S) = PNZInteger (Term s PInteger)

-- | @since 1.0
instance PEq PNZInteger where
  {-# INLINEABLE (#==) #-}
  n #== n' = punsafeBuiltin PLC.EqualsInteger # n # n'

-- | @since 1.0
instance POrd PNZInteger where
  {-# INLINEABLE (#<=) #-}
  n #<= n' = punsafeBuiltin PLC.LessThanEqualsInteger # n # n'
  {-# INLINEABLE (#<) #-}
  n #< n' = punsafeBuiltin PLC.LessThanInteger # n # n'
