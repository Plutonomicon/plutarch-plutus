module Plutarch.Numeric.NZNatural (
  PNZNatural(..)
  ) where

import Plutarch (S, Term, (#))
import Plutarch.Integer (PInteger)
import qualified PlutusCore as PLC
import Plutarch.Bool (PEq ((#==)), POrd ((#<=), (#<)))
import Plutarch.Unsafe (punsafeBuiltin)

-- | @since 1.0
newtype PNZNatural (s :: S) = PNZNatural (Term s PInteger)

-- | @since 1.0
instance PEq PNZNatural where
  {-# INLINEABLE (#==) #-}
  n #== n' = punsafeBuiltin PLC.EqualsInteger # n # n'

-- | @since 1.0
instance POrd PNZNatural where
  {-# INLINEABLE (#<=) #-}
  n #<= n' = punsafeBuiltin PLC.LessThanEqualsInteger # n # n'
  {-# INLINEABLE (#<) #-}
  n #< n' = punsafeBuiltin PLC.LessThanInteger # n # n'
