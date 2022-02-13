module Plutarch.Numeric.Natural (
  PNatural (..)
  ) where

import qualified PlutusCore as PLC
import Plutarch.Bool (PEq ((#==)), POrd ((#<=), (#<)))
import Plutarch (S, Term, (#))
import Plutarch.Integer (PInteger)
import Plutarch.Unsafe (punsafeBuiltin)

-- | @since 1.0
newtype PNatural (s :: S) = PNatural (Term s PInteger)

-- | @since 1.0
instance PEq PNatural where
  {-# INLINEABLE (#==) #-}
  n #== n' = punsafeBuiltin PLC.EqualsInteger # n # n'

-- | @since 1.0
instance POrd PNatural where
  {-# INLINEABLE (#<=) #-}
  n #<= n' = punsafeBuiltin PLC.LessThanEqualsInteger # n # n'
  {-# INLINEABLE (#<) #-}
  n #< n' = punsafeBuiltin PLC.LessThanInteger # n # n'
