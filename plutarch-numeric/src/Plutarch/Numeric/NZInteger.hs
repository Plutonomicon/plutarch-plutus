{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.NZInteger (
  PNZInteger (..),
  NZInteger (..),
) where

import Control.Monad (guard)
import Plutarch (S, Term, (#))
import Plutarch.Bool (PEq ((#==)), POrd ((#<), (#<=)))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (
  PConstant (
    PConstantRepr,
    PConstanted,
    pconstantFromRepr,
    pconstantToRepr
  ),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

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

-- | @since 1.0
newtype NZInteger = NZInteger Integer
  deriving
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    )
    via Integer

-- | @since 1.0
instance PUnsafeLiftDecl PNZInteger where
  type PLifted PNZInteger = NZInteger

-- | @since 1.0
instance PConstant NZInteger where
  type PConstantRepr NZInteger = Integer
  type PConstanted NZInteger = PNZInteger
  {-# INLINEABLE pconstantToRepr #-}
  pconstantToRepr (NZInteger i) = i
  {-# INLINEABLE pconstantFromRepr #-}
  pconstantFromRepr i = do
    guard (i /= 0)
    pure . NZInteger $ i
