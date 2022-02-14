{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.NZNatural (
  PNZNatural (..),
  NZNatural (..),
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

-- | @since 1.0
newtype NZNatural = NZNatural Integer
  deriving
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    )
    via Integer

-- | @since 1.0
instance PUnsafeLiftDecl PNZNatural where
  type PLifted PNZNatural = NZNatural

-- | @since 1.0
instance PConstant NZNatural where
  type PConstantRepr NZNatural = Integer
  type PConstanted NZNatural = PNZNatural
  {-# INLINEABLE pconstantToRepr #-}
  pconstantToRepr (NZNatural i) = i
  {-# INLINEABLE pconstantFromRepr #-}
  pconstantFromRepr i = do
    guard (i > 0)
    pure . NZNatural $ i
