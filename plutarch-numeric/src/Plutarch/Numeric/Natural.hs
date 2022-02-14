{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Natural (
  PNatural (..),
  Natural (..),
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

-- | @since 1.0
newtype Natural = Natural Integer
  deriving
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    )
    via Integer

-- | @since 1.0
instance PUnsafeLiftDecl PNatural where
  type PLifted PNatural = Natural

-- | @since 1.0
instance PConstant Natural where
  type PConstantRepr Natural = Integer
  type PConstanted Natural = PNatural
  {-# INLINEABLE pconstantToRepr #-}
  pconstantToRepr (Natural n) = n
  {-# INLINEABLE pconstantFromRepr #-}
  pconstantFromRepr i = do
    guard (i >= 0)
    pure . Natural $ i
