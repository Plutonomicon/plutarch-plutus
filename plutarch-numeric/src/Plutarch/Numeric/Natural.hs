{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Natural (
  PNatural,
  Natural (..),
) where

import Plutarch (S, (#))
import Plutarch.Bool (PEq ((#==)), POrd ((#<), (#<=)))
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

{- | Plutarch version of 'Natural'.

 @since 1.0
-}
data PNatural (s :: S)

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

{- | The natural numbers, specifically \(\mathbb{N}\), which /includes/ zero.

 @since 1.0
-}
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
  pconstantFromRepr i
    | i == 0 = Nothing
    | otherwise = Just . Natural $ i
