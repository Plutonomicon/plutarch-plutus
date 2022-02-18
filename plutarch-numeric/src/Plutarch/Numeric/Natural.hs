{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Natural (
  PNatural,
  Natural (..),
  toNatural,
  ptoNatural,
  toAbsNatural,
  ptoAbsNatural,
) where

import Plutarch (S, Term, pcon, (#))
import Plutarch.Bool (
  PEq ((#==)),
  POrd ((#<), (#<=)),
  pif,
 )
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
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
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

{- | Convert an 'Integer' to a 'Natural' if it is non-negative.

 @since 1.0
-}
toNatural :: Integer -> Maybe Natural
toNatural i
  | i >= 0 = Just . Natural $ i
  | otherwise = Nothing

{- | As 'toNatural', but for Plutarch 'Term's.

 @since 1.0
-}
ptoNatural ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PMaybe PNatural)
ptoNatural t = pif (0 #<= t) (pcon . PJust . punsafeCoerce $ t) (pcon PNothing)

{- | Convert an 'Integer' to a 'Natural', yielding the absolute value if given a
 negative number.

 @since 1.0
-}
toAbsNatural :: Integer -> Natural
toAbsNatural = Natural . Prelude.abs

{- | As 'toAbsNatural', but for Plutarch 'Term's.

 @since 1.0
-}
ptoAbsNatural ::
  forall (s :: S).
  Term s PInteger ->
  Term s PNatural
ptoAbsNatural t = punsafeCoerce . pif (0 #<= t) t $ abs t
