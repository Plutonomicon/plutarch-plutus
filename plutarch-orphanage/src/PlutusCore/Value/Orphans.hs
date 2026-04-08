{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since wip
module PlutusCore.Value.Orphans () where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust, maybeToList)
import GHC.Exts (fromListN)
import PlutusCore.Builtin (BuiltinResult (BuiltinSuccess, BuiltinSuccessWithLogs))
import PlutusCore.Value qualified as PlutusCore
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  chooseInt,
  chooseInteger,
  functionMap,
  suchThatMap,
  vectorOf,
 )
import Test.QuickCheck.Instances ()

-- | @since wip
instance Arbitrary PlutusCore.K where
  {-# INLINEABLE arbitrary #-}
  arbitrary = suchThatMap mkBoundedBS PlutusCore.k
  {-# INLINEABLE shrink #-}
  shrink k = do
    bs' <- shrink . PlutusCore.unK $ k
    maybeToList . PlutusCore.k $ bs'

-- | @since wip
instance CoArbitrary PlutusCore.K where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary k = coarbitrary (PlutusCore.unK k)

-- | @since wip
instance Function PlutusCore.K where
  {-# INLINEABLE function #-}
  -- Note (Koz, 08/04/2026): This is safe, as we can never get any input that
  -- isn't valid.
  function = functionMap PlutusCore.unK (fromJust . PlutusCore.k)

-- | @since wip
instance Arbitrary PlutusCore.Quantity where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    suchThatMap
      (chooseInteger (-(2 ^ (127 :: Integer)), 2 ^ (127 :: Integer) - 1))
      PlutusCore.quantity
  {-# INLINEABLE shrink #-}
  shrink q = do
    i' <- shrink . PlutusCore.unQuantity $ q
    maybeToList . PlutusCore.quantity $ i'

-- | @since wip
instance CoArbitrary PlutusCore.Quantity where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary q = coarbitrary (PlutusCore.unQuantity q)

-- | @since wip
instance Function PlutusCore.Quantity where
  {-# INLINEABLE function #-}
  function = functionMap PlutusCore.unQuantity (fromJust . PlutusCore.quantity)

-- | @since wip
instance Arbitrary PlutusCore.Value where
  {-# INLINEABLE arbitrary #-}
  arbitrary = suchThatMap arbitrary (fromResult . PlutusCore.fromList)
  {-# INLINEABLE shrink #-}
  shrink v = do
    let asList = PlutusCore.toList v
    asList' <- shrink asList
    maybeToList . fromResult . PlutusCore.fromList $ asList'

-- | @since wip
instance CoArbitrary PlutusCore.Value where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary x = coarbitrary (PlutusCore.toList x)

-- | @since wip
instance Function PlutusCore.Value where
  {-# INLINEABLE function #-}
  function = functionMap PlutusCore.toList (fromJust . fromResult . PlutusCore.fromList)

-- Helpers

mkBoundedBS :: Gen ByteString
mkBoundedBS = do
  len <- chooseInt (0, 32)
  bytes <- vectorOf len arbitrary
  pure . fromListN len $ bytes

fromResult :: BuiltinResult PlutusCore.Value -> Maybe PlutusCore.Value
fromResult = \case
  BuiltinSuccess x -> Just x
  BuiltinSuccessWithLogs _ x -> Just x
  _ -> Nothing
