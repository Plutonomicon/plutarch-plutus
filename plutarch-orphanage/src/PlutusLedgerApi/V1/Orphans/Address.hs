{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Address () where

import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Credential ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )

-- | @since 1.0.0
instance Arbitrary PLA.Address where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.Address <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  -- As Credential does not shrink, we just pass it through.
  shrink (PLA.Address cred scred) = PLA.Address cred <$> shrink scred

-- | @since 1.0.0
instance CoArbitrary PLA.Address where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Address cred scred) =
    coarbitrary cred . coarbitrary scred

-- | @since 1.0.0
instance Function PLA.Address where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Address -> (PLA.Credential, Maybe PLA.StakingCredential)
      into (PLA.Address cred scred) = (cred, scred)
      outOf :: (PLA.Credential, Maybe PLA.StakingCredential) -> PLA.Address
      outOf (cred, scred) = PLA.Address cred scred
