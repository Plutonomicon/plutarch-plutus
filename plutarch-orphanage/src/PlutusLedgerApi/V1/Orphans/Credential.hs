{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Credential () where

import PlutusLedgerApi.QuickCheck.Utils (fromAsWord64)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Crypto ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonNegative (NonNegative),
  functionMap,
  oneof,
  variant,
 )

{- | As 'PLA.Credential' is just a wrapper around a hash with a tag, shrinking
this type doesn't make much sense. Therefore we don't do it.

@since 1.0.0
-}
instance Arbitrary PLA.Credential where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.PubKeyCredential <$> arbitrary
      , PLA.ScriptCredential <$> arbitrary
      ]

-- | @since 1.0.0
instance CoArbitrary PLA.Credential where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.PubKeyCredential pkh -> variant (0 :: Int) . coarbitrary pkh
    PLA.ScriptCredential sh -> variant (1 :: Int) . coarbitrary sh

-- | @since 1.0.0
instance Function PLA.Credential where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Credential -> Either PLA.PubKeyHash PLA.ScriptHash
      into = \case
        PLA.PubKeyCredential pkh -> Left pkh
        PLA.ScriptCredential sh -> Right sh
      outOf :: Either PLA.PubKeyHash PLA.ScriptHash -> PLA.Credential
      outOf = \case
        Left pkh -> PLA.PubKeyCredential pkh
        Right sh -> PLA.ScriptCredential sh

-- | @since 1.0.0
instance Arbitrary PLA.StakingCredential where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.StakingHash <$> arbitrary
      , PLA.StakingPtr . fromAsWord64
          <$> arbitrary
          <*> (fromAsWord64 <$> arbitrary)
          <*> (fromAsWord64 <$> arbitrary)
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    -- Since Credentials don't shrink, we don't shrink this case
    PLA.StakingHash _ -> []
    PLA.StakingPtr i j k -> do
      NonNegative i' <- shrink (NonNegative i)
      NonNegative j' <- shrink (NonNegative j)
      NonNegative k' <- shrink (NonNegative k)
      pure . PLA.StakingPtr i' j' $ k'

-- | @since 1.0.0
instance CoArbitrary PLA.StakingCredential where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.StakingHash cred -> variant (0 :: Int) . coarbitrary cred
    PLA.StakingPtr i j k ->
      variant (1 :: Int) . coarbitrary i . coarbitrary j . coarbitrary k

-- | @since 1.0.0
instance Function PLA.StakingCredential where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.StakingCredential -> Either PLA.Credential (Integer, Integer, Integer)
      into = \case
        PLA.StakingHash cred -> Left cred
        PLA.StakingPtr i j k -> Right (i, j, k)
      outOf :: Either PLA.Credential (Integer, Integer, Integer) -> PLA.StakingCredential
      outOf = \case
        Left cred -> PLA.StakingHash cred
        Right (i, j, k) -> PLA.StakingPtr i j k
