{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Tx () where

import Data.Coerce (coerce)
import PlutusLedgerApi.Orphans.Common (
  Blake2b256Hash (Blake2b256Hash),
 )
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Address ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonNegative (NonNegative),
  functionMap,
  getNonNegative,
 )

{- | BLAKE2b-256 hash (32 bytes) of a transaction ID.

@since 1.0.0
-}
deriving via Blake2b256Hash instance Arbitrary PLA.TxId

-- | @since 1.0.0
deriving via Blake2b256Hash instance CoArbitrary PLA.TxId

-- | @since 1.0.0
instance Function PLA.TxId where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.TxId

-- | @since 1.0.2
instance Arbitrary PLA.TxOutRef where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxOutRef <$> arbitrary <*> (getNonNegative <$> arbitrary)
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOutRef tid ix) =
    PLA.TxOutRef <$> shrink tid <*> (fmap getNonNegative . shrink . NonNegative $ ix)

-- | @since 1.0.2
instance CoArbitrary PLA.TxOutRef where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOutRef tid ix) =
    coarbitrary tid . coarbitrary ix

-- | @since 1.0.2
instance Function PLA.TxOutRef where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.TxOutRef tid ix) -> (tid, ix)) (uncurry PLA.TxOutRef)

-- | @since 1.0.2
instance Arbitrary PLA.TxOut where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxOut
      <$> arbitrary -- address
      <*> (Value.getUtxoValue <$> arbitrary) -- value
      <*> arbitrary -- maybe datum hash
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOut addr val mdh) = do
    addr' <- shrink addr
    val' <- Value.getUtxoValue <$> shrink (Value.UTxOValue val)
    mdh' <- shrink mdh
    pure . PLA.TxOut addr' val' $ mdh'

-- | @since 1.0.2
instance CoArbitrary PLA.TxOut where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOut addr val mdh) =
    coarbitrary addr . coarbitrary val . coarbitrary mdh

-- | @since 1.0.2
instance Function PLA.TxOut where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.TxOut -> (PLA.Address, PLA.Value, Maybe PLA.DatumHash)
      into (PLA.TxOut addr val mdh) = (addr, val, mdh)
      outOf :: (PLA.Address, PLA.Value, Maybe PLA.DatumHash) -> PLA.TxOut
      outOf (addr, val, mdh) = PLA.TxOut addr val mdh
