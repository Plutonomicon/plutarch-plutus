{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Contexts () where

import Data.Set qualified as Set
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.DCert ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Tx ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonEmptyList (NonEmpty),
  functionMap,
  getNonEmpty,
  oneof,
  variant,
 )

-- | @since 1.0.2
instance Arbitrary PLA.TxInInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxInInfo <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInInfo outref resolved) =
    PLA.TxInInfo <$> shrink outref <*> shrink resolved

-- | @since 1.0.2
instance CoArbitrary PLA.TxInInfo where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxInInfo outref resolved) =
    coarbitrary outref . coarbitrary resolved

-- | @since 1.0.2
instance Function PLA.TxInInfo where
  {-# INLINEABLE function #-}
  function =
    functionMap
      (\(PLA.TxInInfo outref resolved) -> (outref, resolved))
      (uncurry PLA.TxInInfo)

-- | @since 1.0.0
instance Arbitrary PLA.TxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxInfo . getNonEmpty
      <$> arbitrary -- inputs
      <*> (getNonEmpty <$> arbitrary) -- outputs
      <*> (Value.getFeeValue <$> arbitrary) -- fee
      <*> (Value.getMintValue <$> arbitrary) -- mint
      <*> arbitrary -- dcert
      <*> arbitrary -- withdrawals
      <*> arbitrary -- valid time range
      <*> (Set.toList <$> arbitrary) -- signatories
      <*> arbitrary -- data
      <*> arbitrary -- tid
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInfo ins outs fee mint dcert wdrl validRange sigs dats tid) = do
    NonEmpty ins' <- shrink (NonEmpty ins)
    NonEmpty outs' <- shrink (NonEmpty outs)
    Value.FeeValue fee' <- shrink (Value.FeeValue fee)
    Value.MintValue mint' <- shrink (Value.MintValue mint)
    dcert' <- shrink dcert
    wdrl' <- shrink wdrl
    validRange' <- shrink validRange
    sigs' <- Set.toList <$> shrink (Set.fromList sigs)
    dats' <- shrink dats
    tid' <- shrink tid
    pure . PLA.TxInfo ins' outs' fee' mint' dcert' wdrl' validRange' sigs' dats' $ tid'

-- | @since 1.0.0
instance CoArbitrary PLA.TxInfo where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxInfo ins outs fee mint dcert wdrl validRange sigs dats tid) =
    coarbitrary ins
      . coarbitrary outs
      . coarbitrary fee
      . coarbitrary mint
      . coarbitrary dcert
      . coarbitrary wdrl
      . coarbitrary validRange
      . coarbitrary sigs
      . coarbitrary dats
      . coarbitrary tid

-- | @since 1.0.0
instance Function PLA.TxInfo where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      -- We have to nest tuples as Function doesn't have instances for anything
      -- bigger than a 6-tuple.
      into ::
        PLA.TxInfo ->
        ([PLA.TxInInfo], [PLA.TxOut], PLA.Value, PLA.Value, [PLA.DCert], ([(PLA.StakingCredential, Integer)], PLA.POSIXTimeRange, [PLA.PubKeyHash], [(PLA.DatumHash, PLA.Datum)], PLA.TxId))
      into (PLA.TxInfo ins outs fee mint dcert wdrl validRange sigs dats tid) =
        (ins, outs, fee, mint, dcert, (wdrl, validRange, sigs, dats, tid))
      outOf ::
        ([PLA.TxInInfo], [PLA.TxOut], PLA.Value, PLA.Value, [PLA.DCert], ([(PLA.StakingCredential, Integer)], PLA.POSIXTimeRange, [PLA.PubKeyHash], [(PLA.DatumHash, PLA.Datum)], PLA.TxId)) ->
        PLA.TxInfo
      outOf (ins, outs, fee, mint, dcert, (wdrl, validRange, sigs, dats, tid)) =
        PLA.TxInfo ins outs fee mint dcert wdrl validRange sigs dats tid

-- | @since 1.0.0
instance Arbitrary PLA.ScriptPurpose where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.Minting <$> arbitrary
      , PLA.Spending <$> arbitrary
      , PLA.Rewarding <$> arbitrary
      , PLA.Certifying <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.Minting cs -> PLA.Minting <$> shrink cs
    PLA.Spending txo -> PLA.Spending <$> shrink txo
    PLA.Rewarding scred -> PLA.Rewarding <$> shrink scred
    PLA.Certifying dcert -> PLA.Certifying <$> shrink dcert

-- | @since 1.0.0
instance CoArbitrary PLA.ScriptPurpose where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.Minting cs -> variant (0 :: Int) . coarbitrary cs
    PLA.Spending txo -> variant (1 :: Int) . coarbitrary txo
    PLA.Rewarding scred -> variant (2 :: Int) . coarbitrary scred
    PLA.Certifying dcert -> variant (3 :: Int) . coarbitrary dcert

-- | @since 1.0.0
instance Function PLA.ScriptPurpose where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.ScriptPurpose ->
        Either PLA.CurrencySymbol (Either PLA.TxOutRef (Either PLA.StakingCredential PLA.DCert))
      into = \case
        PLA.Minting cs -> Left cs
        PLA.Spending txo -> Right (Left txo)
        PLA.Rewarding scred -> Right (Right (Left scred))
        PLA.Certifying dcert -> Right (Right (Right dcert))
      outOf ::
        Either PLA.CurrencySymbol (Either PLA.TxOutRef (Either PLA.StakingCredential PLA.DCert)) ->
        PLA.ScriptPurpose
      outOf = \case
        Left cs -> PLA.Minting cs
        Right (Left txo) -> PLA.Spending txo
        Right (Right (Left scred)) -> PLA.Rewarding scred
        Right (Right (Right dcert)) -> PLA.Certifying dcert
