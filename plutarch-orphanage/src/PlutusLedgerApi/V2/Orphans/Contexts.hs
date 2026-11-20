{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V2.Orphans.Contexts () where

import Data.Set qualified as Set
import PlutusLedgerApi.V1.Orphans.Contexts ()
import PlutusLedgerApi.V1.Orphans.DCert ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Tx ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PLA
import PlutusLedgerApi.V2.Orphans.Tx ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonEmptyList (NonEmpty),
  functionMap,
  getNonEmpty,
 )
import Test.QuickCheck.Helpers (neShrink, neShrinkOver, runShrink)

-- | @since 1.0.2
instance Arbitrary PLA.TxInInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxInInfo <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInInfo outref resolved) =
    runShrink (PLA.TxInInfo <$> neShrink outref <*> neShrink resolved)

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

-- | @since 1.0.2
instance Arbitrary PLA.TxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxInfo . getNonEmpty
      <$> arbitrary -- inputs
      <*> arbitrary -- reference inputs
      <*> (getNonEmpty <$> arbitrary) -- outputs
      <*> (Value.getFeeValue <$> arbitrary) -- fee
      <*> (Value.getMintValue <$> arbitrary) -- mint
      <*> arbitrary -- dcert
      <*> arbitrary -- withdrawals
      <*> arbitrary -- valid range
      <*> (Set.toList <$> arbitrary) -- signatures
      <*> arbitrary -- redeemers
      <*> arbitrary -- datums
      <*> arbitrary -- tid
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid) =
    runShrink
      ( PLA.TxInfo
          <$> neShrinkOver @(NonEmptyList PLA.TxInInfo) ins
          <*> neShrink routs
          <*> neShrinkOver @(NonEmptyList PLA.TxOut) outs
          <*> neShrinkOver @Value.FeeValue fee
          <*> neShrinkOver @Value.NonAdaValue mint
          <*> neShrink dcert
          <*> neShrink wdrl
          <*> neShrink validRange
          <*> (Set.toList <$> neShrink (Set.fromList sigs))
          <*> neShrink reds
          <*> neShrink dats
          <*> neShrink tid
      )

-- | @since 1.0.2
instance CoArbitrary PLA.TxInfo where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid) =
    coarbitrary ins
      . coarbitrary routs
      . coarbitrary outs
      . coarbitrary fee
      . coarbitrary mint
      . coarbitrary dcert
      . coarbitrary wdrl
      . coarbitrary validRange
      . coarbitrary sigs
      . coarbitrary reds
      . coarbitrary dats
      . coarbitrary tid

-- | @since 1.0.2
instance Function PLA.TxInfo where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.TxInfo ->
        ([PLA.TxInInfo], [PLA.TxInInfo], [PLA.TxOut], PLA.Value, PLA.Value, ([PLA.DCert], PLA.Map PLA.StakingCredential Integer, PLA.POSIXTimeRange, [PLA.PubKeyHash], PLA.Map PLA.ScriptPurpose PLA.Redeemer, PLA.Map PLA.DatumHash PLA.Datum, PLA.TxId))
      into (PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid) =
        (ins, routs, outs, fee, mint, (dcert, wdrl, validRange, sigs, reds, dats, tid))
      outOf ::
        ([PLA.TxInInfo], [PLA.TxInInfo], [PLA.TxOut], PLA.Value, PLA.Value, ([PLA.DCert], PLA.Map PLA.StakingCredential Integer, PLA.POSIXTimeRange, [PLA.PubKeyHash], PLA.Map PLA.ScriptPurpose PLA.Redeemer, PLA.Map PLA.DatumHash PLA.Datum, PLA.TxId)) ->
        PLA.TxInfo
      outOf (ins, routs, outs, fee, mint, (dcert, wdrl, validRange, sigs, reds, dats, tid)) =
        PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid
