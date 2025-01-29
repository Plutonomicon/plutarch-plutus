{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V2.Orphans.Tx () where

import PlutusLedgerApi.V1.Orphans.Address ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PLA
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
  oneof,
  variant,
 )

-- | @since 1.0.0
instance Arbitrary PLA.OutputDatum where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ pure PLA.NoOutputDatum
      , PLA.OutputDatumHash <$> arbitrary
      , PLA.OutputDatum <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  -- We only shrink the OutputDatum case, since the others wouldn't shrink
  -- anyway.
  shrink = \case
    PLA.OutputDatum d -> PLA.OutputDatum <$> shrink d
    _ -> []

-- | @since 1.0.0
instance CoArbitrary PLA.OutputDatum where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.NoOutputDatum -> variant (0 :: Int)
    PLA.OutputDatumHash dh -> variant (1 :: Int) . coarbitrary dh
    PLA.OutputDatum d -> variant (2 :: Int) . coarbitrary d

-- | @since 1.0.0
instance Function PLA.OutputDatum where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.OutputDatum -> Maybe (Either PLA.DatumHash PLA.Datum)
      into = \case
        PLA.NoOutputDatum -> Nothing
        PLA.OutputDatumHash dh -> Just (Left dh)
        PLA.OutputDatum d -> Just (Right d)
      outOf :: Maybe (Either PLA.DatumHash PLA.Datum) -> PLA.OutputDatum
      outOf = \case
        Nothing -> PLA.NoOutputDatum
        Just (Left dh) -> PLA.OutputDatumHash dh
        Just (Right d) -> PLA.OutputDatum d

-- | @since 1.0.0
instance Arbitrary PLA.TxOut where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxOut
      <$> arbitrary
      <*> (Value.getUtxoValue <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOut addr val od msh) =
    PLA.TxOut
      <$> shrink addr
      <*> (Value.getUtxoValue <$> shrink (Value.UTxOValue val))
      <*> shrink od
      <*> shrink msh

-- | @since 1.0.0
instance CoArbitrary PLA.TxOut where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOut addr val od msh) =
    coarbitrary addr . coarbitrary val . coarbitrary od . coarbitrary msh

-- | @since 1.0.0
instance Function PLA.TxOut where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.TxOut ->
        (PLA.Address, PLA.Value, PLA.OutputDatum, Maybe PLA.ScriptHash)
      into (PLA.TxOut addr val od msh) = (addr, val, od, msh)
      outOf ::
        (PLA.Address, PLA.Value, PLA.OutputDatum, Maybe PLA.ScriptHash) ->
        PLA.TxOut
      outOf (addr, val, od, msh) = PLA.TxOut addr val od msh
