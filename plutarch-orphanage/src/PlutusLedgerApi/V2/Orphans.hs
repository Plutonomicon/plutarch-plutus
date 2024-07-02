{-# OPTIONS_GHC -Wno-orphans #-}

-- Note on Function instances
--
-- In many cases, we have hand-rolled instances of Function that merely delegate
-- to an inner type for a newtype. While in theory, this should be
-- via-derivable, because Function relies on an opaque type which we can't
-- coerce through, we have to do this by hand.

-- | QuickCheck orphans (plus a few helpers) for V2 Plutus ledger API types.
module PlutusLedgerApi.V2.Orphans (
  Value.NonAdaValue (..),
  Value.getNonAdaValue,
  Value.UTxOValue (..),
  Value.getUtxoValue,
  Value.FeeValue (..),
  Value.getFeeValue,
) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Set qualified as Set
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.Orphans.Common (
  Blake2b256Hash (Blake2b256Hash),
 )
import PlutusLedgerApi.QuickCheck.Utils (
  fromAsWord64,
 )
import PlutusLedgerApi.V1.Orphans.Address ()
import PlutusLedgerApi.V1.Orphans.Credential ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import PlutusLedgerApi.V1.Orphans.Time ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PLA
import PlutusLedgerApi.V2.Orphans.Tx ()
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonEmptyList (NonEmpty),
  NonNegative (NonNegative),
  functionMap,
  getNonEmpty,
  getNonNegative,
  oneof,
  resize,
  sized,
  variant,
 )
import Test.QuickCheck.Instances.ByteString ()

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance Arbitrary PLA.LedgerBytes

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.LedgerBytes

-- | @since 1.0.0
instance Function PLA.LedgerBytes where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.LedgerBytes

-- | @since 1.0.0
instance Arbitrary PLA.DCert where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.DCertDelegRegKey <$> arbitrary
      , PLA.DCertDelegDeRegKey <$> arbitrary
      , PLA.DCertDelegDelegate <$> arbitrary <*> arbitrary
      , PLA.DCertPoolRegister <$> arbitrary <*> arbitrary
      , PLA.DCertPoolRetire <$> arbitrary <*> (fromAsWord64 <$> arbitrary)
      , pure PLA.DCertGenesis
      , pure PLA.DCertMir
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.DCertDelegRegKey sc -> PLA.DCertDelegRegKey <$> shrink sc
    PLA.DCertDelegDeRegKey sc -> PLA.DCertDelegDeRegKey <$> shrink sc
    -- PubKeyHash can't shrink, so we just pass it through, as otherwise, the
    -- semantics of shrinking would mean the whole think can't shrink.
    PLA.DCertDelegDelegate sc pkh -> PLA.DCertDelegDelegate <$> shrink sc <*> pure pkh
    -- PubKeyHash can't shrink, so neither can this.
    PLA.DCertPoolRegister _ _ -> []
    -- PubKeyHash can't shrink, so we just pass it through, as otherwise, the
    -- semantics of shrinking would mean the whole think can't shrink.
    PLA.DCertPoolRetire pkh e ->
      PLA.DCertPoolRetire pkh . getNonNegative <$> shrink (NonNegative e)
    -- None of the other constructors have any data, so we don't shrink them.
    _ -> []

-- | @since 1.0.0
instance CoArbitrary PLA.DCert where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.DCertDelegRegKey sc -> variant (0 :: Int) . coarbitrary sc
    PLA.DCertDelegDeRegKey sc -> variant (1 :: Int) . coarbitrary sc
    PLA.DCertDelegDelegate sc pkh -> variant (2 :: Int) . coarbitrary sc . coarbitrary pkh
    PLA.DCertPoolRegister pkh pkh' -> variant (3 :: Int) . coarbitrary pkh . coarbitrary pkh'
    PLA.DCertPoolRetire pkh e -> variant (4 :: Int) . coarbitrary pkh . coarbitrary e
    PLA.DCertGenesis -> variant (5 :: Int)
    PLA.DCertMir -> variant (6 :: Int)

-- | @since 1.0.0
instance Function PLA.DCert where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.DCert ->
        Maybe
          ( Maybe
              ( Either
                  PLA.StakingCredential
                  ( Either
                      PLA.StakingCredential
                      ( Either
                          (PLA.StakingCredential, PLA.PubKeyHash)
                          ( Either (PLA.PubKeyHash, PLA.PubKeyHash) (PLA.PubKeyHash, Integer)
                          )
                      )
                  )
              )
          )
      into = \case
        PLA.DCertGenesis -> Nothing
        PLA.DCertMir -> Just Nothing
        PLA.DCertDelegRegKey sc -> Just (Just (Left sc))
        PLA.DCertDelegDeRegKey sc -> Just (Just (Right (Left sc)))
        PLA.DCertDelegDelegate sc pkh -> Just (Just (Right (Right (Left (sc, pkh)))))
        PLA.DCertPoolRegister pkh pkh' -> Just (Just (Right (Right (Right (Left (pkh, pkh'))))))
        PLA.DCertPoolRetire pkh e -> Just (Just (Right (Right (Right (Right (pkh, e))))))
      outOf ::
        Maybe
          ( Maybe
              ( Either
                  PLA.StakingCredential
                  ( Either
                      PLA.StakingCredential
                      ( Either
                          (PLA.StakingCredential, PLA.PubKeyHash)
                          ( Either (PLA.PubKeyHash, PLA.PubKeyHash) (PLA.PubKeyHash, Integer)
                          )
                      )
                  )
              )
          ) ->
        PLA.DCert
      outOf = \case
        Nothing -> PLA.DCertGenesis
        Just Nothing -> PLA.DCertMir
        Just (Just (Left sc)) -> PLA.DCertDelegRegKey sc
        Just (Just (Right (Left sc))) -> PLA.DCertDelegDeRegKey sc
        Just (Just (Right (Right (Left (sc, pkh))))) -> PLA.DCertDelegDelegate sc pkh
        Just (Just (Right (Right (Right (Left (pkh, pkh')))))) -> PLA.DCertPoolRegister pkh pkh'
        Just (Just (Right (Right (Right (Right (pkh, e)))))) -> PLA.DCertPoolRetire pkh e

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

-- | @since 1.0.0
instance Arbitrary PLA.TxOutRef where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxOutRef <$> arbitrary <*> (getNonNegative <$> arbitrary)
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOutRef txI ix) = do
    -- TxId doesn't shrink, so we don't bother
    NonNegative ix' <- shrink (NonNegative ix)
    pure . PLA.TxOutRef txI $ ix'

-- | @since 3.1.0
instance CoArbitrary PLA.TxOutRef where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOutRef txI ix) = coarbitrary txI . coarbitrary ix

-- | @since 3.1.0
instance Function PLA.TxOutRef where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.TxOutRef -> (PLA.TxId, Integer)
      into (PLA.TxOutRef txi ix) = (txi, ix)
      outOf :: (PLA.TxId, Integer) -> PLA.TxOutRef
      outOf (txi, ix) = PLA.TxOutRef txi ix

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

-- | @since 1.0.0
instance Arbitrary PLA.TxInInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxInInfo <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInInfo outRef out) = PLA.TxInInfo <$> shrink outRef <*> shrink out

-- | @since 1.0.0
instance CoArbitrary PLA.TxInInfo where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxInInfo outRef out) = coarbitrary outRef . coarbitrary out

-- | @since 1.0.0
instance Function PLA.TxInInfo where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.TxInInfo outRef out) -> (outRef, out)) (uncurry PLA.TxInInfo)

-- | @since 1.0.0
instance Arbitrary PLA.TxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxInfo . getNonEmpty
      <$> arbitrary
      <*> (getNonEmpty <$> arbitrary)
      <*> arbitrary
      <*> (Value.getFeeValue <$> arbitrary)
      <*> (Value.getNonAdaValue <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (Set.toList <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid) =
    PLA.TxInfo . getNonEmpty
      <$> shrink (NonEmpty ins)
      <*> shrink routs
      <*> (getNonEmpty <$> shrink (NonEmpty outs))
      <*> (Value.getFeeValue <$> shrink (Value.FeeValue fee))
      <*> (Value.getNonAdaValue <$> shrink (Value.NonAdaValue mint))
      <*> shrink dcert
      <*> shrink wdrl
      <*>
      -- Ranges don't shrink anyway
      pure validRange
      <*> (Set.toList <$> shrink (Set.fromList sigs))
      <*> shrink reds
      <*> shrink dats
      <*> shrink tid

-- | @since 1.0.0
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

-- | @since 1.0.0
instance Function PLA.TxInfo where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      -- We have to nest tuples as Function doesn't have instances for anything
      -- bigger than a 6-tuple.
      into ::
        PLA.TxInfo ->
        ( [PLA.TxInInfo]
        , [PLA.TxInInfo]
        , [PLA.TxOut]
        , PLA.Value
        , PLA.Value
        , [PLA.DCert]
        , ( AssocMap.Map PLA.StakingCredential Integer
          , PLA.POSIXTimeRange
          , [PLA.PubKeyHash]
          , AssocMap.Map PLA.ScriptPurpose PLA.Redeemer
          , AssocMap.Map PLA.DatumHash PLA.Datum
          , PLA.TxId
          )
        )
      into (PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid) =
        (ins, routs, outs, fee, mint, dcert, (wdrl, validRange, sigs, reds, dats, tid))
      outOf ::
        ( [PLA.TxInInfo]
        , [PLA.TxInInfo]
        , [PLA.TxOut]
        , PLA.Value
        , PLA.Value
        , [PLA.DCert]
        , ( AssocMap.Map PLA.StakingCredential Integer
          , PLA.POSIXTimeRange
          , [PLA.PubKeyHash]
          , AssocMap.Map PLA.ScriptPurpose PLA.Redeemer
          , AssocMap.Map PLA.DatumHash PLA.Datum
          , PLA.TxId
          )
        ) ->
        PLA.TxInfo
      outOf (ins, routs, outs, fee, mint, dcert, (wdrl, validRange, sigs, reds, dats, tid)) =
        PLA.TxInfo ins routs outs fee mint dcert wdrl validRange sigs reds dats tid

-- | @since 1.0.0
instance Arbitrary PLA.ScriptContext where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.ScriptContext <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.ScriptContext txi purpose) = PLA.ScriptContext <$> shrink txi <*> shrink purpose

-- | @since 1.0.0
instance CoArbitrary PLA.ScriptContext where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.ScriptContext txi purpose) =
    coarbitrary txi . coarbitrary purpose

-- | @since 1.0.0
instance Function PLA.ScriptContext where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.ScriptContext txi purpose) -> (txi, purpose)) (uncurry PLA.ScriptContext)

{- | This is a very general instance, able to produce 'PLC.Data' of basically
any shape. You probably want something more focused than this.

@since 1.0.0
-}
instance Arbitrary PLC.Data where
  {-# INLINEABLE arbitrary #-}
  arbitrary = sized $ \originalSize -> go originalSize originalSize
    where
      -- We have to track our original size (for contents) as well as a
      -- possibly-reduced size (for structure) separately. If we don't do this,
      -- 'leaf' data may end up being far smaller than it should be, biasing the
      -- generator.
      go :: Int -> Int -> Gen PLC.Data
      go originalSize currentSize
        | currentSize <= 0 = oneof [genB originalSize, genI originalSize]
        | otherwise =
            oneof
              [ genB originalSize
              , genI originalSize
              , genConstr originalSize currentSize
              , genList originalSize currentSize
              , genMap originalSize currentSize
              ]
      genB :: Int -> Gen PLA.Data
      genB size = PLC.B <$> resize size arbitrary
      genI :: Int -> Gen PLA.Data
      genI size = PLC.I <$> resize size arbitrary
      genConstr :: Int -> Int -> Gen PLA.Data
      genConstr contentSize structureSize =
        PLA.Constr
          <$> resize contentSize (getNonNegative <$> arbitrary)
          <*> resize structureSize (liftArbitrary . go contentSize $ structureSize `quot` 2)
      genList :: Int -> Int -> Gen PLA.Data
      genList contentSize structureSize =
        PLA.List <$> resize structureSize (liftArbitrary . go contentSize $ structureSize `quot` 2)
      genMap :: Int -> Int -> Gen PLA.Data
      genMap contentSize structureSize = do
        let newStructureSize = structureSize `quot` 2
        PLA.Map
          <$> resize
            structureSize
            ( liftArbitrary $
                (,)
                  <$> go contentSize newStructureSize
                  <*> go contentSize newStructureSize
            )
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLC.I i -> PLC.I <$> shrink i
    PLC.B bs -> PLC.B <$> shrink bs
    PLC.Constr ix dats ->
      PLC.Constr
        <$> (fmap getNonNegative . shrink . NonNegative $ ix)
        <*> shrink dats
    PLC.List ell -> PLC.List <$> shrink ell
    PLC.Map kvs -> PLC.Map <$> shrink kvs

-- | @since 1.0.0
instance CoArbitrary PLC.Data where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLC.I i -> variant (0 :: Int) . coarbitrary i
    PLC.B bs -> variant (1 :: Int) . coarbitrary bs
    PLC.Constr ix dats -> variant (2 :: Int) . coarbitrary ix . coarbitrary dats
    PLC.List ell -> variant (3 :: Int) . coarbitrary ell
    PLC.Map kvs -> variant (4 :: Int) . coarbitrary kvs

-- | @since 1.0.0
instance Function PLC.Data where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLC.Data ->
        Either
          Integer
          ( Either
              ByteString
              ( Either
                  (Integer, [PLA.Data])
                  ( Either [PLA.Data] [(PLA.Data, PLA.Data)]
                  )
              )
          )
      into = \case
        PLC.I i -> Left i
        PLC.B bs -> Right (Left bs)
        PLC.Constr ix dats -> Right (Right (Left (ix, dats)))
        PLC.List ell -> Right (Right (Right (Left ell)))
        PLC.Map kvs -> Right (Right (Right (Right kvs)))
      outOf ::
        Either
          Integer
          ( Either
              ByteString
              ( Either
                  (Integer, [PLA.Data])
                  ( Either [PLA.Data] [(PLA.Data, PLA.Data)]
                  )
              )
          ) ->
        PLA.Data
      outOf = \case
        Left i -> PLC.I i
        Right (Left bs) -> PLC.B bs
        Right (Right (Left (ix, dats))) -> PLC.Constr ix dats
        Right (Right (Right (Left ell))) -> PLC.List ell
        Right (Right (Right (Right kvs))) -> PLC.Map kvs
