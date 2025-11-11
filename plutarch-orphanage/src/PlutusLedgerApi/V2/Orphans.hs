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
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.V1.Orphans.Address ()
import PlutusLedgerApi.V1.Orphans.Credential ()
import PlutusLedgerApi.V1.Orphans.DCert ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import PlutusLedgerApi.V1.Orphans.Time ()
import PlutusLedgerApi.V1.Orphans.Tx ()
import PlutusLedgerApi.V1.Orphans.Value ()
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PLA
import PlutusLedgerApi.V2.Orphans.Contexts ()
import PlutusLedgerApi.V2.Orphans.Tx ()
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonNegative (NonNegative),
  functionMap,
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
                  (Either [PLA.Data] [(PLA.Data, PLA.Data)])
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
                  (Either [PLA.Data] [(PLA.Data, PLA.Data)])
              )
          ) ->
        PLA.Data
      outOf = \case
        Left i -> PLC.I i
        Right (Left bs) -> PLC.B bs
        Right (Right (Left (ix, dats))) -> PLC.Constr ix dats
        Right (Right (Right (Left ell))) -> PLC.List ell
        Right (Right (Right (Right kvs))) -> PLC.Map kvs
