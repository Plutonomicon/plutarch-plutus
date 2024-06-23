{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Note on Function instances
--
-- In many cases, we have hand-rolled instances of Function that merely delegate
-- to an inner type for a newtype. While in theory, this should be
-- via-derivable, because Function relies on an opaque type which we can't
-- coerce through, we have to do this by hand.

module PlutusLedgerApi.V2.Orphans (
  -- * Specialized Value wrappers
  FeeValue (..),
  getFeeValue,
  NonAdaValue (..),
  getNonAdaValue,
  UTxOValue (..),
  getUtxoValue,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Set qualified as Set
import Data.Word (Word32)
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.QuickCheck.Utils (
  fromAsWord64,
  unSizedByteString,
 )
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PLA
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonEmptyList (NonEmpty),
  NonNegative (NonNegative),
  Positive (Positive),
  chooseBoundedIntegral,
  chooseInt,
  frequency,
  functionMap,
  getNonEmpty,
  getNonNegative,
  oneof,
  resize,
  scale,
  sized,
  variant,
  vectorOf,
 )
import Test.QuickCheck.Instances.ByteString ()

-- | @since 1.0.0
instance Arbitrary PlutusTx.BuiltinByteString where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PlutusTx.toBuiltin @ByteString <$> arbitrary
  {-# INLINEABLE shrink #-}
  shrink = fmap (PlutusTx.toBuiltin @ByteString) . shrink . PlutusTx.fromBuiltin

-- | @since 1.0.0
instance CoArbitrary PlutusTx.BuiltinByteString where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = coarbitrary . PlutusTx.fromBuiltin

-- | @since 1.0.0
instance Function PlutusTx.BuiltinByteString where
  {-# INLINEABLE function #-}
  function = functionMap PlutusTx.fromBuiltin (PlutusTx.toBuiltin @ByteString)

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance Arbitrary PLA.LedgerBytes

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.LedgerBytes

-- | @since 1.0.0
instance Function PLA.LedgerBytes where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.LedgerBytes

{- | BLAKE2b-244 hash. This does not shrink.

@since 1.0.0
-}
deriving via Blake2b244Hash instance Arbitrary PLA.PubKeyHash

-- | @since 1.0.0
deriving via Blake2b244Hash instance CoArbitrary PLA.PubKeyHash

-- | @since 1.0.0
instance Function PLA.PubKeyHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.PubKeyHash

{- | BLAKE2b-244 hash. This does not shrink.

@since 1.0.0
-}
deriving via Blake2b244Hash instance Arbitrary PLA.ScriptHash

-- | @since 1.0.0
deriving via Blake2b244Hash instance CoArbitrary PLA.ScriptHash

-- | @since 1.0.0
instance Function PLA.ScriptHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.ScriptHash

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

-- | @since 1.0.0
deriving via Integer instance Arbitrary PLA.Lovelace

-- | @since 1.0.0
deriving via Integer instance CoArbitrary PLA.Lovelace

-- | @since 1.0.0
instance Function PLA.Lovelace where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Lovelace

{- | A 'CurrencySymbol' is either a BLAKE2b-244 hash or empty (representing the
Ada symbol). In a fully-fair generator, this makes it vanishingly unlikely
that the Ada symbol will be produced naturally (1 in 2^8^28 = 2^244) odds.
QuickCheck doesn't give us the ability to represent these odds faithfully:
thus, we merely make the Ada symbol as unlikely as we can. If you want to
ensure that the Ada symbol is covered by your tests, you need to make
dedicated tests for this. For this reason, we also don't shrink into the Ada
symbol (indeed, we don't shrink at all).

@since 1.0.0
-}
instance Arbitrary PLA.CurrencySymbol where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.CurrencySymbol
      <$> frequency
        [ (1, pure "")
        , (maxBound, getBlake2b244Hash <$> arbitrary)
        ]

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.CurrencySymbol

-- | @since 1.0.0
instance Function PLA.CurrencySymbol where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.CurrencySymbol

{- | This instance can generate the Ada token name, with faithful odds. It is
limited to generating printable ASCII names, rather than the full UTF-8
range. We did this for two reasons:

1. For testing purposes, we should prioritize readability, hence our choice
   of a textual representation; and
2. It is difficult to work within the size limit (32 bytes) when generating
   UTF-8.

@since 1.0.0
-}
instance Arbitrary PLA.TokenName where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    fmap (PLA.TokenName . PlutusTx.toBuiltin @ByteString . BS.pack) . sized $ \size -> do
      -- We want the length to be size-dependent
      len <- scale (const size) . chooseInt $ (0, 32)
      -- But the bytes themselves should not be: the whole ASCII printable range
      -- should be available always
      vectorOf len . chooseBoundedIntegral $ (33, 126)
  {-# INLINEABLE shrink #-}
  shrink tn =
    PLA.TokenName . PlutusTx.toBuiltin @ByteString <$> do
      let asList = BS.unpack . PlutusTx.fromBuiltin @PlutusTx.BuiltinByteString . coerce $ tn
      bs <- BS.pack <$> shrink asList
      guard (BS.all (\w8 -> w8 >= 33 && w8 <= 126) bs)
      pure bs

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.TokenName

-- | @since 1.0.0
instance Function PLA.TokenName where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.TokenName

{- | This generates well-defined maps: specifically, there are no duplicate
keys. To ensure that this is preserved, we do not shrink keys: we only drop
whole entries, or shrink values associated with keys.

In order to make this instance even moderately efficient, we require an 'Ord'
constraint on keys. In practice, this isn't a significant limitation, as
basically all Plutus types have such an instance.

@since 1.0.0
-}
instance (Arbitrary k, Ord k) => Arbitrary1 (AssocMap.Map k) where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary genVal =
    AssocMap.unsafeFromList <$> do
      -- First, generate a Set of keys to ensure no duplication
      keyList <- Set.toList <$> arbitrary
      -- Then generate a value for each
      traverse (\key -> (key,) <$> genVal) keyList
  {-# INLINEABLE liftShrink #-}
  liftShrink shrinkVal aMap =
    AssocMap.unsafeFromList <$> do
      let asList = AssocMap.toList aMap
      liftShrink (\(key, val) -> (key,) <$> shrinkVal val) asList

-- | @since 1.0.0
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (AssocMap.Map k v) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = liftShrink shrink

-- | @since 1.0.0
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (AssocMap.Map k v) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = coarbitrary . AssocMap.toList

-- | @since 1.0.0
instance (Function k, Function v) => Function (AssocMap.Map k v) where
  {-# INLINEABLE function #-}
  function = functionMap AssocMap.toList AssocMap.unsafeFromList

{- | A 'PLA.Value' containing only Ada, suitable for fees. Furthermore, the
Ada quantity is non-negative.

= Note

This is designed to act as a modifier, and thus, we expose the constructor
even though it preserves invariants. If you use the constructor directly,
be /very/ certain that the Value being wrapped satisfies the invariants
described above: failing to do so means all guarantees of this type are off
the table.

@since 1.0.0
-}
newtype FeeValue = FeeValue PLA.Value
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via PLA.Value
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
instance Arbitrary FeeValue where
  {-# INLINEABLE arbitrary #-}
  arbitrary = FeeValue . PLA.singleton PLA.adaSymbol PLA.adaToken . getNonNegative <$> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (FeeValue v) =
    FeeValue . PLA.singleton PLA.adaSymbol PLA.adaToken <$> do
      let adaAmount = Value.valueOf v PLA.adaSymbol PLA.adaToken
      NonNegative adaAmount' <- shrink (NonNegative adaAmount)
      pure adaAmount'

-- | @since 1.0.0
deriving via PLA.Value instance CoArbitrary FeeValue

-- | @since 1.0.0
instance Function FeeValue where
  {-# INLINEABLE function #-}
  function = functionMap coerce FeeValue

-- | @since 1.0.0
getFeeValue :: FeeValue -> PLA.Value
getFeeValue = coerce

{- | A 'PLA.Value' that contains no Ada.

= Note

This is designed to act as a modifier, and thus, we expose the constructor
even though it preserves invariants. If you use the constructor directly,
be /very/ certain that the Value being wrapped satisfies the invariants
described above: failing to do so means all guarantees of this type are off
the table.

@since 1.0.0
-}
newtype NonAdaValue = NonAdaValue PLA.Value
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via PLA.Value
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
instance Arbitrary NonAdaValue where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    NonAdaValue <$> do
      -- Generate a set of currency symbols that aren't Ada
      keySet <- Set.fromList <$> liftArbitrary (PLA.CurrencySymbol . getBlake2b244Hash <$> arbitrary)
      let keyList = Set.toList keySet
      -- For each key, generate a set of token name keys that aren't Ada
      keyVals <- traverse mkInner keyList
      pure . foldMap (\(cs, vals) -> foldMap (uncurry (Value.singleton cs)) vals) $ keyVals
    where
      mkInner :: PLA.CurrencySymbol -> Gen (PLA.CurrencySymbol, [(PLA.TokenName, Integer)])
      mkInner cs =
        (cs,) . Set.toList . Set.fromList . getNonEmpty <$> liftArbitrary ((,) <$> genNonAdaTokenName <*> arbitrary)
      genNonAdaTokenName :: Gen PLA.TokenName
      genNonAdaTokenName = fmap (PLA.TokenName . PlutusTx.toBuiltin @ByteString . BS.pack) . sized $ \size -> do
        len <- resize size . chooseInt $ (1, 32)
        vectorOf len . chooseBoundedIntegral $ (33, 126)
  {-# INLINEABLE shrink #-}
  -- Since we can't shrink keys anyway, we just borrow the stock shrinker
  shrink (NonAdaValue v) = NonAdaValue <$> shrink v

-- | @since 1.0.0
deriving via PLA.Value instance CoArbitrary NonAdaValue

-- | @since 1.0.0
instance Function NonAdaValue where
  {-# INLINEABLE function #-}
  function = functionMap coerce NonAdaValue

-- | @since 1.0.0
getNonAdaValue :: NonAdaValue -> PLA.Value
getNonAdaValue = coerce

{- | A Value with positive Ada, suitable for 'PLA.TxOut'.

= Note

This is designed to act as a modifier, and thus, we expose the constructor
even though it preserves invariants. If you use the constructor directly,
be /very/ certain that the Value being wrapped satisfies the invariants
described above: failing to do so means all guarantees of this type are off
the table.

@since 1.0.0
-}
newtype UTxOValue = UTxOValue PLA.Value
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via PLA.Value
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
instance Arbitrary UTxOValue where
  {-# INLINEABLE arbitrary #-}
  -- Generate a NonAdaValue, then force a positive Ada value into it.
  arbitrary =
    UTxOValue <$> do
      NonAdaValue v <- arbitrary
      Positive adaQuantity <- arbitrary
      pure $ v <> Value.singleton "" "" adaQuantity
  {-# INLINEABLE shrink #-}
  shrink (UTxOValue v) =
    UTxOValue <$> do
      v' <- shrink v
      guard (Value.valueOf v' "" "" > 0)
      pure v'

-- | @since 1.0.0
deriving via PLA.Value instance CoArbitrary UTxOValue

-- | @since 1.0.0
instance Function UTxOValue where
  {-# INLINEABLE function #-}
  function = functionMap coerce UTxOValue

-- | @since 1.0.0
getUtxoValue :: UTxOValue -> PLA.Value
getUtxoValue = coerce

{- | This is the most general possible instance for 'PLA.Value'. In particular,
this can have zero values, and does not treat the Ada symbol or token name
specially.

@since 1.0.0
-}
instance Arbitrary PLA.Value where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.Value <$> arbitrary
  {-# INLINEABLE shrink #-}
  shrink = fmap PLA.Value . shrink . PLA.getValue

-- | @since 1.0.0
deriving via
  (AssocMap.Map PLA.CurrencySymbol (AssocMap.Map PLA.TokenName Integer))
  instance
    CoArbitrary PLA.Value

-- | @since 1.0.0
instance Function PLA.Value where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Value

-- | @since 1.0.0
deriving via (NonNegative Integer) instance Arbitrary PLA.POSIXTime

-- | @since 1.0.0
deriving via Integer instance CoArbitrary PLA.POSIXTime

-- | @since 1.0.0
instance Function PLA.POSIXTime where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.POSIXTime

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

{- | This is a very general instance, able to produce 'PlutusTx.BuiltinData' of
basically any shape. You probably want something more focused than this.

@since 1.0.0
-}
instance Arbitrary PlutusTx.BuiltinData where
  {-# INLINEABLE arbitrary #-}
  arbitrary = sized $ \originalSize -> go originalSize originalSize
    where
      -- We have to track our original size (for contents) as well as a
      -- possibly-reduced size (for structure) separately. If we don't do this,
      -- 'leaf' data may end up being far smaller than it should be, biasing the
      -- generator.
      go :: Int -> Int -> Gen PlutusTx.BuiltinData
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
      genB :: Int -> Gen PlutusTx.BuiltinData
      genB size = Builtins.mkB <$> resize size arbitrary
      genI :: Int -> Gen PlutusTx.BuiltinData
      genI size = Builtins.mkI <$> resize size arbitrary
      genConstr :: Int -> Int -> Gen PlutusTx.BuiltinData
      genConstr contentSize structureSize =
        Builtins.mkConstr
          <$> resize contentSize (getNonNegative <$> arbitrary)
          <*> resize structureSize (liftArbitrary . go contentSize $ structureSize `quot` 2)
      genList :: Int -> Int -> Gen PlutusTx.BuiltinData
      genList contentSize structureSize =
        Builtins.mkList <$> resize structureSize (liftArbitrary . go contentSize $ structureSize `quot` 2)
      genMap :: Int -> Int -> Gen PlutusTx.BuiltinData
      genMap contentSize structureSize = do
        let newStructureSize = structureSize `quot` 2
        Builtins.mkMap <$> resize structureSize (liftArbitrary $ (,) <$> go contentSize newStructureSize <*> go contentSize newStructureSize)
  {-# INLINEABLE shrink #-}
  shrink dat =
    Builtins.matchData
      dat
      shrinkConstr
      shrinkMap
      shrinkList
      (fmap (Builtins.mkI . getNonNegative) . shrink . NonNegative)
      (fmap Builtins.mkB . shrink)
    where
      shrinkConstr :: Integer -> [PlutusTx.BuiltinData] -> [PlutusTx.BuiltinData]
      shrinkConstr ix dats = do
        NonNegative ix' <- shrink (NonNegative ix)
        dats' <- shrink dats
        pure . Builtins.mkConstr ix' $ dats'
      shrinkMap :: [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)] -> [PlutusTx.BuiltinData]
      shrinkMap kvs = Builtins.mkMap <$> shrink kvs
      shrinkList :: [PlutusTx.BuiltinData] -> [PlutusTx.BuiltinData]
      shrinkList ell = Builtins.mkList <$> shrink ell

-- | @since 1.0.0
instance CoArbitrary PlutusTx.BuiltinData where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary dat =
    Builtins.matchData
      dat
      (\ix dats -> variant (0 :: Int) . coarbitrary ix . coarbitrary dats)
      (\kvs -> variant (1 :: Int) . coarbitrary kvs)
      (\ell -> variant (2 :: Int) . coarbitrary ell)
      (\i -> variant (3 :: Int) . coarbitrary i)
      (\bs -> variant (4 :: Int) . coarbitrary bs)

-- | @since 1.0.0
instance Function PlutusTx.BuiltinData where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PlutusTx.BuiltinData ->
        Either
          (Integer, [PlutusTx.BuiltinData])
          ( Either
              [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)]
              ( Either
                  [PlutusTx.BuiltinData]
                  ( Either Integer PlutusTx.BuiltinByteString
                  )
              )
          )
      into dat =
        Builtins.matchData
          dat
          (\ix -> Left . (ix,))
          (Right . Left)
          (Right . Right . Left)
          (Right . Right . Right . Left)
          (Right . Right . Right . Right)
      outOf ::
        Either
          (Integer, [PlutusTx.BuiltinData])
          ( Either
              [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)]
              ( Either
                  [PlutusTx.BuiltinData]
                  ( Either Integer PlutusTx.BuiltinByteString
                  )
              )
          ) ->
        PlutusTx.BuiltinData
      outOf = \case
        Left (ix, dats) -> Builtins.mkConstr ix dats
        Right (Left kvs) -> Builtins.mkMap kvs
        Right (Right (Left ell)) -> Builtins.mkList ell
        Right (Right (Right (Left i))) -> Builtins.mkI i
        Right (Right (Right (Right bs))) -> Builtins.mkB bs

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance Arbitrary PLA.Datum

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance CoArbitrary PLA.Datum

-- | @since 1.0.0
instance Function PLA.Datum where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Datum

-- | @since 1.0.0
deriving via Blake2b256Hash instance Arbitrary PLA.DatumHash

-- | @since 1.0.0
deriving via Blake2b256Hash instance CoArbitrary PLA.DatumHash

-- | @since 1.0.0
instance Function PLA.DatumHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.DatumHash

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance Arbitrary PLA.Redeemer

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance CoArbitrary PLA.Redeemer

-- | @since 1.0.0
instance Function PLA.Redeemer where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Redeemer

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

-- Needed because otherwise, deriving an instance for TxInfo is impossible.
-- Arguably this is actually a bug on IOG's side, as the corresponding Cardano
-- type _does_ have an Ord instance.

-- | @since 1.0.0
deriving stock instance Ord PLA.ScriptPurpose

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

{- | This instance does not bias the constructor choice: it is equally likely to
produce 'PLA.Finite', 'PLA.NegInf' and 'PLA.PosInf'. Bear this in mind when
using: in particular, the instance for 'PLA.Interval' /does not/ make use of
this instance.

@since 1.0.0
-}
instance Arbitrary1 PLA.Extended where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary genInner =
    oneof
      [ pure PLA.NegInf
      , PLA.Finite <$> genInner
      , pure PLA.PosInf
      ]
  {-# INLINEABLE liftShrink #-}
  liftShrink shrinkInner = \case
    PLA.NegInf -> []
    PLA.Finite x -> PLA.Finite <$> shrinkInner x
    PLA.PosInf -> []

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats.

@since 1.0.0
-}
instance Arbitrary a => Arbitrary (PLA.Extended a) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = liftShrink shrink

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Extended a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.NegInf -> variant (0 :: Int)
    PLA.Finite x -> variant (1 :: Int) . coarbitrary x
    PLA.PosInf -> variant (2 :: Int)

-- | @since 1.0.0
instance Function a => Function (PLA.Extended a) where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Extended a -> Maybe (Maybe a)
      into = \case
        PLA.NegInf -> Nothing
        PLA.PosInf -> Just Nothing
        PLA.Finite x -> Just (Just x)
      outOf :: Maybe (Maybe a) -> PLA.Extended a
      outOf = \case
        Nothing -> PLA.NegInf
        Just Nothing -> PLA.PosInf
        Just (Just x) -> PLA.Finite x

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.LowerBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.LowerBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.LowerBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.LowerBound e c) = case e of
    PLA.Finite _ -> PLA.LowerBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.LowerBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.LowerBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.LowerBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.LowerBound e c) -> (e, c)) (uncurry PLA.LowerBound)

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.UpperBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.UpperBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.UpperBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.UpperBound e c) = case e of
    PLA.Finite _ -> PLA.UpperBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.UpperBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.UpperBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.UpperBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.UpperBound e c) -> (e, c)) (uncurry PLA.UpperBound)

{- | We provide an instance specialized to 'PLA.POSIXTime', rather than a more
general one, as it doesn't make much sense to talk about 'PLA.Interval's of
arbitrary types in general. Furthermore, this is the only instance we
actually use, so there's no real loss there.

This instance tries to make time intervals as fairly as possible, while also
ensuring that they're sensibly formed. We work under the assumption of a
32-bit epoch: while this is _technically_ not going to last much longer,
we're safe until about 2030 on that basis, which should be enough for now.

We choose not to shrink intervals, as this is surprisingly complex: in at
least one common case, it's not even possible to write a shrinker that will
ever 'bottom out', due to us having infinite bounds!

@since 1.0.0
-}
instance Arbitrary (PLA.Interval PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    let epochSize = fromIntegral (maxBound :: Word32)
    lowerBound <-
      frequency
        [ (1, pure PLA.NegInf)
        , (1, pure PLA.PosInf)
        , (epochSize, PLA.Finite <$> arbitrary)
        ]
    case lowerBound of
      -- With a finite lower bound, it makes sense to talk about an upper one
      PLA.Finite x -> do
        lowerClosure <- arbitrary
        let lower = PLA.LowerBound lowerBound lowerClosure
        -- To ensure we generate something sensible for the upper bound, we
        -- either generate a 'diff', or positive infinity.
        whatUpper <-
          frequency
            [ (1, pure . Left $ PLA.PosInf)
            , (epochSize, Right <$> arbitrary)
            ]
        case whatUpper of
          -- If we have an infinite upper bound, we know it will be open.
          Left _ -> do
            let upper = PLA.UpperBound PLA.PosInf False
            pure . PLA.Interval lower $ upper
          Right diff -> case (diff, lowerClosure) of
            -- A diff of 0 means we can only have a singleton closure sensibly.
            (0, _) -> pure . Interval.singleton $ x
            -- A diff of 1 with an open lower bound means we either have a
            -- singleton closure or an empty one.
            (1, False) -> do
              upperClosure <- arbitrary
              pure $
                if upperClosure
                  then Interval.singleton x
                  else Interval.never
            -- A diff of 1 with a closed lower bound is either a singleton
            -- closure or one with two values.
            (1, True) -> do
              upperClosure <- arbitrary
              pure $
                if upperClosure
                  then PLA.Interval lower . PLA.UpperBound (PLA.Finite (x + diff)) $ upperClosure
                  else Interval.singleton x
            -- A diff bigger than 1 can be treated uniformly.
            (_, _) -> PLA.Interval lower . PLA.UpperBound (PLA.Finite (x + diff)) <$> arbitrary
      -- With an negative infinite lower bound, we know it will be open.
      PLA.NegInf -> do
        let lower = PLA.LowerBound lowerBound False
        -- To ensure we generate something sensible for the upper bound, we
        -- do not attempt to generate NegInf
        upperBound <-
          frequency
            [ (1, pure PLA.PosInf)
            , (epochSize, PLA.Finite <$> arbitrary)
            ]
        case upperBound of
          -- With a finite upper bound, we just choose a closure and move on.
          PLA.Finite _ -> do
            upper <- PLA.UpperBound upperBound <$> arbitrary
            pure . PLA.Interval lower $ upper
          -- With an infinite upper bound, we have the range that includes
          -- everything. We use the canonical choice provided by
          -- Interval.always.
          _ -> pure Interval.always
      -- With an positive infinite lower bound, we have the empty interval, and
      -- can choose any representation of such that we like. We use the
      -- canonical choice provided by Interval.never.
      PLA.PosInf -> pure Interval.never

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Interval a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Interval lower upper) = coarbitrary lower . coarbitrary upper

-- | @since 1.0.0
instance Function a => Function (PLA.Interval a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.Interval lower upper) -> (lower, upper)) (uncurry PLA.Interval)

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
      <*> (getUtxoValue <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOut addr val od msh) =
    PLA.TxOut
      <$> shrink addr
      <*> (getUtxoValue <$> shrink (UTxOValue val))
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
      <*> (getFeeValue <$> arbitrary)
      <*> (getNonAdaValue <$> arbitrary)
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
      <*> (getNonEmpty <$> shrink (NonEmpty routs))
      <*> shrink outs
      <*> (getFeeValue <$> shrink (FeeValue fee))
      <*> (getNonAdaValue <$> shrink (NonAdaValue mint))
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

-- | @since 1.0.0
deriving via PLA.DatumHash instance Arbitrary PLA.RedeemerHash

-- | @since 1.0.0
deriving via PLA.DatumHash instance CoArbitrary PLA.RedeemerHash

-- | @since 1.0.0
instance Function PLA.RedeemerHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.RedeemerHash

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

-- Helpers

-- Wrapper for BLAKE2b-244 hashes for convenience.
newtype Blake2b244Hash = Blake2b244Hash PlutusTx.BuiltinByteString
  deriving (Eq, Ord) via PlutusTx.BuiltinByteString
  deriving stock (Show)

-- No shrinker, as it doesn't make much sense to.
instance Arbitrary Blake2b244Hash where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Blake2b244Hash . PlutusTx.toBuiltin @ByteString . unSizedByteString @28 <$> arbitrary

deriving via PlutusTx.BuiltinByteString instance CoArbitrary Blake2b244Hash

getBlake2b244Hash :: Blake2b244Hash -> PlutusTx.BuiltinByteString
getBlake2b244Hash = coerce

-- Wrapper for BLAKE2b-256 hashes for convenience.
newtype Blake2b256Hash = Blake2b256Hash PlutusTx.BuiltinByteString
  deriving (Eq, Ord) via PlutusTx.BuiltinByteString
  deriving stock (Show)

-- No shrinker, as it doesn't make much sense to.
instance Arbitrary Blake2b256Hash where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Blake2b256Hash . PlutusTx.toBuiltin @ByteString . unSizedByteString @32 <$> arbitrary

deriving via PlutusTx.BuiltinByteString instance CoArbitrary Blake2b256Hash

-- This is frankly a bizarre omission
instance Arbitrary1 NonEmptyList where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary genInner =
    NonEmpty <$> do
      x <- genInner
      xs <- liftArbitrary genInner
      pure $ x : xs
  {-# INLINEABLE liftShrink #-}
  liftShrink shrinkInner (NonEmpty ell) =
    NonEmpty <$> case ell of
      [] -> []
      (x : xs) -> (:) <$> shrinkInner x <*> liftShrink shrinkInner xs
