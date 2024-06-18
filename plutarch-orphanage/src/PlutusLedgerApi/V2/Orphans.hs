{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V2.Orphans () where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Coerce (coerce)
import Data.Set qualified as Set
import PlutusLedgerApi.QuickCheck.Utils (
  fromAsWord64,
  fromNonAdaCurrencySymbol,
  unSizedByteString,
 )
import PlutusLedgerApi.V2 qualified as PLA
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonNegative (NonNegative),
  chooseBoundedIntegral,
  chooseInt,
  frequency,
  functionMap,
  getNonNegative,
  oneof,
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

{- | Base-16 encoded bytestring.

@since 1.0.0
-}
instance Arbitrary PLA.LedgerBytes where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.LedgerBytes . PlutusTx.toBuiltin @ByteString . Base16.encode <$> arbitrary
  {-# INLINEABLE shrink #-}
  -- We use the lenient strategy here, as it's not possible for us to ever end
  -- up with anything that wasn't base-16 to begin with.
  shrink =
    fmap (PLA.LedgerBytes . PlutusTx.toBuiltin @ByteString . Base16.encode)
      . shrink
      . Base16.decodeLenient
      . PlutusTx.fromBuiltin
      . coerce @PLA.LedgerBytes @PlutusTx.BuiltinByteString

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.LedgerBytes

-- | @since 1.0.0
instance Function PLA.LedgerBytes where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.LedgerBytes

{- | BLAKE2b-244 hash. This does not shrink.

@since 1.0.0
-}
instance Arbitrary PLA.PubKeyHash where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.PubKeyHash . PlutusTx.toBuiltin @ByteString . unSizedByteString @28 <$> arbitrary

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.PubKeyHash

-- | @since 1.0.0
instance Function PLA.PubKeyHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.PubKeyHash

{- | BLAKE2b-244 hash. This does not shrink.

@since 1.0.0
-}
deriving via PLA.PubKeyHash instance Arbitrary PLA.ScriptHash

-- | @since 1.0.0
deriving via PLA.PubKeyHash instance CoArbitrary PLA.ScriptHash

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

You may also want to look at 'NonAdaCurrencySymbol' if you're not interested
in generating the Ada symbol in your case.

@since 1.0.0
-}
instance Arbitrary PLA.CurrencySymbol where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    frequency
      [ (1, pure . PLA.CurrencySymbol . PlutusTx.toBuiltin @ByteString $ "")
      , (maxBound, fromNonAdaCurrencySymbol <$> arbitrary)
      ]

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.CurrencySymbol

-- | @since 1.0.0
instance Function PLA.CurrencySymbol where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.CurrencySymbol

{- | This instance can generate the Ada token name, with faithful odds. It is
limited to generating printable ASCII names, rather than the full UTF-8
range. See 'PlutusLedgerApi.QuickCheck.Utils.NonAdaTokenName' documentation
for an explanation of why we chose to do this. We also shrink into the Ada
token name: bear this in mind.

If you don't want to generate (or shrink into) the Ada token name, consider
using 'PlutusLedgerApi.QuickCheck.Utils.NonAdaTokenName' instead.

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

-- TODO: We should probably treat Ada values differently?

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

{- | As this type is a hash, we don't shrink it.

@since 1.0.0
-}
instance Arbitrary PLA.TxId where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxId . PlutusTx.toBuiltin @ByteString . unSizedByteString @32 <$> arbitrary

-- | @since 1.0.0
deriving via PlutusTx.BuiltinByteString instance CoArbitrary PLA.TxId

-- | @since 1.0.0
instance Function PLA.TxId where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.TxId
