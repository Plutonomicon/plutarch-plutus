{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Value (
  UTxOValue (..),
  getUtxoValue,
  NonAdaValue (..),
  getNonAdaValue,
  FeeValue (..),
  getFeeValue,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Set qualified as Set
import PlutusLedgerApi.Orphans.Common (getBlake2b244Hash)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary,
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
  resize,
  scale,
  sized,
  vectorOf,
 )

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
      keyVals <- traverse (scale (`quot` 8) . mkInner) keyList
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

{- | This is the most general possible instance for 'PLA.Value'. In particular,
this can have zero values, and does not treat the Ada symbol or token name
specially.

@since 1.0.0
-}
instance Arbitrary PLA.Value where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.Value <$> liftArbitrary (scale (`quot` 4) arbitrary)
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
      len <- resize size . chooseInt $ (0, 32)
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

-- Helpers

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
