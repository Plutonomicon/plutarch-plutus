module PlutusLedgerApi.V3.Orphans.Value (
  -- * Specialized Value wrappers
  MintValue (..),
  getMintValue,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Set qualified as Set
import PlutusLedgerApi.Orphans.Common (getBlake2b244Hash)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Value ()
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary,
  Function (function),
  Gen,
  NonZero (NonZero),
  chooseBoundedIntegral,
  chooseInt,
  functionMap,
  getNonEmpty,
  getNonZero,
  resize,
  scale,
  sized,
  vectorOf,
 )

{- | A 'PLA.Value' that contains only non-zero amounts but does not have zero Ada entry

= Note

This is designed to act as a modifier, and thus, we expose the constructor
even though it preserves invariants. If you use the constructor directly,
be /very/ certain that the Value being wrapped satisfies the invariants
described above: failing to do so means all guarantees of this type are off
the table.

@since WIP
-}
newtype MintValue = MintValue PLA.Value
  deriving
    ( -- | @since WIP
      Eq
    )
    via PLA.Value
  deriving stock
    ( -- | @since WIP
      Show
    )

-- | @since WIP
instance Arbitrary MintValue where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    MintValue <$> do
      -- Generate a set of currency symbols that aren't Ada
      keySet <- Set.fromList <$> liftArbitrary (PLA.CurrencySymbol . getBlake2b244Hash <$> arbitrary)
      let keyList = Set.toList keySet
      -- For each key, generate a set of token name keys that aren't Ada
      keyVals <- traverse (scale (`quot` 8) . mkInner) keyList
      pure . foldMap (\(cs, vals) -> foldMap (uncurry (Value.singleton cs)) vals) $ keyVals
    where
      mkInner :: PLA.CurrencySymbol -> Gen (PLA.CurrencySymbol, [(PLA.TokenName, Integer)])
      mkInner cs =
        (cs,)
          . Set.toList
          . Set.fromList
          . getNonEmpty
          <$> liftArbitrary ((,) <$> genNonAdaTokenName <*> (getNonZero <$> arbitrary))

      genNonAdaTokenName :: Gen PLA.TokenName
      genNonAdaTokenName = fmap (PLA.TokenName . PlutusTx.toBuiltin @ByteString . BS.pack) . sized $ \size -> do
        len <- resize size . chooseInt $ (1, 32)
        vectorOf len . chooseBoundedIntegral $ (33, 126)
  {-# INLINEABLE shrink #-}
  shrink (MintValue (Value.Value v)) =
    MintValue . Value.Value <$> do
      -- To ensure we don't break anything, we shrink in only two ways:
      --
      -- 1. Dropping keys (outer or inner)
      -- 2. Shrinking amounts
      --
      -- To make this a bit easier on ourselves, we first 'unpack' the Value
      -- completely, shrink the resulting (nested) list, then 'repack'. As neither
      -- of these changes affect order or uniqueness, we're safe.
      let asList = fmap AssocMap.toList <$> AssocMap.toList v
      shrunk <- liftShrink (\(cs, inner) -> (cs,) <$> liftShrink (\(tn, amount) -> (tn,) . getNonZero <$> shrink (NonZero amount)) inner) asList
      pure . AssocMap.unsafeFromList . fmap (fmap AssocMap.unsafeFromList) $ shrunk

-- | @since WIP
deriving via PLA.Value instance CoArbitrary MintValue

-- | @since WIP
instance Function MintValue where
  {-# INLINEABLE function #-}
  function = functionMap coerce MintValue

-- | @since WIP
getMintValue :: MintValue -> Value.Value
getMintValue = coerce
