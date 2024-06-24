{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.Orphans.Common (
  Blake2b256Hash (..),
  Blake2b244Hash (..),
  getBlake2b256Hash,
  getBlake2b244Hash,
) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import PlutusLedgerApi.QuickCheck.Utils (unSizedByteString)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonNegative (NonNegative),
  functionMap,
  getNonNegative,
  liftArbitrary,
  oneof,
  resize,
  sized,
  variant,
 )

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

{- | Wrapper for BLAKE2b-244 hashes for convenience.
@since
-}
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

getBlake2b256Hash :: Blake2b256Hash -> PlutusTx.BuiltinByteString
getBlake2b256Hash = coerce

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
