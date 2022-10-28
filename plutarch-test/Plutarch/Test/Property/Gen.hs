{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generator helpers
module Plutarch.Test.Property.Gen (
  genRational,
  genInteger,
  genList,
  bsOfLength,
) where

import Control.Monad (MonadPlus, liftM2, mfilter)
import Data.List (nub, sortOn)
import Data.Ratio ((%))

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty.QuickCheck (
  Arbitrary,
  Gen,
  Negative (getNegative),
  Positive (getPositive),
  arbitrary,
  choose,
  elements,
  listOf1,
  oneof,
  vectorOf,
 )

import PlutusLedgerApi.V1

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (c2w)
import Test.QuickCheck.Instances ()

import PlutusTx.AssocMap qualified as PlutusMap

genInteger :: MonadGen g => g Integer
genInteger = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

genRational :: (MonadPlus g, MonadGen g) => g Rational
genRational = liftM2 (%) genInteger (mfilter (/= 0) genInteger)

genList :: MonadGen g => g a -> g [a]
genList = Gen.list (Range.linear 0 100)

------------------- Arbitrary instances for several ApiTypes -----------------------

bsOfLength :: Int -> Gen ByteString
bsOfLength n =
  BS.pack <$> vectorOf n (c2w <$> choose (minBound :: Char, maxBound))

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin @ByteString <$> arbitrary

instance Arbitrary CurrencySymbol where
  arbitrary =
    let arbitrary' =
          ((fmap (toBuiltin @ByteString) . bsOfLength) =<< elements [0, 28])
     in CurrencySymbol <$> arbitrary'

instance Arbitrary Value where
  arbitrary =
    (\a -> Value . PlutusMap.fromList . sortOn fst . zip a)
      <$> currSyms
      <*> listOf1 arbitraryTokMap
    where
      -- List of unique token names.
      tokNames = nub <$> listOf1 (arbitrary @TokenName)
      -- List of unique currency symbols.
      currSyms = nub <$> listOf1 (arbitrary @CurrencySymbol)
      arbitraryTokMap =
        (\a -> PlutusMap.fromList . sortOn fst . zip a)
          <$> tokNames
          <*> listOf1 (oneof [getPositive @Integer <$> arbitrary, getNegative @Integer <$> arbitrary])

instance Arbitrary TokenName where
  arbitrary = do
    ln <- choose (0, 32)
    str <-
      BS.pack
        <$> vectorOf
          ln
          ( oneof $
              fmap
                (fmap c2w)
                [ choose ('a', 'f')
                , choose ('A', 'F')
                , choose ('0', '9')
                ]
          )
    pure . TokenName . toBuiltin @ByteString $ str

instance Arbitrary PubKeyHash where
  arbitrary =
    let arbitrary' =
          toBuiltin @ByteString <$> bsOfLength 28
     in PubKeyHash <$> arbitrary'

instance Arbitrary ScriptHash where
  arbitrary =
    let arbitrary' =
          toBuiltin @ByteString <$> bsOfLength 28
     in ScriptHash <$> arbitrary'

instance Arbitrary Credential where
  arbitrary =
    oneof
      [ PubKeyCredential <$> arbitrary
      , ScriptCredential <$> arbitrary
      ]

instance Arbitrary StakingCredential where
  arbitrary =
    oneof
      [ StakingHash <$> arbitrary
      , StakingPtr <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary
