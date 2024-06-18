{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}

module PlutusLedgerApi.QuickCheck.Utils (
  SizedByteString (SizedByteString),
  unSizedByteString,
  AsWord64 (AsWord64),
  fromAsWord64,
  NonAdaCurrencySymbol (NonAdaCurrencySymbol),
  fromNonAdaCurrencySymbol,
  NonAdaTokenName (NonAdaTokenName),
  fromNonAdaTokenName,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)
import GHC.TypeNats (KnownNat, Natural, natVal)
import PlutusLedgerApi.V2 qualified as PLA
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary,
  Fixed (Fixed),
  Function (function),
  chooseBoundedIntegral,
  chooseInt,
  functionMap,
  scale,
  sized,
  vectorOf,
 )
import Test.QuickCheck.Instances.ByteString ()

{- | The Ada 'PLA.CurrencySymbol' is somewhat special, as it not only doesn't
look like any other one, it also often gets special treatment, such as in
'PLA.Value'. Thus, we may want to generate a 'PLA.CurrencySymbol' that isn't
the Ada 'PLA.CurrencySymbol'. This type is designed to help with that.

We don't expose the constructor, instead providing a read-only pattern, as
well as an accessor function, to ensure that the size invariant is
maintained. This is needed, as 'PLA.CurrencySymbol's that aren't the Ada
symbol are BLAKE2b-244 hashes, which have a fixed size.

This type does not shrink, as it wouldn't make much sense to.

@since 1.0.0
-}
newtype NonAdaCurrencySymbol = UnsafeNonAdaCurrencySymbol (Fixed (SizedByteString 28))
  deriving
    ( -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      Ord
    , -- | @since 1.0.0
      CoArbitrary
    )
    via (SizedByteString 28)
  deriving
    ( -- | @since 1.0.0
      Arbitrary
    )
    via (Fixed (SizedByteString 28))
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

{- | The Ada PLA.TokenName is somewhat special, as it not only doesn't look like
any other one, it also often gets special treatment, such as in 'PLA.Value'.
Thus, we may want to generate a 'PLA.TokenName' that isn't the Ada
'PLA.TokenName'. Furthermore, non-Ada 'PLA.TokenName's are interpreted as the
binary representation of UTF-8, but have a size limit of 32 bytes. Thus, we
have two choices:

1. Attempt to generate the full possible UTF-8 range, and then hope the
   encoding doesn't exceed the size limit; or
2. Limit ourselves to the ASCII subset (or rather, its _printable_ subset) to
   ensure the generator can't 'miss'.

We choose strategy 2 for performance. If you have concerns about UTF-8
'PLA.TokenName's, you should write separate tests to check this.

We don't expose the constructor, instead providing a read-only pattern, as
well as an accessor function, to ensure that the size invariant is
maintained. This is needed, as 'PLA.TokenName's have a size limit.

@since 1.0.0
-}
newtype NonAdaTokenName = UnsafeNonAdaTokenName ByteString
  deriving
    ( -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      Ord
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
instance Arbitrary NonAdaTokenName where
  {-# INLINEABLE arbitrary #-}
  arbitrary = fmap (UnsafeNonAdaTokenName . BS.pack) . sized $ \size -> do
    -- We want the length to be size-dependent
    len <- scale (const size) . chooseInt $ (1, 32)
    -- But the bytes themselves should not be: the whole ASCII printable range
    -- should be available always.
    vectorOf len . chooseBoundedIntegral $ (33, 126)
  {-# INLINEABLE shrink #-}
  shrink (UnsafeNonAdaTokenName bs) = do
    bs' <- shrink bs
    guard (not . BS.null $ bs')
    guard (BS.all (\w8 -> w8 >= 33 && w8 <= 126) bs')
    pure . UnsafeNonAdaTokenName $ bs'

-- | @since 1.0.0
deriving via ByteString instance CoArbitrary NonAdaTokenName

-- | @since 1.0.0
instance Function NonAdaTokenName where
  {-# INLINEABLE function #-}
  function = functionMap coerce UnsafeNonAdaTokenName

{- | Read-only pattern for accessing the underlying 'PLA.TokenName'

@since 1.0.0
-}
pattern NonAdaTokenName :: PLA.TokenName -> NonAdaTokenName
pattern NonAdaTokenName tn <- (fromNonAdaTokenName -> tn)

{- | Get the underlying 'PLA.TokenName', which is guaranteed not to be the Ada
token name.

@since 1.0.0
-}
fromNonAdaTokenName :: NonAdaTokenName -> PLA.TokenName
fromNonAdaTokenName = coerce . PlutusTx.toBuiltin @ByteString . coerce

-- | @since 1.0.0
instance Function NonAdaCurrencySymbol where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: NonAdaCurrencySymbol -> SizedByteString 28
      into = coerce
      outOf :: SizedByteString 28 -> NonAdaCurrencySymbol
      outOf = coerce

{- | Read-only pattern for accessing the underlying 'PLA.CurrencySymbol'.
Use it just like you would use a data constructor in a pattern match.

@since 1.0.0
-}
pattern NonAdaCurrencySymbol :: PLA.CurrencySymbol -> NonAdaCurrencySymbol
pattern NonAdaCurrencySymbol cs <- (fromNonAdaCurrencySymbol -> cs)

{- | Get the underlying 'PLA.CurrencySymbol', which is guaranteed not to be the Ada
symbol.

@since 1.0.0
-}
fromNonAdaCurrencySymbol :: NonAdaCurrencySymbol -> PLA.CurrencySymbol
fromNonAdaCurrencySymbol = coerce . PlutusTx.toBuiltin @ByteString . coerce

{- | Helper for 'ByteString's of a fixed length. We don't expose the
constructor, instead providing a read-only pattern, as well as an accessor
function, to ensure that the size invariant is maintained.

@since 1.0.0
-}
newtype SizedByteString (n :: Natural) = UnsafeSizedByteString ByteString
  deriving
    ( -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      Ord
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

type role SizedByteString nominal

-- | @since 1.0.0
instance KnownNat n => Arbitrary (SizedByteString n) where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    UnsafeSizedByteString . BS.pack <$> do
      let !len = fromIntegral . natVal $ Proxy @n
      vectorOf len arbitrary
  {-# INLINEABLE shrink #-}
  shrink =
    fmap (UnsafeSizedByteString . BS.pack)
      . traverse shrink
      . BS.unpack
      . unSizedByteString

-- | @since 1.0.0
deriving via ByteString instance CoArbitrary (SizedByteString n)

-- | @since 1.0.0
instance Function (SizedByteString n) where
  {-# INLINEABLE function #-}
  function = functionMap coerce UnsafeSizedByteString

{- | Read-only pattern for accessing the underlying 'ByteString'. Use it just
like you would use a data constructor in a pattern match.

@since 1.0.0
-}
pattern SizedByteString :: forall (n :: Natural). ByteString -> SizedByteString n
pattern SizedByteString bs <- UnsafeSizedByteString bs

{-# COMPLETE SizedByteString #-}

{- | Get the underlying 'ByteString'. It is guaranteed to have the length
specified in its type.

@since 1.0.0
-}
unSizedByteString ::
  forall (n :: Natural).
  SizedByteString n ->
  ByteString
unSizedByteString = coerce

{- | Plutus' ledger API often has to \'fake\' 'Word64' using the much larger
'Integer' type. This helper is designed to generate 'Integer's that fit into
'Word64'.

We don't expose the constructor directly; instead, we provide a read-only
pattern and an accessor function.

@since 1.0.0
-}
newtype AsWord64 = UnsafeAsWord64 Word64
  deriving
    ( -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      Ord
    , -- | @since 1.0.0
      Arbitrary
    , -- | @since 1.0.0
      CoArbitrary
    )
    via Word64
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
instance Function AsWord64 where
  {-# INLINEABLE function #-}
  function = functionMap coerce UnsafeAsWord64

{- | Read-only pattern for accessing the underlying 'Integer'. Use it just like
you would use a data constructor in a pattern match.

@since 1.0.0
-}
pattern AsWord64 :: Integer -> AsWord64
pattern AsWord64 i <- (fromAsWord64 -> i)

-- | @since 1.0.0
fromAsWord64 :: AsWord64 -> Integer
fromAsWord64 = fromIntegral . coerce @_ @Word64
