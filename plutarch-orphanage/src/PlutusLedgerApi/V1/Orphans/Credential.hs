{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Credential () where

import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Crypto ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import Test.QuickCheck (
  Arbitrary (arbitrary),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
  oneof,
  variant,
 )

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
