{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Scripts () where

import Data.Coerce (coerce)
import PlutusLedgerApi.Orphans.Common (
  Blake2b244Hash (Blake2b244Hash),
  Blake2b256Hash (Blake2b256Hash),
 )
import PlutusLedgerApi.V1 qualified as PLA
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance Arbitrary PLA.Redeemer

-- | @since 1.0.0
deriving via PlutusTx.BuiltinData instance CoArbitrary PLA.Redeemer

-- | @since 1.0.0
instance Function PLA.Redeemer where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Redeemer

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
deriving via PLA.DatumHash instance Arbitrary PLA.RedeemerHash

-- | @since 1.0.0
deriving via PLA.DatumHash instance CoArbitrary PLA.RedeemerHash

-- | @since 1.0.0
instance Function PLA.RedeemerHash where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.RedeemerHash

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
