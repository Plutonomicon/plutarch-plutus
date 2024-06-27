{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Tx () where

import Data.Coerce (coerce)
import PlutusLedgerApi.Orphans.Common (
  Blake2b256Hash (Blake2b256Hash),
 )
import PlutusLedgerApi.V1 qualified as PLA
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )

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
