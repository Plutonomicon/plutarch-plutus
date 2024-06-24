{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Crypto () where

import Data.Coerce (coerce)
import PlutusLedgerApi.Orphans.Common (
  Blake2b244Hash (Blake2b244Hash),
 )
import PlutusLedgerApi.V1 qualified as PLA
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )

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
