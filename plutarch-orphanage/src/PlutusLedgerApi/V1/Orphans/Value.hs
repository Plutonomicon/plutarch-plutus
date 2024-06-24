{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Value () where

import Data.Coerce (coerce)
import PlutusLedgerApi.V1 qualified as PLA
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )

-- | @since 1.0.0
deriving via Integer instance Arbitrary PLA.Lovelace

-- | @since 1.0.0
deriving via Integer instance CoArbitrary PLA.Lovelace

-- | @since 1.0.0
instance Function PLA.Lovelace where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Lovelace
