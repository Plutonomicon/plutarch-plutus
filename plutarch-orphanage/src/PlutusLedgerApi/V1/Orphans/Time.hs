{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Time () where

import Data.Coerce (coerce)
import PlutusLedgerApi.V1 qualified as PLA
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )

-- | @since 1.0.0
deriving via Integer instance Arbitrary PLA.POSIXTime

-- | @since 1.0.0
deriving via Integer instance CoArbitrary PLA.POSIXTime

-- | @since 1.0.0
instance Function PLA.POSIXTime where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.POSIXTime
