{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans (UnsortedAssocMap, getUnsortedAssocMap) where

import PlutusLedgerApi.Orphans.Common (UnsortedAssocMap, getUnsortedAssocMap)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Address ()
import PlutusLedgerApi.V1.Orphans.Contexts ()
import PlutusLedgerApi.V1.Orphans.Credential ()
import PlutusLedgerApi.V1.Orphans.Crypto ()
import PlutusLedgerApi.V1.Orphans.DCert ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Scripts ()
import PlutusLedgerApi.V1.Orphans.Time ()
import PlutusLedgerApi.V1.Orphans.Tx ()
import PlutusLedgerApi.V1.Orphans.Value ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )

-- | @since 1.0.2
instance Arbitrary PLA.ScriptContext where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.ScriptContext <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.ScriptContext tinfo p) =
    PLA.ScriptContext <$> shrink tinfo <*> shrink p

-- | @since 1.0.2
instance CoArbitrary PLA.ScriptContext where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.ScriptContext tinfo p) =
    coarbitrary tinfo . coarbitrary p

-- | @since 1.0.2
instance Function PLA.ScriptContext where
  {-# INLINEABLE function #-}
  function =
    functionMap
      (\(PLA.ScriptContext tinfo p) -> (tinfo, p))
      (uncurry PLA.ScriptContext)
