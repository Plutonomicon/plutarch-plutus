module Plutarch.Test.Suite.PlutarchLedgerApi.V3.Value (
  tests,
) where

import Plutarch.Builtin.Value (PBuiltinValue)
import Plutarch.LedgerApi.V3 (PMintValue)
import Plutarch.LedgerApi.V3.Value (pfromMintValue, ptoSortedValue)
import Plutarch.LedgerApi.Value (pforgetSorted, ppromoteToSortedValue)
import Plutarch.Prelude (
  PAsData,
  PBool (PTrue),
  S,
  Term,
  pcon,
  pconstant,
  pfromData,
  plam,
  plet,
  plift,
  (#),
 )
import Plutarch.Test.Utils (precompileTerm)
import PlutusCore.Value qualified as PlutusCore
import PlutusLedgerApi.V3 qualified as PLA
import PlutusLedgerApi.V3.Orphans ()
import Test.QuickCheck (Property, arbitrary, forAllShrinkShow, shrink)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "PBuiltinValue"
    [ testProperty "PMintValues convert to PBuiltinValues without error" propMintToBuiltin
    , testProperty "PBuiltinValues convert to PSortedValues without error" propBuiltinToSorted
    ]

-- Properties

propMintToBuiltin :: Property
propMintToBuiltin = forAllShrinkShow arbitrary shrink show $ \(mv :: PLA.MintValue) ->
  plift (precompileTerm (plam go) # pconstant mv)
  where
    go ::
      forall (s :: S).
      Term s (PAsData PMintValue) ->
      Term s PBool
    go mv = plet (pfromMintValue mv) $ \_ -> pcon PTrue

propBuiltinToSorted :: Property
propBuiltinToSorted = forAllShrinkShow arbitrary shrink show $ \(v :: PlutusCore.Value) ->
  plift (precompileTerm (plam go) # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PBuiltinValue ->
      Term s PBool
    go v = plet (pfromData . ptoSortedValue $ v) $ \sv ->
      plet (ppromoteToSortedValue # pforgetSorted sv) $ \_ ->
        pcon PTrue
