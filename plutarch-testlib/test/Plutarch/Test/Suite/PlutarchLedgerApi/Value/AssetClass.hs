module Plutarch.Test.Suite.PlutarchLedgerApi.Value.AssetClass (
  tests,
) where

import Plutarch.LedgerApi.Value (PAssetClass)
import Plutarch.Prelude
import Plutarch.Test.Utils (prettyEquals)
import PlutusLedgerApi.V1.Orphans ()
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import Test.QuickCheck (arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (forAllShow, testProperty)

tests :: TestTree
tests =
  testGroup
    "PAssetClass"
    [ testProperty "Eq AssetClass = PEq PAssetClass" $
        forAllShow arbitrary show $
          \(ac0 :: PlutusValue.AssetClass, ac1 :: PlutusValue.AssetClass) ->
            prettyEquals
              (ac0 == ac1)
              (plift $ pconstant @PAssetClass ac0 #== pconstant ac1)
    , testProperty "Ord AssetClass = POrd PAssetClass" $
        forAllShow arbitrary show $
          \(ac0 :: PlutusValue.AssetClass, ac1 :: PlutusValue.AssetClass) ->
            prettyEquals
              (ac0 < ac1)
              (plift $ pconstant @PAssetClass ac0 #< pconstant ac1)
    ]
