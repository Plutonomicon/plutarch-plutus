module Plutarch.Test.Suite.PlutarchLedgerApi.V1.Value (tests) where

import Plutarch.Internal.Term (punsafeCoerce)
import Plutarch.LedgerApi.Value (PRawValue, PSortedValue)
import Plutarch.LedgerApi.Value qualified as PValue
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerPropertiesValue)
import Plutarch.Test.QuickCheck (propEval)
import Plutarch.Test.Utils (precompileTerm, prettyEquals, prettyShow)
import PlutusLedgerApi.V1.Orphans (getUtxoValue)
import PlutusLedgerApi.V1.Value qualified as Value
import Test.Tasty (DependencyType (AllSucceed), TestTree, after, testGroup)
import Test.Tasty.QuickCheck (arbitrary, forAllShrinkShow, shrink, testProperty)

tests :: TestTree
tests =
  testGroup
    "Value"
    [ checkLedgerPropertiesValue
    , testGroup
        "Generators sanity tests"
        [ propEval "UtxoValue is sorted" $ \val ->
            precompileTerm PValue.passertSorted
              # pconstant (getUtxoValue val)
              -- FIXME: remove this test case or re-add passertPositive
              -- , propEval "UtxoValue is positive" $ \val ->
              --    precompileTerm PValue.passertPositive # pconstant (getUtxoValue val)
        ]
    , after AllSucceed "$(NF-1) == \"Generators sanity tests\" && $(NF-2) == \"Value\" && $(NF-3) == \"V1\"" $
        testGroup
          "Haskell Equivalents"
          [ testProperty "lt = plt" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.lt lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.plt plhs prhs)
                          # (ptoSortedValue # pconstant lhs)
                          # (ptoSortedValue # pconstant rhs)
                      )
          , testProperty "leq = pleq" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.leq lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.pleq plhs prhs)
                          # (ptoSortedValue # pconstant lhs)
                          # (ptoSortedValue # pconstant rhs)
                      )
          ]
    ]

ptoSortedValue :: forall (s :: S). Term s (PRawValue :--> PSortedValue)
ptoSortedValue = plam punsafeCoerce
