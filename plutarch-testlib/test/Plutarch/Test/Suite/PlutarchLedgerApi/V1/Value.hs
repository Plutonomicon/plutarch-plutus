module Plutarch.Test.Suite.PlutarchLedgerApi.V1.Value (tests) where

import Plutarch.Internal.Term (punsafeCoerce)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Sorted, Unsorted))
import Plutarch.LedgerApi.Value (AmountGuarantees (NoGuarantees, NonZero, Positive), PValue)
import Plutarch.LedgerApi.Value qualified as PValue
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerPropertiesValue)
import Plutarch.Test.QuickCheck (propEval)
import Plutarch.Test.Utils (precompileTerm, prettyEquals, prettyShow)
import PlutusLedgerApi.V1.Orphans (getUtxoValue)
import PlutusLedgerApi.V1.Value qualified as Value
import Test.Tasty (DependencyType (AllFinish), TestTree, after, testGroup)
import Test.Tasty.QuickCheck (arbitrary, forAllShrinkShow, shrink, testProperty)

tests :: TestTree
tests =
  testGroup
    "Value"
    [ checkLedgerPropertiesValue
    , testGroup
        "Generators sanity tests"
        [ propEval "UtxoValue is sorted" $
            \val ->
              precompileTerm (PValue.passertSorted @Unsorted @NoGuarantees)
                # pconstant (getUtxoValue val)
        , propEval "UtxoValue is positive" $
            \val ->
              precompileTerm (PValue.passertPositive @Unsorted @NoGuarantees)
                # pconstant (getUtxoValue val)
        ]
    , after AllFinish "$NF == \"Generators sanity tests\"" $
        testGroup
          "Haskell Equivalents"
          [ testProperty "leq = pleNonZero" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.leq lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.pleqNonZero plhs prhs)
                          # (ptoSortedNonZero # pconstant lhs)
                          # (ptoSortedNonZero # pconstant rhs)
                      )
          , testProperty "lt = pltNonZero" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.lt lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.pltNonZero plhs prhs)
                          # (ptoSortedNonZero # pconstant lhs)
                          # (ptoSortedNonZero # pconstant rhs)
                      )
          , testProperty "leq = plePositive" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.leq lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.pleqPositive plhs prhs)
                          # (ptoSortedPositive # pconstant lhs)
                          # (ptoSortedPositive # pconstant rhs)
                      )
          , testProperty "lt = pltPositive" $
              forAllShrinkShow arbitrary shrink prettyShow $
                \(getUtxoValue -> lhs, getUtxoValue -> rhs) ->
                  Value.lt lhs rhs
                    `prettyEquals` plift
                      ( precompileTerm (plam $ \plhs prhs -> PValue.pltPositive plhs prhs)
                          # (ptoSortedPositive # pconstant lhs)
                          # (ptoSortedPositive # pconstant rhs)
                      )
          ]
    ]

ptoSortedNonZero :: ClosedTerm (PValue 'Unsorted 'NoGuarantees :--> PValue 'Sorted 'NonZero)
ptoSortedNonZero = plam punsafeCoerce

ptoSortedPositive :: ClosedTerm (PValue 'Unsorted 'NoGuarantees :--> PValue 'Sorted 'Positive)
ptoSortedPositive = plam punsafeCoerce
