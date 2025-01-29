module Plutarch.Test.Suite.PlutarchLedgerApi.V3.Value (tests) where

import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Unsorted))
import Plutarch.LedgerApi.Value (AmountGuarantees (NoGuarantees))
import Plutarch.LedgerApi.Value qualified as PValue
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (propEval)
import Plutarch.Test.Utils (precompileTerm)
import PlutusLedgerApi.V3.Orphans (getMintValue)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Value"
    [ testGroup
        "Generators sanity tests"
        [ propEval "MintValue is sorted" $
            \val ->
              precompileTerm (PValue.passertSorted @Unsorted @NoGuarantees)
                # pconstant (getMintValue val)
        , propEval "MintValue is non-zero" $
            \val ->
              precompileTerm (PValue.passertNonZero @Unsorted @NoGuarantees)
                # pconstant (getMintValue val)
        ]
    ]
