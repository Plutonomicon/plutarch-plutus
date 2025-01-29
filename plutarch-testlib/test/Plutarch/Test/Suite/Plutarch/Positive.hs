module Plutarch.Test.Suite.Plutarch.Positive (tests) where

import Plutarch.Maybe (pnothing)
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerPropertiesPCountable)
import Plutarch.Test.QuickCheck (propEvalEqual, propEvalFail)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonPositive (getNonPositive))

tests :: TestTree
tests =
  testGroup
    "Positive"
    [ checkLedgerPropertiesPCountable @PPositive
    , testGroup
        "Properties"
        [ propEvalFail "ptryPositive of non-positive" $
            \(input :: NonPositive Integer) -> ptryPositive # pconstant (getNonPositive input)
        , propEvalEqual
            "ppositive of non-positive"
            (\(input :: NonPositive Integer) -> ppositive # pconstant (getNonPositive input))
            (const pnothing)
        ]
    ]
