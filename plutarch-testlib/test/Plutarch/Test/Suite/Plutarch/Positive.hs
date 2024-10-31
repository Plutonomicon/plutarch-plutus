module Plutarch.Test.Suite.Plutarch.Positive (tests) where

import Plutarch.Internal.Numeric (PNum (pfromInteger))
import Plutarch.Maybe (pnothing)
import Plutarch.Positive (PPositive, ppositive, ptryPositive)
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerPropertiesPCountable)
import Plutarch.Test.QuickCheck (propCompileFail, propEvalEqual, propEvalFail)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonPositive (getNonPositive))

tests :: TestTree
tests =
  testGroup
    "Positive"
    [ checkLedgerPropertiesPCountable @PPositive
    , testGroup
        "Properties"
        [ propCompileFail "pfromInteger of non-positive" $
            \(input :: NonPositive Integer) -> pfromInteger @PPositive $ getNonPositive input
        , propEvalFail "ptryPositive of non-positive" $
            \(input :: NonPositive Integer) -> ptryPositive # pconstant (getNonPositive input)
        , propEvalEqual
            "ppositive of non-positive"
            (\(input :: NonPositive Integer) -> ppositive # pconstant (getNonPositive input))
            (const pnothing)
        ]
    ]
