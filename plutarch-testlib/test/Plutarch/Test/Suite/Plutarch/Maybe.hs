module Plutarch.Test.Suite.Plutarch.Maybe (tests) where

import Plutarch.LedgerApi.Utils (PMaybeData, pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.Maybe (pmapMaybe)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEvalEqual, goldenGroup, plutarchGolden)
import Plutarch.Test.QuickCheck (checkHaskellEquivalent, propEvalEqual)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Maybe"
    [ plutarchGolden
        "Goldens"
        "maybe"
        [ goldenGroup
            "eq"
            [ goldenGroup
                "true"
                [ goldenEvalEqual "nothing" (pcon @(PMaybe PInteger) PNothing #== pcon PNothing) (pcon PTrue)
                , goldenEvalEqual "just" (pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42)) (pcon PTrue)
                ]
            , goldenGroup
                "false"
                [ goldenEvalEqual "nothing-just" (pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42)) (pcon PFalse)
                , goldenEvalEqual "just-just" (pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42)) (pcon PFalse)
                ]
            ]
        ]
    , propEvalEqual
        "pmaybeToMaybeData . pmaybeDataToMaybe = id"
        (\(m :: Maybe Integer) -> pmaybeToMaybeData #$ pmaybeDataToMaybe # pconstant m)
        (\(m :: Maybe Integer) -> pconstant m)
    , testProperty "fmap = pmapMaybe" $
        checkHaskellEquivalent @(PMaybeData PInteger) @(PMaybeData PBool)
          (fmap even)
          (plam $ \m -> pmaybeToMaybeData #$ pmapMaybe # peven #$ pmaybeDataToMaybe # m)
    ]

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0
