module Plutarch.Test.Suite.Plutarch.Maybe (tests) where

import Data.Kind (Type)
import Plutarch.Maybe (PMaybeSoP, pmapMaybe, pmaybeSoPToMaybe, pmaybeToMaybeSoP)
import Plutarch.LedgerApi.Utils (PMaybeData, pmapMaybeData, pmaybeDataToMaybe, pmaybeToMaybeData)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkPLiftableLaws)
import Plutarch.Test.QuickCheck (checkHaskellEquivalent, propEvalEqual)
import Plutarch.Test.Utils (instanceOfType)
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
                [ goldenEval "nothing" (pcon @(PMaybe PInteger) PNothing #== pcon PNothing)
                , goldenEval "just" (pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42))
                ]
            , goldenGroup
                "false"
                [ goldenEval "nothing-just" (pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42))
                , goldenEval "just-just" (pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42))
                ]
            ]
        ]
    , propEvalEqual
        "pmaybeToMaybeData . pmaybeDataToMaybe = id"
        (\(m :: Maybe Integer) -> pmaybeToMaybeData #$ pmaybeDataToMaybe # pconstant @(PMaybeData PInteger) m)
        (\(m :: Maybe Integer) -> pconstant m)
    , propEvalEqual
        "pmaybeDataToMaybe . pmaybeToMaybeData= id"
        (\(m :: Maybe Integer) -> pmaybeDataToMaybe #$ pmaybeToMaybeData # pconstant @(PMaybe PInteger) m)
        (\(m :: Maybe Integer) -> pconstant m)
    , propEvalEqual
        "pmaybeToMaybeSoP . pmaybeSoPToMaybe = id"
        (\(m :: Maybe Integer) -> pmaybeToMaybeSoP #$ pmaybeSoPToMaybe # pconstant @(PMaybeSoP PInteger) m)
        (\(m :: Maybe Integer) -> pconstant m)
    , propEvalEqual
        "pmaybeSoPToMaybe . pmaybeToMaybeSoP = id"
        (\(m :: Maybe Integer) -> pmaybeSoPToMaybe #$ pmaybeToMaybeSoP # pconstant @(PMaybe PInteger) m)
        (\(m :: Maybe Integer) -> pconstant m)
    , testProperty "fmap = pmapMaybe" $
        checkHaskellEquivalent @(PMaybeData PInteger) @(PMaybeData PBool)
          (fmap even)
          (plam $ \m -> pmaybeToMaybeData #$ pmapMaybe # peven #$ pmaybeDataToMaybe # m)
    , testProperty "fmap = pmapMaybeData" $
        checkHaskellEquivalent @(PMaybeData PInteger) @(PMaybeData PBool)
          (fmap even)
          (plam $ \m -> pmapMaybeData # plam (\v -> pdata (peven # pfromData v)) # m)
    , testGroup (instanceOfType @(S -> Type) @(PMaybe PInteger) "PLiftable") $
        checkPLiftableLaws @(PMaybe PInteger)
    , testGroup (instanceOfType @(S -> Type) @(PMaybeData PInteger) "PLiftable") $
        checkPLiftableLaws @(PMaybeData PInteger)
    , testGroup (instanceOfType @(S -> Type) @(PMaybeSoP PInteger) "PLiftable") $
        checkPLiftableLaws @(PMaybeSoP PInteger)
    ]

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0
