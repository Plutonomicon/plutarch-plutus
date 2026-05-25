module Plutarch.Test.Suite.Plutarch.Either (tests) where

import Plutarch.Either (PEitherData (PDLeft, PDRight))
import Plutarch.Evaluate (evalTerm')
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.LedgerApi.V1 (PPosixTime)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.Methods (pmaxDefaultBetter, pminDefaultBetter)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Either"
    [ plutarchGolden
        "Goldens"
        "either"
        [ goldenGroup
            "eq"
            [ goldenGroup
                "true"
                [ goldenEval "left" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PLeft 42))
                , goldenEval "right" (pcon @(PEither PInteger PInteger) (PRight 42) #== pcon (PRight 42))
                ]
            , goldenGroup
                "false"
                [ goldenEval "left-right" (pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PRight 42))
                , goldenEval "left-left" (pcon @(PEither PInteger PInteger) (PLeft 24) #== pcon (PLeft 42))
                , goldenEval "right-right" (pcon @(PEither PInteger PInteger) (PRight 24) #== pcon (PRight 42))
                ]
            ]
        ]
    , testGroup
        "PEitherData"
        [ checkLedgerProperties @(PEitherData PPosixTime PPosixTime)
        , pmaxDefaultBetter pdleft pdright
        , pminDefaultBetter pdleft pdright
        , plutarchGolden
            "Goldens"
            "either-data"
            [ goldenEval "lte" (pdleft #<= pdright)
            , goldenEval "lt" (pdleft #< pdright)
            , goldenEval "pmax" (pmax pdleft pdright)
            , goldenEval "pmin" (pmin pdleft pdright)
            ]
        ]
    ]
  where
    pdleft :: forall (s :: S). Term s (PEitherData PInteger PByteString)
    pdleft = evalTerm' NoTracing $ pcon . PDLeft . pdata $ 10
    pdright :: forall (s :: S). Term s (PEitherData PInteger PByteString)
    pdright = evalTerm' NoTracing $ pcon . PDRight . pdata . pconstant $ "foo"
