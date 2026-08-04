module Derivation (goldens) where

import Plutarch.Primitive.Eq (peq)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenEval)
import TH.MS (PTheseMS)
import TH.SOP (PEither, PThese)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Derivation"
    [ plutarchGolden
        "peq @(PThese PInteger PInteger)"
        "Derivation Case 1"
        (peq @(PThese PInteger PInteger))
    , plutarchGoldenEval
        "peq @(PThese PInteger PInteger)"
        "Derivation Case 1"
        (peq @(PThese PInteger PInteger))
    , plutarchGolden
        "peq @(PEither PInteger PByteString)"
        "Derivation Case 2"
        (peq @(PEither PInteger PInteger))
    , plutarchGoldenEval
        "peq @(PEither PInteger PByteString)"
        "Derivation Case 2"
        (peq @(PEither PInteger PInteger))
    , plutarchGolden
        "peq @(PTheseMS PInteger PInteger)"
        "Derivation Case 3"
        (peq @(PTheseMS PInteger PInteger))
    , plutarchGoldenEval
        "peq @(PTheseMS PInteger PInteger)"
        "Derivation Case 3"
        (peq @(PTheseMS PInteger PInteger))
    ]
