module Derivation (goldens) where

import Plutarch.Primitive.Eq (peq)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGoldenAll)
import TH.MS (PTheseMS)
import TH.SOP (PEither, PThese)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Derivation"
    [ plutarchGoldenAll
        "peq @(PThese PInteger PInteger)"
        "Derivation Case 1"
        (peq @(PThese PInteger PInteger))
    , plutarchGoldenAll
        "peq @(PEither PInteger PByteString)"
        "Derivation Case 2"
        (peq @(PEither PInteger PInteger))
    , plutarchGoldenAll
        "peq @(PTheseMS PInteger PInteger)"
        "Derivation Case 3"
        (peq @(PTheseMS PInteger PInteger))
    ]
