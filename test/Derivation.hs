module Derivation (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Eq (peq)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGoldenAll)
import TH.MS (PTheseMS)
import TH.SOP (PEither, PSOPList (PSOPCons, PSOPNil), PThese)
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
    , plutarchGoldenAll
        "peq @(PSOPList PInteger)"
        "Derivation Case 4"
        case4
    ]

-- Cases

case4 :: forall (s :: S). Term s PBool
case4 = peq # ell1 # ell2
  where
    ell1 :: Term s (PSOPList PInteger)
    ell1 = pcon PSOPNil
    ell2 :: Term s (PSOPList PInteger)
    ell2 =
      pcon
        ( PSOPCons
            (pconstant @PInteger 3)
            ( pcon
                ( PSOPCons
                    (pconstant @PInteger 2)
                    (pcon (PSOPCons (pconstant @PInteger 1) (pcon PSOPNil)))
                )
            )
        )
