module Plutarch.Test.Suite.PlutarchLedgerApi.Parse (
  tests,
) where

import Data.Kind (Type)
import Data.String (IsString)
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.LedgerApi.V2 qualified as V2
import Plutarch.Prelude
import Plutarch.Test.Unit (testEval)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PValidateData"
    [ testEval
        "PTxOutRef"
        ( pparseDataSmokeTest @V1.PTxOutRef
            (PlutusV1.TxOutRef blake2b256HashFixture 0)
        )
    , testEval
        "PPubKeyHash"
        ( pparseDataSmokeTest @V1.PPubKeyHash
            blake2b224HashFixture
        )
    , testGroup
        "PAddress"
        [ testEval
            "enterprise address"
            ( pparseDataSmokeTest @V1.PAddress
                ( PlutusV1.Address
                    (PlutusV1.PubKeyCredential blake2b224HashFixture)
                    Nothing
                )
            )
        ]
    , testGroup
        "POutputDatum"
        [ testEval
            "POutputDatumHash"
            ( pparseDataSmokeTest @V2.POutputDatum
                (PlutusV2.OutputDatumHash blake2b256HashFixture)
            )
        , testEval
            "POutputDatum"
            ( pparseDataSmokeTest @V2.POutputDatum
                ( PlutusV2.OutputDatum $
                    PlutusV2.Datum $
                      PlutusV2.BuiltinData $
                        PlutusV2.Constr 1 [PlutusV2.I 5, PlutusV2.B "deadbeef"]
                )
            )
        ]
    ]

pparseDataSmokeTest ::
  forall (a :: S -> Type) (s :: S).
  ( PLiftable a
  , PIsData a
  , PValidateData a
  ) =>
  AsHaskell a ->
  Term s (PAsData a)
pparseDataSmokeTest = pparseData @a . pforgetData . pdata . pconstant @a

-- 32 bytes
blake2b256HashFixture :: forall (a :: Type). IsString a => a
blake2b256HashFixture = "5849c6eb23693ac058fc76b5a2eac7a2e9f10a1f6f153bd681b559024fb75b8c"

-- 28 bytes
blake2b224HashFixture :: forall (a :: Type). IsString a => a
blake2b224HashFixture = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"
