module Plutarch.Test.Suite.PlutarchLedgerApi.V1 (tests) where

import Plutarch.LedgerApi.V1 qualified as PLA
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import Plutarch.Test.Laws (
  checkHaskellIntegralEquivalent,
  checkHaskellNumEquivalent,
  checkHaskellOrdEquivalent,
  checkLedgerProperties,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
 )
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Interval qualified as Interval
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Value qualified as Value
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V1.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V1"
    [ checkLedgerProperties @PLA.PScriptPurpose
    , adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PScriptContext
    , checkLedgerProperties @PLA.PDCert
    , checkLedgerProperties @PLA.PCredential
    , checkLedgerProperties @PLA.PStakingCredential
    , checkLedgerProperties @PLA.PLovelace
    , checkLedgerProperties @PLA.PCurrencySymbol
    , checkLedgerProperties @PLA.PTokenName
    , testGroup
        (typeName @(S -> Type) @PLA.PPosixTime)
        [ checkLedgerPropertiesPCountable @PLA.PPosixTime
        , checkLedgerPropertiesPEnumerable @PLA.PPosixTime
        , checkHaskellNumEquivalent @PLA.PPosixTime
        , checkHaskellIntegralEquivalent @PLA.PPosixTime
        , checkLedgerProperties @PLA.PPosixTime
        ]
    , -- We only care about intervals of PPosixTime, so we don't check anything else
      testGroup
        (typeName @(S -> Type) @(PLA.PExtended PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PExtended PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PExtended PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PLowerBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PLowerBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PLowerBound PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PUpperBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PUpperBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PUpperBound PLA.PPosixTime)
        ]
    , Interval.tests
    , checkLedgerProperties @PLA.PDatum
    , checkLedgerProperties @PLA.PRedeemer
    , checkLedgerProperties @PLA.PDatumHash
    , checkLedgerProperties @PLA.PRedeemerHash
    , checkLedgerProperties @PLA.PScriptHash
    , checkLedgerProperties @PLA.PAddress
    , checkLedgerProperties @PLA.PTxId
    , adjustOption (fewerTests 4) $ checkLedgerProperties @PLA.PTxOut
    , adjustOption (fewerTests 4) $ checkLedgerProperties @PLA.PTxInInfo
    , checkLedgerProperties @PLA.PTxOutRef
    , checkLedgerProperties @PLA.PPubKeyHash
    , adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PTxInfo
    , Value.tests
    , checkLedgerProperties @Value.PAssetClass
    ]
