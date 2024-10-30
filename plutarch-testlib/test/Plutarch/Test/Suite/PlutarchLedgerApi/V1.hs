module Plutarch.Test.Suite.PlutarchLedgerApi.V1 (tests) where

import Plutarch.LedgerApi.V1 qualified as PLA
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Test.Laws (
  checkHaskellIntegralEquivalent,
  checkHaskellNumEquivalent,
  checkLedgerProperties,
  checkLedgerPropertiesAssocMap,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
  checkLedgerPropertiesValue,
 )
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Interval qualified as Interval
import Plutarch.Test.Utils (fewerTests)
import PlutusLedgerApi.V1 (POSIXTime)
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
        "PPosixTime"
        [ checkLedgerPropertiesPCountable @PLA.PPosixTime
        , checkLedgerPropertiesPEnumerable @PLA.PPosixTime
        , checkHaskellNumEquivalent @POSIXTime
        , checkHaskellIntegralEquivalent @POSIXTime
        , checkLedgerProperties @PLA.PPosixTime
        ]
    , -- We only care about intervals of PPosixTime, so we don't check anything else
      checkLedgerProperties @(PLA.PExtended PLA.PPosixTime)
    , checkLedgerProperties @(PLA.PLowerBound PLA.PPosixTime)
    , checkLedgerProperties @(PLA.PUpperBound PLA.PPosixTime)
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
    , checkLedgerPropertiesValue
    , checkLedgerPropertiesAssocMap
    , checkLedgerProperties @Value.PAssetClass
    ]
