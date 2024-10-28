module Plutarch.Test.Suite.PlutarchLedgerApi.V2 (tests) where

import Plutarch.LedgerApi.V2 qualified as PLA
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.Utils (fewerTests)
import PlutusLedgerApi.V2.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V2"
    [ adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PScriptContext
    , adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PTxInfo
    , adjustOption (fewerTests 4) $ checkLedgerProperties @PLA.PTxOut
    , adjustOption (fewerTests 4) $ checkLedgerProperties @PLA.PTxInInfo
    , checkLedgerProperties @PLA.POutputDatum
    ]
