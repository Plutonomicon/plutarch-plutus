module Plutarch.Test.Suite.PlutarchLedgerApi.V2 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V2 qualified as PLA
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.QuickCheck (propPTryFromRoundrip)
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V2.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V2"
    [ adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptContext)
          [ checkLedgerProperties @PLA.PScriptContext
          , propPTryFromRoundrip @PLA.PScriptContext
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundrip @PLA.PTxInfo
          ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxOut)
          [ checkLedgerProperties @PLA.PTxOut
          , propPTryFromRoundrip @PLA.PTxOut
          ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInInfo)
          [ checkLedgerProperties @PLA.PTxInInfo
          , propPTryFromRoundrip @PLA.PTxInInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.POutputDatum)
        [ checkLedgerProperties @PLA.POutputDatum
        , propPTryFromRoundrip @PLA.POutputDatum
        ]
    ]
