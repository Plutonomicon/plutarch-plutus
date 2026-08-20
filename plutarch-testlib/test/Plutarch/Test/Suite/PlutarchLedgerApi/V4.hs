module Plutarch.Test.Suite.PlutarchLedgerApi.V4 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V4 qualified as PLA
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.QuickCheck (propPTryFromRoundrip)
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V4.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V4"
    [ testGroup
        (typeName @(S -> Type) @PLA.PAccountId)
        [ checkLedgerProperties @PLA.PAccountId
        , propPTryFromRoundrip @PLA.PAccountId
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAddress)
        [ checkLedgerProperties @PLA.PAddress
        , propPTryFromRoundrip @PLA.PAddress
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOut)
        [ checkLedgerProperties @PLA.PTxOut
        , propPTryFromRoundrip @PLA.PTxOut
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAccountBalanceInterval)
        [ checkLedgerProperties @PLA.PAccountBalanceInterval
        , propPTryFromRoundrip @PLA.PAccountBalanceInterval
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAccountBalanceIntervals)
        [ checkLedgerProperties @PLA.PAccountBalanceIntervals
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxCert)
        [ checkLedgerProperties @PLA.PTxCert
        , propPTryFromRoundrip @PLA.PTxCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundrip @PLA.PScriptPurpose
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxInInfo)
        [ checkLedgerProperties @PLA.PTxInInfo
        , propPTryFromRoundrip @PLA.PTxInInfo
        ]
    , adjustOption (fewerTests 8) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundrip @PLA.PTxInfo
          ]
    , adjustOption (fewerTests 8) $
        testGroup
          (typeName @(S -> Type) @PLA.PTopTxInfoSimplified)
          [ checkLedgerProperties @PLA.PTopTxInfoSimplified
          , propPTryFromRoundrip @PLA.PTopTxInfoSimplified
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTopTxInfo)
          [ checkLedgerProperties @PLA.PTopTxInfo
          , propPTryFromRoundrip @PLA.PTopTxInfo
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptInfo)
          [ checkLedgerProperties @PLA.PScriptInfo
          , propPTryFromRoundrip @PLA.PScriptInfo
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptContext)
          [ checkLedgerProperties @PLA.PScriptContext
          , propPTryFromRoundrip @PLA.PScriptContext
          ]
    ]
