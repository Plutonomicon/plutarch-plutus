module Plutarch.Test.Suite.PlutarchLedgerApi.V4 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V4 qualified as PLA
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.QuickCheck (propPTryFromRoundtrip)
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V4.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V4"
    [ testGroup
        (typeName @(S -> Type) @PLA.PRationalData)
        [ checkLedgerProperties @PLA.PRationalData
        , propPTryFromRoundtrip @PLA.PRationalData
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PPosixTimeRange)
        [ checkLedgerProperties @PLA.PPosixTimeRange
        , propPTryFromRoundtrip @PLA.PPosixTimeRange
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PGovernanceActionId)
        [ checkLedgerProperties @PLA.PGovernanceActionId
        , propPTryFromRoundtrip @PLA.PGovernanceActionId
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PProtocolVersion)
        [ checkLedgerProperties @PLA.PProtocolVersion
        , propPTryFromRoundtrip @PLA.PProtocolVersion
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PConstitution)
        [ checkLedgerProperties @PLA.PConstitution
        , propPTryFromRoundtrip @PLA.PConstitution
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCommittee)
        [ checkLedgerProperties @PLA.PCommittee
        , propPTryFromRoundtrip @PLA.PCommittee
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PGovernanceAction)
        [ checkLedgerProperties @PLA.PGovernanceAction
        , propPTryFromRoundtrip @PLA.PGovernanceAction
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PProposalProcedure)
        [ checkLedgerProperties @PLA.PProposalProcedure
        , propPTryFromRoundtrip @PLA.PProposalProcedure
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAccountId)
        [ checkLedgerProperties @PLA.PAccountId
        , propPTryFromRoundtrip @PLA.PAccountId
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAddress)
        [ checkLedgerProperties @PLA.PAddress
        , propPTryFromRoundtrip @PLA.PAddress
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOut)
        [ checkLedgerProperties @PLA.PTxOut
        , propPTryFromRoundtrip @PLA.PTxOut
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAccountBalanceInterval)
        [ checkLedgerProperties @PLA.PAccountBalanceInterval
        , propPTryFromRoundtrip @PLA.PAccountBalanceInterval
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAccountBalanceIntervals)
        [ checkLedgerProperties @PLA.PAccountBalanceIntervals
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxCert)
        [ checkLedgerProperties @PLA.PTxCert
        , propPTryFromRoundtrip @PLA.PTxCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundtrip @PLA.PScriptPurpose
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxInInfo)
        [ checkLedgerProperties @PLA.PTxInInfo
        , propPTryFromRoundtrip @PLA.PTxInInfo
        ]
    , adjustOption (fewerTests 8) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundtrip @PLA.PTxInfo
          ]
    , adjustOption (fewerTests 8) $
        testGroup
          (typeName @(S -> Type) @PLA.PTopTxInfoSimplified)
          [ checkLedgerProperties @PLA.PTopTxInfoSimplified
          , propPTryFromRoundtrip @PLA.PTopTxInfoSimplified
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTopTxInfo)
          [ checkLedgerProperties @PLA.PTopTxInfo
          , propPTryFromRoundtrip @PLA.PTopTxInfo
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptInfo)
          [ checkLedgerProperties @PLA.PScriptInfo
          , propPTryFromRoundtrip @PLA.PScriptInfo
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptContext)
          [ checkLedgerProperties @PLA.PScriptContext
          , propPTryFromRoundtrip @PLA.PScriptContext
          ]
    ]
