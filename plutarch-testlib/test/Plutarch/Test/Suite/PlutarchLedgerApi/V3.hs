module Plutarch.Test.Suite.PlutarchLedgerApi.V3 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V3 qualified as PLA
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.QuickCheck (propPTryFromRoundrip)
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V3.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V3"
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
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptInfo)
        [ checkLedgerProperties @PLA.PScriptInfo
        , propPTryFromRoundrip @PLA.PScriptInfo
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundrip @PLA.PScriptPurpose
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOutRef)
        [ checkLedgerProperties @PLA.PTxOutRef
        , propPTryFromRoundrip @PLA.PTxOutRef
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxId)
        [ checkLedgerProperties @PLA.PTxId
        , propPTryFromRoundrip @PLA.PTxId
        ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInInfo)
          [ checkLedgerProperties @PLA.PTxInInfo
          , propPTryFromRoundrip @PLA.PTxInInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxCert)
        [ checkLedgerProperties @PLA.PTxCert
        , propPTryFromRoundrip @PLA.PTxCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDelegatee)
        [ checkLedgerProperties @PLA.PDelegatee
        , propPTryFromRoundrip @PLA.PDelegatee
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDRepCredential)
        [ checkLedgerProperties @PLA.PDRepCredential
        , propPTryFromRoundrip @PLA.PDRepCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PColdCommitteeCredential)
        [ checkLedgerProperties @PLA.PColdCommitteeCredential
        , propPTryFromRoundrip @PLA.PColdCommitteeCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PHotCommitteeCredential)
        [ checkLedgerProperties @PLA.PHotCommitteeCredential
        , propPTryFromRoundrip @PLA.PHotCommitteeCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDRep)
        [ checkLedgerProperties @PLA.PDRep
        , propPTryFromRoundrip @PLA.PDRep
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PVoter)
        [ checkLedgerProperties @PLA.PVoter
        , propPTryFromRoundrip @PLA.PVoter
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PGovernanceActionId)
        [ checkLedgerProperties @PLA.PGovernanceActionId
        , propPTryFromRoundrip @PLA.PGovernanceActionId
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PVote)
        [ checkLedgerProperties @PLA.PVote
        , propPTryFromRoundrip @PLA.PVote
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PProtocolVersion)
        [ checkLedgerProperties @PLA.PProtocolVersion
        , propPTryFromRoundrip @PLA.PProtocolVersion
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PProposalProcedure)
        [ checkLedgerProperties @PLA.PProposalProcedure
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PGovernanceAction)
        [ checkLedgerProperties @PLA.PGovernanceAction
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PChangedParameters)
        [ checkLedgerProperties @PLA.PChangedParameters
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PConstitution)
        [ checkLedgerProperties @PLA.PConstitution
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCommittee)
        [ checkLedgerProperties @PLA.PCommittee
        ]
    ]
