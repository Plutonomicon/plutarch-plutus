module Plutarch.Test.Suite.PlutarchLedgerApi.V3 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V3 qualified as PLA
import Plutarch.Prelude
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.QuickCheck (propPTryFromRoundtrip)
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
          , propPTryFromRoundtrip @PLA.PScriptContext
          ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundtrip @PLA.PTxInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptInfo)
        [ checkLedgerProperties @PLA.PScriptInfo
        , propPTryFromRoundtrip @PLA.PScriptInfo
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundtrip @PLA.PScriptPurpose
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOutRef)
        [ checkLedgerProperties @PLA.PTxOutRef
        , propPTryFromRoundtrip @PLA.PTxOutRef
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxId)
        [ checkLedgerProperties @PLA.PTxId
        , propPTryFromRoundtrip @PLA.PTxId
        ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInInfo)
          [ checkLedgerProperties @PLA.PTxInInfo
          , propPTryFromRoundtrip @PLA.PTxInInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxCert)
        [ checkLedgerProperties @PLA.PTxCert
        , propPTryFromRoundtrip @PLA.PTxCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDelegatee)
        [ checkLedgerProperties @PLA.PDelegatee
        , propPTryFromRoundtrip @PLA.PDelegatee
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDRepCredential)
        [ checkLedgerProperties @PLA.PDRepCredential
        , propPTryFromRoundtrip @PLA.PDRepCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PColdCommitteeCredential)
        [ checkLedgerProperties @PLA.PColdCommitteeCredential
        , propPTryFromRoundtrip @PLA.PColdCommitteeCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PHotCommitteeCredential)
        [ checkLedgerProperties @PLA.PHotCommitteeCredential
        , propPTryFromRoundtrip @PLA.PHotCommitteeCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDRep)
        [ checkLedgerProperties @PLA.PDRep
        , propPTryFromRoundtrip @PLA.PDRep
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PVoter)
        [ checkLedgerProperties @PLA.PVoter
        , propPTryFromRoundtrip @PLA.PVoter
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PGovernanceActionId)
        [ checkLedgerProperties @PLA.PGovernanceActionId
        , propPTryFromRoundtrip @PLA.PGovernanceActionId
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PVote)
        [ checkLedgerProperties @PLA.PVote
        , propPTryFromRoundtrip @PLA.PVote
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PProtocolVersion)
        [ checkLedgerProperties @PLA.PProtocolVersion
        , propPTryFromRoundtrip @PLA.PProtocolVersion
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
