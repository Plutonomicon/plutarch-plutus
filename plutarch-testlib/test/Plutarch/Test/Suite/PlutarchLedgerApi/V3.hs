module Plutarch.Test.Suite.PlutarchLedgerApi.V3 (tests) where

import Plutarch.LedgerApi.V3 qualified as PLA
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.Utils (fewerTests)
import PlutusLedgerApi.V3.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V3"
    [ adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PScriptContext
    , adjustOption (fewerTests 16) $ checkLedgerProperties @PLA.PTxInfo
    , checkLedgerProperties @PLA.PScriptInfo
    , checkLedgerProperties @PLA.PScriptPurpose
    , checkLedgerProperties @PLA.PTxOutRef
    , checkLedgerProperties @PLA.PTxId
    , adjustOption (fewerTests 4) $ checkLedgerProperties @PLA.PTxInInfo
    , checkLedgerProperties @PLA.PTxCert
    , checkLedgerProperties @PLA.PDelegatee
    , checkLedgerProperties @PLA.PDRepCredential
    , checkLedgerProperties @PLA.PColdCommitteeCredential
    , checkLedgerProperties @PLA.PHotCommitteeCredential
    , checkLedgerProperties @PLA.PDRep
    , checkLedgerProperties @PLA.PVoter
    , checkLedgerProperties @PLA.PGovernanceActionId
    , checkLedgerProperties @PLA.PVote
    , checkLedgerProperties @PLA.PProtocolVersion
    , checkLedgerProperties @PLA.PProposalProcedure
    , checkLedgerProperties @PLA.PGovernanceAction
    , checkLedgerProperties @PLA.PChangedParameters
    , checkLedgerProperties @PLA.PConstitution
    , checkLedgerProperties @PLA.PCommittee
    ]
