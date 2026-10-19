{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V3 module in plutus-ledger-api
module Plutarch.LedgerApi.V3.Contexts (
  PColdCommitteeCredential (..),
  PHotCommitteeCredential (..),
  PDRepCredential (..),
  PDRep (..),
  PDelegatee (..),
  PTxCert (..),
  PVoter (..),
  PVote (..),
  PGovernanceActionId (..),
  PCommittee (..),
  PConstitution (..),
  PProtocolVersion (..),
  PChangedParameters (..),
  PGovernanceAction (..),
  PProposalProcedure (..),
  PScriptPurpose (..),
  PScriptInfo (..),
  PTxInInfo (..),
  PTxInfo (..),
  PScriptContext (..),
  pfindOwnInput,
  pfindDatum,
  pfindDatumHash,
  pparseDatum,
  pfindTxInByTxOutRef,
  pfindContinuingOutputs,
  pgetContinuingOutputs,
  ptxSignedBy,
  ppubKeyOutputsAt,
  pvaluePaidTo,
  pvalueSpent,
  pvalueProduced,
  pownCurrencySymbol,
  pspendsOutput,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils (PMaybeData, PRationalData)
import Plutarch.LedgerApi.V1.Address (PAddress (..))
import Plutarch.LedgerApi.V1.Credential (PCredential (PPubKeyCredential))
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.V1.Time (PPosixTime)
import Plutarch.LedgerApi.V2.Tx (PTxOut (..))
import Plutarch.LedgerApi.V3.MintValue qualified as MintValue
import Plutarch.LedgerApi.V3.Tx (PTxId, PTxOutRef (..))
import Plutarch.LedgerApi.Value (PLedgerValue, pemptyLedgerValue)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Maybe (pmapDropNothing, pmapMaybe)
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 3.1.0
newtype PColdCommitteeCredential (s :: S) = PColdCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PColdCommitteeCredential)
  deriving
    ( -- | @since 3.6.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PColdCommitteeCredential PCredential)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PColdCommitteeCredential Plutus.ColdCommitteeCredential
  instance
    PLiftable PColdCommitteeCredential

-- | @since 3.4.0
instance PTryFrom PData (PAsData PColdCommitteeCredential)

-- | @since 3.1.0
newtype PHotCommitteeCredential (s :: S) = PHotCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PHotCommitteeCredential)
  deriving
    ( -- | @since 3.6.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PHotCommitteeCredential PCredential)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PHotCommitteeCredential Plutus.HotCommitteeCredential
  instance
    PLiftable PHotCommitteeCredential

-- | @since 3.4.0
instance PTryFrom PData (PAsData PHotCommitteeCredential)

-- | @since 3.1.0
newtype PDRepCredential (s :: S) = PDRepCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PDRepCredential)
  deriving
    ( -- | @since 3.6.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PDRepCredential PCredential)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PDRepCredential Plutus.DRepCredential
  instance
    PLiftable PDRepCredential

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDRepCredential)

-- | @since 3.1.0
data PDRep (s :: S)
  = PDRep (Term s (PAsData PDRepCredential))
  | PDRepAlwaysAbstain
  | PDRepAlwaysNoConfidence
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PDRep)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PDRep Plutus.DRep
  instance
    PLiftable PDRep

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDRep)

-- | @since 3.1.0
data PDelegatee (s :: S)
  = PDelegStake (Term s (PAsData PPubKeyHash))
  | PDelegVote (Term s PDRep)
  | PDelegStakeVote (Term s (PAsData PPubKeyHash)) (Term s PDRep)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PDelegatee)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PDelegatee Plutus.Delegatee
  instance
    PLiftable PDelegatee

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDelegatee)

-- | @since 3.1.0
data PTxCert (s :: S)
  = PTxCertRegStaking (Term s PCredential) (Term s (PMaybeData Value.PLovelace))
  | PTxCertUnRegStaking (Term s PCredential) (Term s (PMaybeData Value.PLovelace))
  | PTxCertDelegStaking (Term s PCredential) (Term s PDelegatee)
  | PTxCertRegDeleg (Term s PCredential) (Term s PDelegatee) (Term s (PAsData Value.PLovelace))
  | PTxCertRegDRep (Term s PDRepCredential) (Term s (PAsData Value.PLovelace))
  | PTxCertUpdateDRep (Term s PDRepCredential)
  | PTxCertUnRegDRep (Term s PDRepCredential) (Term s (PAsData Value.PLovelace))
  | PTxCertPoolRegister (Term s (PAsData PPubKeyHash)) (Term s (PAsData PPubKeyHash))
  | PTxCertPoolRetire (Term s (PAsData PPubKeyHash)) (Term s (PAsData PInteger))
  | PTxCertAuthHotCommittee (Term s PColdCommitteeCredential) (Term s PHotCommitteeCredential)
  | PTxCertResignColdCommittee (Term s PColdCommitteeCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxCert)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxCert Plutus.TxCert
  instance
    PLiftable PTxCert

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxCert)

-- | @since 3.1.0
data PVoter (s :: S)
  = PCommitteeVoter (Term s PHotCommitteeCredential)
  | PDRepVoter (Term s PDRepCredential)
  | PStakePoolVoter (Term s (PAsData PPubKeyHash))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PVoter)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PVoter Plutus.Voter
  instance
    PLiftable PVoter

-- | @since 3.4.0
instance PTryFrom PData (PAsData PVoter)

-- | @since 3.1.0
data PVote (s :: S)
  = PVoteYes
  | PVoteNo
  | PAbstain
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PVote)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PVote Plutus.Vote
  instance
    PLiftable PVote

-- | @since 3.4.0
instance PTryFrom PData (PAsData PVote)

-- | @since 3.1.0
data PGovernanceActionId (s :: S)
  = PGovernanceActionId (Term s (PAsData PTxId)) (Term s (PAsData PInteger))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PGovernanceActionId)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PGovernanceActionId Plutus.GovernanceActionId
  instance
    PLiftable PGovernanceActionId

-- | @since 3.4.0
instance PTryFrom PData (PAsData PGovernanceActionId)

{- | Checks that we have a valid 'PGovernanceActionId'. The underlying 'PTxId'
must be exactly 32 bytes, as Cardano transactions are hashed with BLAKE2b-256,
and the action index must be a non-negative 'PInteger'.

@since 3.6.0
-}
instance PValidateData PGovernanceActionId where
  pwithValidated opq x =
    pmatch (pasConstr # opq) $ \(PBuiltinPair constrIdx fields) ->
      pif
        ((constrIdx #== 0) #&& ((plength # fields) #== 2))
        ( pwithValidated @PTxId (ptryIndex 0 fields) $
            plet (pasInt # ptryIndex 1 fields) $ \outIdx ->
              pif
                (outIdx #< 0)
                perror
                x
        )
        perror

-- | @since 3.1.0
data PCommittee (s :: S) = PCommittee
  { pcommittee'members :: Term s (PAsData (AssocMap.PUnsortedMap PColdCommitteeCredential PInteger))
  , pcommittee'quorum :: Term s PRationalData
  }
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PCommittee)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PCommittee Plutus.Committee
  instance
    PLiftable PCommittee

-- | @since 3.4.0
instance PTryFrom PData (PAsData PCommittee)

{- | A constitution, omitting the optional anchor.

@since 3.1.0
-}
newtype PConstitution (s :: S) = PConstitution (Term s (PMaybeData PScriptHash))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PConstitution)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PConstitution Plutus.Constitution
  instance
    PLiftable PConstitution

-- | @since 3.4.0
instance PTryFrom PData (PAsData PConstitution)

-- | @since 3.1.0
data PProtocolVersion (s :: S) = PProtocolVersion
  { pprotocolVersion'major :: Term s (PAsData PInteger)
  , pprotocolVersion'minor :: Term s (PAsData PInteger)
  }
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PProtocolVersion)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PProtocolVersion Plutus.ProtocolVersion
  instance
    PLiftable PProtocolVersion

-- | @since 3.4.0
instance PTryFrom PData (PAsData PProtocolVersion)

-- | @since 3.1.0
newtype PChangedParameters (s :: S)
  = PChangedParameters (Term s PData)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PChangedParameters)
  deriving
    ( -- | @since 3.6.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PChangedParameters PData)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PChangedParameters Plutus.ChangedParameters
  instance
    PLiftable PChangedParameters

-- | @since 3.4.0
instance PTryFrom PData (PAsData PChangedParameters)

-- | @since 3.1.0
data PGovernanceAction (s :: S)
  = PParameterChange (Term s (PMaybeData PGovernanceActionId)) (Term s PChangedParameters) (Term s (PMaybeData PScriptHash))
  | PHardForkInitiation (Term s (PMaybeData PGovernanceActionId)) (Term s PProtocolVersion)
  | PTreasuryWithdrawals
      (Term s (PAsData (AssocMap.PUnsortedMap PCredential Value.PLovelace)))
      (Term s (PMaybeData PScriptHash))
  | PNoConfidence (Term s (PMaybeData PGovernanceActionId))
  | PUpdateCommittee
      (Term s (PMaybeData PGovernanceActionId))
      (Term s (PAsData (PBuiltinList (PAsData PColdCommitteeCredential))))
      (Term s (PAsData (AssocMap.PUnsortedMap PColdCommitteeCredential PInteger)))
      (Term s PRationalData)
  | PNewConstitution (Term s (PMaybeData PGovernanceActionId)) (Term s PConstitution)
  | PInfoAction
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PGovernanceAction)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PGovernanceAction Plutus.GovernanceAction
  instance
    PLiftable PGovernanceAction

-- | @since 3.4.0
instance PTryFrom PData (PAsData PGovernanceAction)

-- | @since 3.1.0
data PProposalProcedure (s :: S) = PProposalProcedure
  { pproposalProcedure'deposit :: Term s (PAsData Value.PLovelace)
  , pproposalProcedure'returnAddr :: Term s PCredential
  , pproposalProcedure'governanceAction :: Term s PGovernanceAction
  }
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PProposalProcedure)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PProposalProcedure Plutus.ProposalProcedure
  instance
    PLiftable PProposalProcedure

-- | @since 3.4.0
instance PTryFrom PData (PAsData PProposalProcedure)

-- | @since 2.0.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PAsData Value.PCurrencySymbol))
  | PSpending (Term s PTxOutRef)
  | -- | @since 3.1.0
    PRewarding (Term s PCredential)
  | PCertifying (Term s (PAsData PInteger)) (Term s PTxCert)
  | -- | @since 3.1.0
    PVoting (Term s PVoter)
  | -- | @since 3.1.0
    PProposing (Term s (PAsData PInteger)) (Term s PProposalProcedure)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptPurpose)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptPurpose)

-- | @since 3.1.0
data PScriptInfo (s :: S)
  = PMintingScript (Term s (PAsData Value.PCurrencySymbol))
  | PSpendingScript (Term s PTxOutRef) (Term s (PMaybeData PDatum))
  | PRewardingScript (Term s PCredential)
  | PCertifyingScript (Term s (PAsData PInteger)) (Term s PTxCert)
  | PVotingScript (Term s PVoter)
  | PProposingScript (Term s (PAsData PInteger)) (Term s PProposalProcedure)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptInfo Plutus.ScriptInfo
  instance
    PLiftable PScriptInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptInfo)

{- | An input of the transaction.

@since 2.0.0
-}
data PTxInInfo (s :: S) = PTxInInfo
  { ptxInInfo'outRef :: Term s PTxOutRef
  , ptxInInfo'resolved :: Term s PTxOut
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxInInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxInInfo Plutus.TxInInfo
  instance
    PLiftable PTxInInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxInInfo)

-- A pending transaction. This is the view as seen by a validator script.
--
-- @since 3.3.0
data PTxInfo (s :: S) = PTxInfo
  { ptxInfo'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'outputs :: Term s (PAsData (PBuiltinList (PAsData PTxOut)))
  , ptxInfo'fee :: Term s (PAsData Value.PLovelace)
  , ptxInfo'mint :: Term s (PAsData MintValue.PMintValue) -- value minted by transaction
  , ptxInfo'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , ptxInfo'wdrl :: Term s (PAsData (AssocMap.PUnsortedMap PCredential Value.PLovelace)) -- Staking withdrawals
  , ptxInfo'validRange :: Term s (Interval.PInterval PPosixTime)
  , ptxInfo'signatories :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , ptxInfo'redeemers :: Term s (PAsData (AssocMap.PUnsortedMap PScriptPurpose PRedeemer))
  , ptxInfo'data :: Term s (PAsData (AssocMap.PUnsortedMap PDatumHash PDatum))
  , ptxInfo'id :: Term s (PAsData PTxId) -- hash of the pending transaction
  , ptxInfo'votes :: Term s (PAsData (AssocMap.PUnsortedMap PVoter (AssocMap.PUnsortedMap PGovernanceActionId PVote)))
  , ptxInfo'proposalProcedures :: Term s (PAsData (PBuiltinList (PAsData PProposalProcedure)))
  , ptxInfo'currentTreasuryAmount :: Term s (PMaybeData Value.PLovelace)
  , ptxInfo'treasuryDonation :: Term s (PMaybeData Value.PLovelace)
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxInfo Plutus.TxInfo
  instance
    PLiftable PTxInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxInfo)

-- | @since 3.1.0
data PScriptContext (s :: S) = PScriptContext
  { pscriptContext'txInfo :: Term s PTxInfo
  , pscriptContext'redeemer :: Term s PRedeemer
  , pscriptContext'scriptInfo :: Term s PScriptInfo
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    , -- | @since 3.6.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptContext)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptContext)

{- | Find the input currently being validated.

@since 3.6.0
-}
pfindOwnInput :: forall (s :: S). Term s (PScriptContext :--> PMaybe PTxInInfo)
pfindOwnInput =
  phoistAcyclic $
    plam $ \scriptCtx ->
      pmatch scriptCtx $ \ctx ->
        plet (pscriptContext'scriptInfo ctx) $ \scriptInfo ->
          pmatch scriptInfo $ \case
            PSpendingScript outRef _ ->
              pfindTxInByTxOutRef # outRef # pscriptContext'txInfo ctx
            _ ->
              pcon PNothing

{- | Find the datum corresponding to a datum hash, if there is one.

@since 3.1.0
-}
pfindDatum :: forall (s :: S). Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $ plam $ \dh txI ->
  pmatch txI $ \tx ->
    AssocMap.plookup # dh # AssocMap.punsafeCoerceToSortedMap (pfromData (ptxInfo'data tx))

{- | Find the hash of a datum if it's part of the pending transaction's hashes.

@since 3.1.0
-}
pfindDatumHash :: forall (s :: S). Term s (PDatum :--> PTxInfo :--> PMaybe PDatumHash)
pfindDatumHash = phoistAcyclic $ plam $ \d txI ->
  pmatch txI $ \tx ->
    pmatch (pfromData (ptxInfo'data tx)) $ \(AssocMap.PUnsortedMap ell) ->
      pmatch (pfind # (matches # d) # pto ell) $ \case
        PNothing -> pcon PNothing
        PJust p -> pmatch p $ \(PBuiltinPair pFst _) ->
          pcon . PJust . pfromData $ pFst
  where
    matches ::
      forall (s' :: S).
      Term
        s'
        ( PDatum
            :--> PBuiltinPair (PAsData PDatumHash) (PAsData PDatum)
            :--> PBool
        )
    matches = phoistAcyclic $ plam $ \needle p ->
      pmatch p $ \(PBuiltinPair _ thing) ->
        needle #== pfromData thing

{- | Lookup up the datum given the datum hash.

  Takes as argument the datum assoc list from a `PTxInfo`. Validates the datum
  using `PTryFrom`.

  __Example:__

  @
  pparseDatum @MyType # datumHash #$ pfield @"datums" # txinfo
  @

  @since 2.1.2
-}
pparseDatum ::
  forall (a :: S -> Type) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s (PDatumHash :--> AssocMap.PUnsortedMap PDatumHash PDatum :--> PMaybe (PAsData a))
pparseDatum = phoistAcyclic $ plam $ \dh datums ->
  pmatch (AssocMap.plookup # dh # AssocMap.punsafeCoerceToSortedMap datums) $ \case
    PNothing -> pcon PNothing
    PJust datum -> pcon . PJust $ ptryFrom (pto datum) fst

{- | Given a UTXO reference and a transaction ('PTxInfo'), resolve it to one of
the transaction's inputs ('PTxInInfo'). If no matching input exists, the result
is 'PNothing'.

= NOTE

This only searches the true transaction inputs and not the referenced
transaction inputs.

@since 3.6.0
-}
pfindTxInByTxOutRef ::
  forall (s :: S).
  Term
    s
    ( PTxOutRef
        :--> PTxInfo
        :--> PMaybe PTxInInfo
    )
pfindTxInByTxOutRef = phoistAcyclic $
  plam $ \outRef txInfo ->
    pmatch txInfo $ \tx ->
      plet (ptxInfo'inputs tx) $ \inputs ->
        pmapMaybe # plam pfromData #$ pfind # (matches # outRef) # pfromData inputs
  where
    matches ::
      forall (s' :: S).
      Term s' (PTxOutRef :--> PAsData PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outRef txininfo ->
        pmatch (pfromData txininfo) $ \ininfo ->
          outRef #== ptxInInfo'outRef ininfo

{- | Find the indices of all the outputs that pay to the same script address we
are currently spending from, if any.

@since 3.6.0
-}
pfindContinuingOutputs :: forall (s :: S). Term s (PScriptContext :--> PBuiltinList PInteger)
pfindContinuingOutputs =
  phoistAcyclic $
    plam $ \scriptCtx ->
      pmatch (pfindOwnInput # scriptCtx) $ \case
        PNothing ->
          ptraceInfoError "can't find any continuing outputs: invalid script purpose"
        PJust ownInput ->
          unTermCont $ do
            ctx <- pmatchC scriptCtx
            txInfo <- pmatchC $ pscriptContext'txInfo ctx
            outputs <- pletC $ pfromData $ ptxInfo'outputs txInfo
            txInInfo <- pmatchC ownInput
            resolved <- pmatchC $ ptxInInfo'resolved txInInfo
            addr <- pletC $ ptxOut'address resolved
            PPair indices _ <-
              pmatchC $
                pfoldr
                  # plam
                    ( \out acc ->
                        pmatch acc $ \(PPair indices currentIdx) ->
                          pmatch (pfromData out) $ \txOut ->
                            pif
                              (ptxOut'address txOut #== addr)
                              (pcon $ PPair (pcons # currentIdx # indices) (currentIdx #- 1))
                              (pcon $ PPair indices (currentIdx #- 1))
                    )
                  # pcon (PPair pnil $ (plength # outputs) #- 1)
                  # outputs
            pure indices

{- | Get all the outputs that pay to the same script address we are currently
spending from, if any.

@since 3.6.0
-}
pgetContinuingOutputs :: forall (s :: S). Term s (PScriptContext :--> PBuiltinList (PAsData PTxOut))
pgetContinuingOutputs =
  phoistAcyclic $
    plam $ \scriptCtx ->
      pmatch (pfindOwnInput # scriptCtx) $ \case
        PNothing ->
          ptraceInfoError "can't get any continuing outputs: invalid script purpose"
        PJust ownInput ->
          unTermCont $ do
            ctx <- pmatchC scriptCtx
            txInfo <- pmatchC $ pscriptContext'txInfo ctx
            outputs <- pletC $ pfromData $ ptxInfo'outputs txInfo
            txInInfo <- pmatchC ownInput
            resolved <- pmatchC $ ptxInInfo'resolved txInInfo
            addr <- pletC $ ptxOut'address resolved
            pure $
              pfilter
                # plam
                  ( \out ->
                      pmatch (pfromData out) $ \txOut ->
                        ptxOut'address txOut #== addr
                  )
                # outputs

{- | Check if a transaction was signed by the given public key.

@since 3.6.0
-}
ptxSignedBy :: forall (s :: S). Term s (PTxInfo :--> PPubKeyHash :--> PBool)
ptxSignedBy =
  plam $ \txInfo pkh ->
    pmatch txInfo $ \tx ->
      plet (pdata pkh) $ \pkhData ->
        pelem # pkhData # pfromData (ptxInfo'signatories tx)

{- | Get the Values paid to a public key address by a pending transaction.

@since 3.6.0
-}
ppubKeyOutputsAt ::
  forall (s :: S).
  Term
    s
    ( PPubKeyHash
        :--> PTxInfo
        :--> PBuiltinList (PAsData PLedgerValue)
    )
ppubKeyOutputsAt =
  phoistAcyclic $
    plam $ \targetPkh txInfo ->
      unTermCont $ do
        tx <- pmatchC txInfo
        outputs <- pletC $ pfromData $ ptxInfo'outputs tx
        pure $
          pmapDropNothing
            # plam
              ( \out ->
                  unTermCont $ do
                    txOut <- pmatchC $ pfromData out
                    addr <- pmatchC $ ptxOut'address txOut
                    payCred <- pmatchC $ paddress'credential addr
                    pure $ case payCred of
                      PPubKeyCredential pkh ->
                        pif
                          (pfromData pkh #== targetPkh)
                          (pcon $ PJust $ ptxOut'value txOut)
                          (pcon PNothing)
                      _ ->
                        pcon PNothing
              )
            # outputs

{- | Get the total value paid to a public key address by a pending transaction.

@since 3.6.0
-}
pvaluePaidTo :: forall (s :: S). Term s (PTxInfo :--> PPubKeyHash :--> PLedgerValue)
pvaluePaidTo =
  phoistAcyclic $
    plam $ \txInfo pkh ->
      plet (ppubKeyOutputsAt # pkh # txInfo) $ \vals ->
        pfoldl
          # plam (\x y -> x <> pfromData y)
          # pemptyLedgerValue
          # vals

{- Get the total value of inputs spent by this transaction.

@since 3.6.0
-}
pvalueSpent :: forall (s :: S). Term s (PTxInfo :--> PLedgerValue)
pvalueSpent =
  phoistAcyclic $
    plam $ \txInfo ->
      unTermCont $ do
        tx <- pmatchC txInfo
        inputs <- pletC $ pfromData $ ptxInfo'inputs tx
        pure $
          pfoldl
            # plam
              ( \acc inp ->
                  unTermCont $ do
                    txInInfo <- pmatchC $ pfromData inp
                    resolved <- pmatchC $ ptxInInfo'resolved txInInfo
                    val <- pletC $ pfromData $ ptxOut'value resolved
                    pure $ acc <> val
              )
            # pemptyLedgerValue
            # inputs

{- Get the total value of outputs produced by this transaction.

@since 3.6.0
-}
pvalueProduced :: forall (s :: S). Term s (PTxInfo :--> PLedgerValue)
pvalueProduced =
  phoistAcyclic $
    plam $ \txInfo ->
      unTermCont $ do
        tx <- pmatchC txInfo
        outputs <- pletC $ pfromData $ ptxInfo'outputs tx
        pure $
          pfoldl
            # plam
              ( \acc out ->
                  unTermCont $ do
                    txOut <- pmatchC $ pfromData out
                    val <- pletC $ pfromData $ ptxOut'value txOut
                    pure $ acc <> val
              )
            # pemptyLedgerValue
            # outputs

{- | Get the 'PCurrencySymbol' of the current minting policy script.

@since 3.6.0
-}
pownCurrencySymbol :: forall (s :: S). Term s (PScriptContext :--> PMaybe Value.PCurrencySymbol)
pownCurrencySymbol =
  phoistAcyclic $
    plam $ \scriptCtx ->
      pmatch scriptCtx $ \ctx ->
        plet (pscriptContext'scriptInfo ctx) $ \scriptInfo ->
          pmatch scriptInfo $ \case
            PMintingScript cs ->
              pcon $ PJust $ pfromData cs
            _ ->
              pcon PNothing

{- | Check if the pending transaction spends a specific transaction output
(identified by the hash of a transaction and an index into that
transactions' outputs)

@since 3.6.0
-}
pspendsOutput :: forall (s :: S). Term s (PTxInfo :--> PTxId :--> PInteger :--> PBool)
pspendsOutput =
  phoistAcyclic $
    plam $ \txInfo txHash outIdx ->
      unTermCont $ do
        tx <- pmatchC txInfo
        inputs <- pletC $ pfromData $ ptxInfo'inputs tx
        pure $
          pany
            # plam
              ( \inp ->
                  unTermCont $ do
                    txInInfo <- pmatchC $ pfromData inp
                    outRef <- pmatchC $ ptxInInfo'outRef txInInfo
                    pure $
                      (pfromData (ptxOutRef'id outRef) #== txHash)
                        #&& (pfromData (ptxOutRef'idx outRef) #== outIdx)
              )
            # inputs
