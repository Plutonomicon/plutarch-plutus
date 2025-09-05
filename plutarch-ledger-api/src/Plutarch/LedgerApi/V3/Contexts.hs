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
  -- TODO: Add these
  -- pfindOwnInput,
  pfindDatum,
  pfindDatumHash,
  -- pfindTxInByTxOutRef,
  --  pfindContinuingInputs,
  --  pgetContinuingInputs,
  --  ptxSignedBy,
  --  ppubKeyOutputsAt,
  --  pvaluePaidTo,
  --  pvalueSpent,
  --  pvalueProduced,
  --  pownCurrencySymbol,
  --  pspendsOutput
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils (PMaybeData, PRationalData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.V1.Time (PPosixTime)
import Plutarch.LedgerApi.V2.Tx (PTxOut)
import Plutarch.LedgerApi.V3.Tx (PTxId, PTxOutRef)
import Plutarch.LedgerApi.Value qualified as Value
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

-- TODO: Investigate what guarantees this provides on the Map, if any

-- | @since 3.1.0
data PCommittee (s :: S) = PCommittee
  { pcommittee'members :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger))
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
      (Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PCredential Value.PLovelace)))
      (Term s (PMaybeData PScriptHash))
  | PNoConfidence (Term s (PMaybeData PGovernanceActionId))
  | PUpdateCommittee
      (Term s (PMaybeData PGovernanceActionId))
      (Term s (PAsData (PBuiltinList (PAsData PColdCommitteeCredential))))
      (Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger)))
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
    )
    via (DeriveAsDataStruct PScriptInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptInfo Plutus.ScriptInfo
  instance
    PLiftable PScriptInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptInfo)

{- | An input of the pending transaction.

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
  , ptxInfo'mint :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.NonZero)) -- value minted by transaction
  , ptxInfo'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , ptxInfo'wdrl :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PCredential Value.PLovelace)) -- Staking withdrawals
  , ptxInfo'validRange :: Term s (Interval.PInterval PPosixTime)
  , ptxInfo'signatories :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , ptxInfo'redeemers :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer))
  , ptxInfo'data :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum))
  , ptxInfo'id :: Term s (PAsData PTxId) -- hash of the pending transaction
  , ptxInfo'votes :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted PVoter (AssocMap.PMap 'AssocMap.Unsorted PGovernanceActionId PVote)))
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
    )
    via (DeriveAsDataStruct PScriptContext)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptContext)

{- | Find the datum corresponding to a datum hash, if there is one.

@since 3.1.0
-}
pfindDatum ::
  forall (s :: S).
  Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $ plam $ \dh txI ->
  pmatch txI $ \tx ->
    AssocMap.plookup # dh # pfromData (ptxInfo'data tx)

{- | Find the hash of a datum if it's part of the pending transaction's hashes.

@since 3.1.0
-}
pfindDatumHash ::
  forall (s :: S).
  Term s (PDatum :--> PTxInfo :--> PMaybe PDatumHash)
pfindDatumHash = phoistAcyclic $ plam $ \d txI ->
  pmatch txI $ \tx ->
    pmatch (pfromData (ptxInfo'data tx)) $ \(AssocMap.PMap ell) ->
      pmatch (pfind # (matches # d) # ell) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon . PJust . pfromData $ pfstBuiltin # p
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
      needle #== pfromData (psndBuiltin # p)
