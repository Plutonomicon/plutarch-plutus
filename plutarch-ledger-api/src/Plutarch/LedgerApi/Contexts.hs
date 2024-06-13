{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V3 module in plutus-ledger-api
module Plutarch.LedgerApi.Contexts (
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
  pfindTxInByTxOutRef,
  -- TODO: Add these
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

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Credential (PCredential, PStakingCredential)
import Plutarch.LedgerApi.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.Time (PPosixTime)
import Plutarch.LedgerApi.Tx (PTxId, PTxOut, PTxOutRef)
import Plutarch.LedgerApi.Utils (PMaybeData, PRationalData)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 3.1.0
newtype PColdCommitteeCredential (s :: S) = PColdCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PColdCommitteeCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PUnsafeLiftDecl PColdCommitteeCredential where
  type PLifted PColdCommitteeCredential = Plutus.ColdCommitteeCredential

-- | @since 3.1.0
deriving via
  (DerivePConstantViaNewtype Plutus.ColdCommitteeCredential PColdCommitteeCredential PCredential)
  instance
    PConstantDecl Plutus.ColdCommitteeCredential

-- | @since 3.1.0
newtype PHotCommitteeCredential (s :: S) = PHotCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PHotCommitteeCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PUnsafeLiftDecl PHotCommitteeCredential where
  type PLifted PHotCommitteeCredential = Plutus.HotCommitteeCredential

-- | @since 3.1.0
deriving via
  (DerivePConstantViaNewtype Plutus.HotCommitteeCredential PHotCommitteeCredential PCredential)
  instance
    PConstantDecl Plutus.HotCommitteeCredential

-- | @since 3.1.0
newtype PDRepCredential (s :: S) = PDRepCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDRepCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PUnsafeLiftDecl PDRepCredential where
  type PLifted PDRepCredential = Plutus.DRepCredential

-- | @since 3.1.0
deriving via
  (DerivePConstantViaNewtype Plutus.DRepCredential PDRepCredential PCredential)
  instance
    PConstantDecl Plutus.DRepCredential

-- | @since 3.1.0
data PDRep (s :: S)
  = PDRep (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PDRepAlwaysAbstain (Term s (PDataRecord '[]))
  | PDRepAlwaysNoConfidence (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDRep where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PDRep where
  type PLifted _ = Plutus.DRep

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.DRep PDRep)
  instance
    PConstantDecl Plutus.DRep

-- | @since 3.1.0
data PDelegatee (s :: S)
  = PDelegStake (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PDelegVote (Term s (PDataRecord '["_0" ':= PDRep]))
  | PDelegStakeVote (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PDRep]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDelegatee where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PDelegatee where
  type PLifted _ = Plutus.Delegatee

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.Delegatee PDelegatee)
  instance
    PConstantDecl Plutus.Delegatee

-- | @since 3.1.0
data PTxCert (s :: S)
  = PTxCertRegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PMaybeData Value.PLovelace]))
  | PTxCertUnRegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PMaybeData Value.PLovelace]))
  | PTxCertDelegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PDelegatee]))
  | PTxCertRegDeleg (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PDelegatee, "_2" ':= Value.PLovelace]))
  | PTxCertRegDRep (Term s (PDataRecord '["_0" ':= PDRepCredential, "_1" ':= Value.PLovelace]))
  | PTxCertUpdateDRep (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PTxCertUnRegDRep (Term s (PDataRecord '["_0" ':= PDRepCredential, "_1" ':= Value.PLovelace]))
  | PTxCertPoolRegister (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PPubKeyHash]))
  | PTxCertPoolRetire (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PInteger]))
  | PTxCertAuthHotCommittee (Term s (PDataRecord '["_0" ':= PColdCommitteeCredential, "_1" ':= PHotCommitteeCredential]))
  | PTxCertResignColdCommittee (Term s (PDataRecord '["_0" ':= PColdCommitteeCredential]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PTxCert where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PTxCert where
  type PLifted _ = Plutus.TxCert

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.TxCert PTxCert)
  instance
    PConstantDecl Plutus.TxCert

-- | @since 3.1.0
data PVoter (s :: S)
  = PCommitteeVoter (Term s (PDataRecord '["_0" ':= PHotCommitteeCredential]))
  | PDRepVoter (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PStakePoolVoter (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PVoter where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PVoter where
  type PLifted _ = Plutus.Voter

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.Voter PVoter)
  instance
    PConstantDecl Plutus.Voter

-- | @since 3.1.0
data PVote (s :: S)
  = PVoteYes (Term s (PDataRecord '[]))
  | PVoteNo (Term s (PDataRecord '[]))
  | PAbstain (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PVote where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PVote where
  type PLifted _ = Plutus.Vote

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.Vote PVote)
  instance
    PConstantDecl Plutus.Vote

-- | @since 3.1.0
newtype PGovernanceActionId (s :: S)
  = PGovernanceActionId (Term s (PDataRecord '["txId" ':= PTxId, "govActionIx" ':= PInteger]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PGovernanceActionId where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PGovernanceActionId where
  type PLifted PGovernanceActionId = Plutus.GovernanceActionId

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.GovernanceActionId PGovernanceActionId)
  instance
    PConstantDecl Plutus.GovernanceActionId

-- TODO: Investigate what guarantees this provides on the Map, if any

-- | @since 3.1.0
newtype PCommittee (s :: S)
  = PCommittee
      ( Term
          s
          ( PDataRecord
              '[ "members" ':= AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger
               , "quorum" ':= PRationalData
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PCommittee where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PCommittee where
  type PLifted _ = Plutus.Committee

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.Committee PCommittee)
  instance
    PConstantDecl Plutus.Committee

{- | A constitution, omitting the optional anchor.

@since 3.1.0
-}
newtype PConstitution (s :: S) = PConstitution (Term s (PMaybeData PScriptHash))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PConstitution where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PUnsafeLiftDecl PConstitution where
  type PLifted PConstitution = Plutus.Constitution

-- | @since 3.1.0
deriving via
  (DerivePConstantViaNewtype Plutus.Constitution PConstitution (PMaybeData PScriptHash))
  instance
    PConstantDecl Plutus.Constitution

-- | @since 3.1.0
newtype PProtocolVersion (s :: S)
  = PProtocolVersion (Term s (PDataRecord '["major" ':= PInteger, "minor" ':= PInteger]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PProtocolVersion where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PProtocolVersion where
  type PLifted _ = Plutus.ProtocolVersion

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.ProtocolVersion PProtocolVersion)
  instance
    PConstantDecl Plutus.ProtocolVersion

-- | @since 3.1.0
newtype PChangedParameters (s :: S)
  = PChangedParameters (Term s PData)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PChangedParameters where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.1.0
instance PUnsafeLiftDecl PChangedParameters where
  type PLifted PChangedParameters = Plutus.ChangedParameters

-- | @since 3.1.0
deriving via
  (DerivePConstantViaBuiltin Plutus.ChangedParameters PChangedParameters PData)
  instance
    PConstantDecl Plutus.ChangedParameters

-- | @since 3.1.0
data PGovernanceAction (s :: S)
  = PParameterChange (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PChangedParameters, "_2" ':= PMaybeData PScriptHash]))
  | PHardForkInitiation (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PProtocolVersion]))
  | PTreasuryWithdrawals (Term s (PDataRecord '["_0" ':= AssocMap.PMap 'AssocMap.Unsorted PCredential Value.PLovelace, "_1" ':= PMaybeData PScriptHash]))
  | PNoConfidence (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId]))
  | PUpdateCommittee
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PGovernanceActionId
               , "_1" ':= PBuiltinList PColdCommitteeCredential
               , "_2" ':= AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger
               , "_3" ':= PRationalData
               ]
          )
      )
  | PNewConstitution (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PConstitution]))
  | PInfoAction (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PGovernanceAction where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PGovernanceAction where
  type PLifted _ = Plutus.GovernanceAction

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.GovernanceAction PGovernanceAction)
  instance
    PConstantDecl Plutus.GovernanceAction

-- | @since 3.1.0
newtype PProposalProcedure (s :: S)
  = PProposalProcedure
      ( Term
          s
          ( PDataRecord
              '[ "deposit" ':= Value.PLovelace
               , "returnAddr" ':= PCredential
               , "governanceAction" ':= PGovernanceAction
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PProposalProcedure where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PProposalProcedure where
  type PLifted _ = Plutus.ProposalProcedure

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.ProposalProcedure PProposalProcedure)
  instance
    PConstantDecl Plutus.ProposalProcedure

-- | @since 2.0.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | -- | @since 3.1.0
    PRewarding (Term s (PDataRecord '["_0" ':= PCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PTxCert]))
  | -- | @since 3.1.0
    PVoting (Term s (PDataRecord '["_0" ':= PVoter]))
  | -- | @since 3.1.0
    PProposing (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PProposalProcedure]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptPurpose where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptPurpose where
  type PLifted PScriptPurpose = Plutus.ScriptPurpose

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose)
  instance
    PConstantDecl Plutus.ScriptPurpose

-- | @since 3.1.0
data PScriptInfo (s :: S)
  = PMintingScript (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpendingScript (Term s (PDataRecord '["_0" ':= PTxOutRef, "_1" ':= PMaybeData PDatum]))
  | PRewardingScript (Term s (PDataRecord '["_0" ':= PCredential]))
  | PCertifyingScript (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PTxCert]))
  | PVotingScript (Term s (PDataRecord '["_0" ':= PVoter]))
  | PProposingScript (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PProposalProcedure]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PScriptInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PScriptInfo where
  type PLifted _ = Plutus.ScriptInfo

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.ScriptInfo PScriptInfo)
  instance
    PConstantDecl Plutus.ScriptInfo

{- | An input of the pending transaction.

@since 2.0.0
-}
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxInInfo where
  type PLifted PTxInInfo = Plutus.TxInInfo

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxInInfo PTxInInfo)
  instance
    PConstantDecl Plutus.TxInInfo

-- A pending transaction. This is the view as seen by a validator script.
--
-- @since 3.1.0
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList PTxInInfo
               , "referenceInputs" ':= PBuiltinList PTxInInfo
               , "outputs" ':= PBuiltinList PTxOut
               , "fee" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NonZero -- value minted by transaction
               , "txCerts" ':= PBuiltinList PTxCert
               , "wdrl" ':= AssocMap.PMap 'AssocMap.Unsorted PStakingCredential PInteger -- Staking withdrawals
               , "validRange" ':= Interval.PInterval PPosixTime
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "redeemers" ':= AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer
               , "data" ':= AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum
               , "id" ':= PTxId -- hash of the pending transaction
               , "votes" ':= AssocMap.PMap 'AssocMap.Unsorted PVoter (AssocMap.PMap 'AssocMap.Unsorted PGovernanceActionId PVote)
               , "proposalProcedures" ':= PBuiltinList PProposalProcedure
               , "currentTreasuryAmount" ':= PMaybeData Value.PLovelace
               , "treasuryDonation" ':= PMaybeData Value.PLovelace
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxInfo where
  type PLifted _ = Plutus.TxInfo

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxInfo PTxInfo)
  instance
    PConstantDecl Plutus.TxInfo

-- | @since 3.1.0
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "redeemer" ':= PRedeemer
               , "scriptInfo" ':= PScriptInfo
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptContext where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptContext where
  type PLifted _ = Plutus.ScriptContext

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.ScriptContext PScriptContext)
  instance
    PConstantDecl Plutus.ScriptContext

{- | Find the input currently being validated.

@since 3.1.0
-}
pfindOwnInput ::
  forall (s :: S).
  Term s (PScriptContext :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $ plam $ \sc ->
  pletFields @["txInfo", "scriptInfo"] sc $ \sc' ->
    pmatch (getField @"scriptInfo" sc') $ \case
      PSpendingScript ss ->
        let outRef = pfield @"_0" # ss
            -- This binding is needed to deal with the typelevel slurry (and
            -- resulting inference mess) that is PDataRecord. Don't remove it.
            --
            -- Koz
            txI = getField @"txInfo" sc'
            inputs = pfield @"inputs" # txI
         in pfind # (matches # outRef) # inputs
      _ -> pcon PNothing
  where
    matches ::
      forall (s' :: S).
      Term
        s'
        ( PAsData PTxOutRef
            :--> PTxInInfo
            :--> PBool
        )
    matches = phoistAcyclic $ plam $ \outRef' inInfo ->
      outRef' #== (pfield @"outRef" # inInfo)

{- | Find the datum corresponding to a datum hash, if there is one.

@since 3.1.0
-}
pfindDatum ::
  forall (s :: S).
  Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $ plam $ \dh txI ->
  let infoData = pfield @"data" # txI
   in AssocMap.plookup # dh # infoData

{- | Find the hash of a datum if it's part of the pending transaction's hashes.

@since 3.1.0
-}
pfindDatumHash ::
  forall (s :: S).
  Term s (PDatum :--> PTxInfo :--> PMaybe PDatumHash)
pfindDatumHash = phoistAcyclic $ plam $ \d txI ->
  let infoData = pfield @"data" # txI
   in pmatch infoData $ \(AssocMap.PMap ell) ->
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

{- | Given a UTxO reference and a transaction ('PTxInfo'), resolve it to one of
the transaction's inputs ('PTxInInfo').

= Note

This only searches \'true\' transaction inputs, and not referenced
transaction inputs.

@since 3.1.0
-}
pfindTxInByTxOutRef ::
  forall (s :: S).
  Term s (PTxOutRef :--> PTxInfo :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $ plam $ \outref tinfo ->
  let infoInputs = pfield @"inputs" # tinfo
   in pfind # (matches # outref) # infoInputs
  where
    matches ::
      forall (s' :: S).
      Term
        s'
        ( PTxOutRef
            :--> PTxInInfo
            :--> PBool
        )
    matches = phoistAcyclic $ plam $ \needle inInfo ->
      needle #== pfield @"outRef" # inInfo