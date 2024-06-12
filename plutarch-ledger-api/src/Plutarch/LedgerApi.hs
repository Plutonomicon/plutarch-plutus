{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | = Note

The 'Value.PValue', 'AssocMap.PMap' and 'Interval.PInterval'-related
functionality can be found in other modules, as these clash with the
Plutarch prelude. These should be imported qualified.
-}
module Plutarch.LedgerApi (
  -- * Contexts
  PScriptContext (..),
  PTxInfo (..),
  PScriptInfo (..),
  PScriptPurpose (..),

  -- * Tx

  -- ** Types
  PTxOutRef (..),
  PTxOut (..),
  PTxId (..),
  PTxInInfo (..),
  POutputDatum (..),

  -- ** Functions
  pgetContinuingOutputs,
  pfindOwnInput,

  -- * Script

  -- ** Types
  PDatum (..),
  PDatumHash (..),
  PRedeemer (..),
  PRedeemerHash (..),
  PScriptHash (..),

  -- ** Functions
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  pparseDatum,

  -- * Value
  Value.PValue (..),
  Value.AmountGuarantees (..),
  Value.PCurrencySymbol (..),
  Value.PTokenName (..),
  Value.PLovelace (..),

  -- * Assoc map

  -- ** Types
  AssocMap.PMap (..),
  AssocMap.KeyGuarantees (..),
  AssocMap.Commutativity (..),

  -- * Address
  PCredential (..),
  PStakingCredential (..),
  PAddress (..),

  -- * Time
  PPosixTime (..),

  -- * Interval
  Interval.PInterval (..),
  Interval.PLowerBound (..),
  Interval.PUpperBound (..),
  Interval.PExtended (..),

  -- * CIP-1694
  PTxCert (..),
  PDelegatee (..),
  PDRepCredential (..),
  PColdCommitteeCredential (..),
  PHotCommitteeCredential (..),
  PDRep (..),
  PVoter (..),
  PGovernanceActionId (..),
  PVote (..),
  PProposalProcedure (..),
  PGovernanceAction (..),
  PChangedParameters (..),
  PConstitution (..),
  PCommittee (..),

  -- * Crypto

  -- ** Types
  PubKey (..),
  PPubKeyHash (..),
  pubKeyHash,

  -- * Utilities

  -- ** Types
  PMaybeData (..),
  PRationalData (..),

  -- ** Utilities
  pfromDJust,
  pisDJust,
  pmaybeData,
  pdjust,
  pdnothing,
  pmaybeToMaybeData,
  passertPDJust,
  prationalFromData,
) where

import Codec.Serialise (serialise)
import Crypto.Hash (
  Blake2b_224 (Blake2b_224),
  Blake2b_256 (Blake2b_256),
  hashWith,
 )
import Data.Bifunctor (first)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Short (fromShort)
import Data.Coerce (coerce)
import Plutarch.Builtin (pasConstr, pasMap, pforgetData)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.Script (Script (unScript))
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Unsafe qualified as Unsafe
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

-- TODO: Investigate what guarantees this provides on the Map, if any

-- | @since 3.1.0
newtype PCommittee (s :: S)
  = PCommittee
      ( Term
          s
          ( PDataRecord
              '[ "committeeMembers" ':= AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger
               , "committeeQuorum" ':= PRationalData
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

-- | @since 2.0.0
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
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
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees -- value minted by transaction
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
instance PTryFrom PData (PAsData PChangedParameters) where
  type PTryFromExcess PData (PAsData PChangedParameters) = Mret (PBuiltinList (PBuiltinPair PData PData))
  {-# INLINEABLE ptryFrom' #-}
  -- We have to verify that we really have a Map here.
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasMap # opq)
    pure (punsafeCoerce opq, ver)

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

-- | @since 2.0.0
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "datum" ':= POutputDatum
               , "referenceScript" ':= PMaybeData PScriptHash
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
instance DerivePlutusType PTxOut where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxOut where
  type PLifted PTxOut = Plutus.TxOut

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxOut PTxOut)
  instance
    PConstantDecl Plutus.TxOut

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

-- | @since 2.0.0
data POutputDatum (s :: S)
  = PNoOutputDatum (Term s (PDataRecord '[]))
  | POutputDatumHash (Term s (PDataRecord '["datumHash" ':= PDatumHash]))
  | -- | Inline datum as per
    -- [CIP-0032](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0032/README.md)
    POutputDatum (Term s (PDataRecord '["outputDatum" ':= PDatum]))
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
instance DerivePlutusType POutputDatum where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl POutputDatum where
  type PLifted POutputDatum = Plutus.OutputDatum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.OutputDatum POutputDatum)
  instance
    PConstantDecl Plutus.OutputDatum

-- | @since 2.0.0
newtype PDatum (s :: S) = PDatum (Term s PData)
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
instance DerivePlutusType PDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatum where
  type PLifted PDatum = Plutus.Datum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Datum PDatum PData)
  instance
    PConstantDecl Plutus.Datum

-- | @since 2.0.0
newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
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
instance DerivePlutusType PRedeemer where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PRedeemer where
  type PLifted PRedeemer = Plutus.Redeemer

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData)
  instance
    PConstantDecl Plutus.Redeemer

-- | @since 2.0.0
newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PDatumHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatumHash where
  type PLifted PDatumHash = Plutus.DatumHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.DatumHash PDatumHash PByteString)
  instance
    PConstantDecl Plutus.DatumHash

-- | @since 2.0.0
newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)

{- | Hash a script, appending the Plutus V2 prefix.

@since 2.0.0
-}
scriptHash :: Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x02"

-- | @since 2.0.0
data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PScriptHash]))
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PCredential where
  type PLifted PCredential = Plutus.Credential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Credential PCredential)
  instance
    PConstantDecl Plutus.Credential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PCredential)

-- | @since 2.0.0
data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataRecord '["_0" ':= PCredential]))
  | PStakingPtr
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PInteger
               , "_1" ':= PInteger
               , "_2" ':= PInteger
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
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PStakingCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PStakingCredential where
  type PLifted PStakingCredential = Plutus.StakingCredential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.StakingCredential PStakingCredential)
  instance
    PConstantDecl Plutus.StakingCredential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PStakingCredential)

-- | @since 2.0.0
newtype PPosixTime (s :: S) = PPosixTime (Term s PInteger)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PIntegral
    , -- | @since 2.0.0
      PNum
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPosixTime where
  type PLifted PPosixTime = Plutus.POSIXTime

-- | @since 2.0.0
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPosixTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPosixTime) where
  type PTryFromExcess PData (PAsData PPosixTime) = Mret PPosixTime
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s PData ->
    ((Term s (PAsData PPosixTime), Reduce (PTryFromExcess PData (PAsData PPosixTime) s)) -> Term s r) ->
    Term s r
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term s (PAsData PInteger), unwrapped :: Term s PInteger) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    tcont $ \f -> pif (0 #<= unwrapped) (f ()) (ptraceInfoError "ptryFrom(POSIXTime): must be positive")
    pure (punsafeCoerce wrapped, pcon $ PPosixTime unwrapped)

-- | @since 2.0.0
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPubKeyHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPubKeyHash where
  type PLifted PPubKeyHash = Plutus.PubKeyHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    PConstantDecl Plutus.PubKeyHash

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Mret PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PPubKeyHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash $ unwrapped)

-- | @since 2.0.0
newtype PubKey = PubKey
  { getPubKey :: Plutus.LedgerBytes
  -- ^ @since 2.0.0
  }
  deriving stock
    ( -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      Ord
    )
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
pubKeyHash :: PubKey -> Plutus.PubKeyHash
pubKeyHash = coerce hashLedgerBytes

-- | @since 2.0.0
newtype PTxId (s :: S) = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxId where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxId where
  type PLifted PTxId = Plutus.TxId

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxId PTxId)
  instance
    PConstantDecl Plutus.TxId

-- | @since 2.0.0
instance PTryFrom PData PTxId where
  type PTryFromExcess PData PTxId = Mret PByteString
  ptryFrom' opq cont = ptryFrom @(PAsData PTxId) opq (cont . first punsafeCoerce)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PTxId) where
  type PTryFromExcess PData (PAsData PTxId) = Mret PByteString
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasConstr # opq
    tcont $ \f ->
      pif (pfstBuiltin # opq' #== 0) (f ()) $ ptraceInfoError "ptryFrom(TxId): invalid constructor id"
    flds <- tcont . plet $ psndBuiltin # opq'
    let dataBs = phead # flds
    tcont $ \f ->
      pif (pnil #== ptail # flds) (f ()) $ ptraceInfoError "ptryFrom(TxId): constructor fields len > 1"
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) dataBs snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #== 32) (f ()) $ ptraceInfoError "ptryFrom(TxId): must be 32 bytes long"
    pure (punsafeCoerce opq, unwrapped)

{- | Reference to a transaction output, with an index referencing which exact
output we mean.

@since 2.0.0
-}
newtype PTxOutRef (s :: S)
  = PTxOutRef
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTxId
               , "idx" ':= PInteger
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PTryFrom PData
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxOutRef where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxOutRef where
  type PLifted PTxOutRef = Plutus.TxOutRef

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxOutRef PTxOutRef)
  instance
    PConstantDecl Plutus.TxOutRef

-- | @since 2.0.0
newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= PMaybeData PStakingCredential
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PAddress where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PAddress where
  type PLifted PAddress = Plutus.Address

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Address PAddress)
  instance
    PConstantDecl Plutus.Address

-- | @since 2.0.0
instance PTryFrom PData (PAsData PAddress)

-- | @since 2.0.0
data PMaybeData (a :: S -> Type) (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
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
instance DerivePlutusType (PMaybeData a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PMaybeData a)

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PMaybeData a))

-- | @since 2.0.0
instance PLiftData a => PUnsafeLiftDecl (PMaybeData a) where
  type PLifted (PMaybeData a) = Maybe (PLifted a)

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Maybe a) (PMaybeData (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Maybe a)

-- Have to manually write this instance because the constructor id ordering is screwed for 'Maybe'....

-- | @since 2.0.0
instance (PIsData a, POrd a) => PPartialOrd (PMaybeData a) where
  x #< y = pmaybeLT False (#<) # x # y
  x #<= y = pmaybeLT True (#<=) # x # y

-- | @since 2.0.0
instance (PIsData a, POrd a) => POrd (PMaybeData a)

-- | @since 2.0.0
newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptHash where
  type PLifted PScriptHash = Plutus.ScriptHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.ScriptHash PScriptHash PByteString)
  instance
    PConstantDecl Plutus.ScriptHash

-- | @since 2.0.0
instance PTryFrom PData (PAsData PScriptHash) where
  type PTryFromExcess PData (PAsData PScriptHash) = Mret PScriptHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PScriptHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PScriptHash $ unwrapped)

-- | @since 2.0.0
datumHash :: Plutus.Datum -> Plutus.DatumHash
datumHash = coerce . dataHash

-- | @since 2.0.0
dataHash ::
  forall (a :: Type).
  Plutus.ToData a =>
  a ->
  PlutusTx.BuiltinByteString
dataHash = hashData . Plutus.toData

-- | @since 2.0.0
redeemerHash :: Plutus.Redeemer -> Plutus.RedeemerHash
redeemerHash = coerce . dataHash

{- | Find the output txns corresponding to the input being validated.

  Takes as arguments the inputs, outputs and the spending transaction referenced
  from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
          outputs = pfield @"outputs" # (getField @"txInfo" ctx)
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pgetContinuingOutputs ::
  forall (s :: S).
  Term s (PBuiltinList PTxInInfo :--> PBuiltinList PTxOut :--> PTxOutRef :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceInfoError "can't get any continuing outputs"
  where
    matches ::
      forall (s' :: S).
      Term s' (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pfindOwnInput ::
  forall (s :: S).
  Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches ::
      forall (s' :: S).
      Term s' (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

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
  Term s (PDatumHash :--> AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum :--> PMaybe (PAsData a))
pparseDatum = phoistAcyclic $ plam $ \dh datums ->
  pmatch (AssocMap.plookup # dh # datums) $ \case
    PNothing -> pcon PNothing
    PJust datum -> pcon . PJust $ ptryFrom (pto datum) fst

{- | Extracts the element out of a 'PDJust' and throws an error if its
argument is 'PDNothing'.

@since 2.1.1
-}
pfromDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing _ -> ptraceInfoError "pfromDJust: found PDNothing"
    PDJust x -> pfromData $ pfield @"_0" # x

{- | Yield 'PTrue' if a given 'PMaybeData' is of the form @'PDJust' _@.

@since 2.1.1
-}
pisDJust ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PDJust _ -> pconstant True
    _ -> pconstant False

{- | Special version of 'pmaybe' that works with 'PMaybeData'.

@since 2.1.1
-}
pmaybeData ::
  forall (a :: PType) (b :: PType) (s :: S).
  PIsData a =>
  Term s (b :--> (a :--> b) :--> PMaybeData a :--> b)
pmaybeData = phoistAcyclic $
  plam $ \d f m -> pmatch m $
    \case
      PDJust x -> f #$ pfield @"_0" # x
      _ -> d

{- | Construct a 'PDJust' value.

@since 2.1.1
-}
pdjust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> PMaybeData a)
pdjust = phoistAcyclic $
  plam $
    \x -> pcon $ PDJust $ pdcons @"_0" # pdata x #$ pdnil

{- | Construct a 'PDNothing' value.

@since 2.1.1
-}
pdnothing ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a)
pdnothing = phoistAcyclic $ pcon $ PDNothing pdnil

{- | Construct a 'PMaybeData' given a 'PMaybe'. Could be useful if you want to
"lift" from 'PMaybe' to 'Maybe'.

@since 2.1.1
-}
pmaybeToMaybeData ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybe a :--> PMaybeData a)
pmaybeToMaybeData = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> pcon $ PDNothing pdnil
    PJust x -> pcon $ PDJust $ pdcons @"_0" # pdata x # pdnil

{- | Extract the value stored in a 'PMaybeData' container. If there's no value,
throw an error with the given message.

@since 2.1.1
-}
passertPDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PString :--> PMaybeData a :--> a)
passertPDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceInfoError emsg

{- | A Rational type that corresponds to the data encoding used by 'Plutus.Rational'.

@since 3.1.0
-}
newtype PRationalData s
  = PRationalData
      ( Term
          s
          ( PDataRecord
              '[ "numerator" ':= PInteger
               , "denominator" ':= PPositive
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
instance PPartialOrd PRationalData where
  (#<=) = liftCompareOp (#<=)
  (#<) = liftCompareOp (#<)

-- | @since 3.1.0
instance POrd PRationalData

-- | @since 3.1.0
instance DerivePlutusType PRationalData where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PRationalData where type PLifted PRationalData = Plutus.Rational

-- | @since 3.1.0
deriving via (DerivePConstantViaData Plutus.Rational PRationalData) instance PConstantDecl Plutus.Rational

-- | @since 3.1.0
instance PTryFrom PData PRationalData where
  type PTryFromExcess PData PRationalData = Flip Term PPositive
  ptryFrom' opq cont = ptryFrom @(PAsData PRationalData) opq (cont . first Unsafe.punsafeCoerce)

{- | This instance produces a verified positive denominator as the excess output.

@since 3.1.0
-}
instance PTryFrom PData (PAsData PRationalData) where
  type PTryFromExcess PData (PAsData PRationalData) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    opq' <- pletC $ pasConstr # opq
    pguardC "ptryFrom(PRationalData): invalid constructor id" $ pfstBuiltin # opq' #== 0
    flds <- pletC $ psndBuiltin # opq'
    _numr <- pletC $ ptryFrom @(PAsData PInteger) (phead # flds) snd
    ratTail <- pletC $ ptail # flds
    denm <- pletC $ ptryFrom @(PAsData PPositive) (phead # ratTail) snd
    pguardC "ptryFrom(PRationalData): constructor fields len > 2" $ ptail # ratTail #== pnil
    pure (Unsafe.punsafeCoerce opq, denm)

-- | @since 3.1.0
prationalFromData :: ClosedTerm (PRationalData :--> PRational)
prationalFromData = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    l <- pletFieldsC @'["numerator", "denominator"] x
    pure . pcon $ PRational (getField @"numerator" l) (getField @"denominator" l)

-- Helpers

liftCompareOp ::
  forall (s :: S).
  (forall (s' :: S). Term s' PInteger -> Term s' PInteger -> Term s' PBool) ->
  Term s PRationalData ->
  Term s PRationalData ->
  Term s PBool
liftCompareOp f x y = phoistAcyclic (plam go) # x # y
  where
    go ::
      forall (s' :: S).
      Term s' PRationalData ->
      Term s' PRationalData ->
      Term s' PBool
    go l r = unTermCont $ do
      l' <- pletFieldsC @'["numerator", "denominator"] l
      r' <- pletFieldsC @'["numerator", "denominator"] r
      let ln = pfromData $ getField @"numerator" l'
      let ld = pfromData $ getField @"denominator" l'
      let rn = pfromData $ getField @"numerator" r'
      let rd = pfromData $ getField @"denominator" r'
      pure $ f (ln * pto rd) (rn * pto ld)
newtype Flip f a b = Flip (f b a) deriving stock (Generic)

hashScriptWithPrefix :: ByteString -> Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash . hashBlake2b_224 $
    prefix <> (fromShort . serialiseUPLC . unScript $ scr)

hashLedgerBytes :: Plutus.LedgerBytes -> PlutusTx.BuiltinByteString
hashLedgerBytes = hashBlake2b_224 . PlutusTx.fromBuiltin . Plutus.getLedgerBytes

hashBlake2b_224 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_224 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_224

hashBlake2b_256 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_256 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_256

hashData :: Plutus.Data -> PlutusTx.BuiltinByteString
hashData = hashBlake2b_256 . toStrict . serialise

pmaybeLT ::
  forall (a :: S -> Type) (s :: S).
  Bool ->
  ( forall (s' :: S) (rec_ :: [PLabeledType]).
    rec_ ~ '["_0" ':= a] =>
    Term s' (PDataRecord rec_) ->
    Term s' (PDataRecord rec_) ->
    Term s' PBool
  ) ->
  Term s (PMaybeData a :--> PMaybeData a :--> PBool)
pmaybeLT whenBothNothing ltF = phoistAcyclic $
  plam $ \x y -> unTermCont $ do
    a <- tcont . plet $ pasConstr #$ pforgetData $ pdata x
    b <- tcont . plet $ pasConstr #$ pforgetData $ pdata y
    cid1 <- tcont . plet $ pfstBuiltin # a
    cid2 <- tcont . plet $ pfstBuiltin # b
    pure
      $ pif
        (cid1 #< cid2)
        (pconstant False)
      $ pif
        (cid1 #== cid2)
        {- Some hand optimization here: usually, the fields would be 'plet'ed here if using 'POrd' derivation
          machinery. However, in this case - there's no need for the fields for the 'Nothing' case.

          Would be nice if this could be done on the auto derivation case....
        -}
        ( pif
            (cid1 #== 0)
            (ltF (punsafeCoerce $ psndBuiltin # a) (punsafeCoerce $ psndBuiltin # b))
            -- Both are 'Nothing'. Let caller choose answer.
            $ pconstant whenBothNothing
        )
      $ pconstant True
