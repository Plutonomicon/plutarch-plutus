{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Contexts (
  PAccountBalanceInterval (..),
  PAccountBalanceIntervals (..),
  PTxCert (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTopTxInfoSimplified (..),
  PTopTxInfo (..),
  PScriptInfo (..),
  PScriptContext (..),
) where

import Data.Bifunctor (bimap)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (CouldNotDecodeData))
import Plutarch.LedgerApi.AssocMap (PUnsortedMap)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.V1.Time (PPosixTime)
import Plutarch.LedgerApi.V3.Contexts (
  PColdCommitteeCredential,
  PDRepCredential,
  PDelegatee,
  PGovernanceActionId,
  PHotCommitteeCredential,
  PProposalProcedure,
  PVote,
  PVoter,
 )
import Plutarch.LedgerApi.V3.MintValue (PMintValue)
import Plutarch.LedgerApi.V3.Tx (PTxId, PTxOutRef)
import Plutarch.LedgerApi.V4.Address (PAccountId)
import Plutarch.LedgerApi.V4.Tx (PTxOut)
import Plutarch.LedgerApi.Value (PCurrencySymbol, PLovelace)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V4 qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusMap

-- | @since 3.8.0
data PAccountBalanceInterval (s :: S)
  = PAccountBalanceLowerBound (Term s (PAsData PLovelace))
  | PAccountBalanceUpperBound (Term s (PAsData PLovelace))
  | PAccountBalanceBothBounds (Term s (PAsData PLovelace)) (Term s (PAsData PLovelace))
  | PAccountBalanceExact (Term s (PAsData PLovelace))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PAccountBalanceInterval)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAccountBalanceInterval Plutus.AccountBalanceInterval
  instance
    PLiftable PAccountBalanceInterval

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountBalanceInterval)

-- | @since 3.8.0
newtype PAccountBalanceIntervals (s :: S)
  = PAccountBalanceIntervals (Term s (PUnsortedMap PAccountId PAccountBalanceInterval))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PAccountBalanceIntervals)
  deriving
    ( -- | @since 3.8.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PAccountBalanceIntervals (PUnsortedMap PAccountId PAccountBalanceInterval))

-- | @since 3.8.0
instance PIsData PAccountBalanceIntervals where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 3.8.0
instance PLiftable PAccountBalanceIntervals where
  type AsHaskell PAccountBalanceIntervals = Plutus.AccountBalanceIntervals
  type PlutusRepr PAccountBalanceIntervals = [(Plutus.Data, Plutus.Data)]
  haskToRepr (Plutus.AccountBalanceIntervals m) =
    fmap (bimap Plutus.toData Plutus.toData) . PlutusMap.toList $ m
  reprToHask x =
    Plutus.AccountBalanceIntervals . PlutusMap.unsafeFromList
      <$> traverse
        ( \(k, v) ->
            (,)
              <$> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) k
              <*> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) v
        )
        x
  reprToPlut = reprToPlutUni
  plutToRepr = plutToReprUni

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountBalanceIntervals)

-- | @since 3.8.0
data PTxCert (s :: S)
  = PTxCertRegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PLovelace))
  | PTxCertUnRegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PLovelace))
  | PTxCertDelegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PDelegatee))
  | PTxCertRegAccountDeleg (Term s (PAsData PAccountId)) (Term s (PAsData PDelegatee)) (Term s (PAsData PLovelace))
  | PTxCertRegDRep (Term s (PAsData PDRepCredential)) (Term s (PAsData PLovelace))
  | PTxCertUpdateDRep (Term s (PAsData PDRepCredential))
  | PTxCertUnRegDRep (Term s (PAsData PDRepCredential)) (Term s (PAsData PLovelace))
  | PTxCertPoolRegister (Term s (PAsData PPubKeyHash)) (Term s (PAsData PPubKeyHash))
  | PTxCertPoolRetire (Term s (PAsData PPubKeyHash)) (Term s (PAsData PInteger))
  | PTxCertAuthHotCommittee (Term s (PAsData PColdCommitteeCredential)) (Term s (PAsData PHotCommitteeCredential))
  | PTxCertResignColdCommittee (Term s (PAsData PColdCommitteeCredential))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxCert)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxCert Plutus.TxCert
  instance
    PLiftable PTxCert

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxCert)

-- | @since 3.8.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PAsData PScriptHash)) (Term s (PAsData PCurrencySymbol))
  | PSpending (Term s (PAsData PScriptHash)) (Term s (PAsData PTxOutRef))
  | PWithdrawing (Term s (PAsData PScriptHash)) (Term s (PAsData PCredential))
  | PCertifying (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger)) (Term s (PAsData PTxCert))
  | PVoting (Term s (PAsData PScriptHash)) (Term s (PAsData PVoter))
  | PProposing (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger)) (Term s (PAsData PProposalProcedure))
  | PGuarding (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptPurpose)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptPurpose)

-- | @since 3.8.0
data PTxInInfo (s :: S) = PTxInInfo
  { ptxInInfo'outRef :: Term s (PAsData PTxOutRef)
  , ptxInInfo'resolved :: Term s (PAsData PTxOut)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxInInfo)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxInInfo Plutus.TxInInfo
  instance
    PLiftable PTxInInfo

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxInInfo)

-- | @since 3.8.0
data PTxInfo (s :: S) = PTxInfo
  { ptxInfo'id :: Term s (PAsData PTxId)
  , ptxInfo'subTxIx :: Term s (PAsData PInteger)
  , ptxInfo'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'outputs :: Term s (PAsData (PBuiltinList (PAsData PTxOut)))
  , ptxInfo'fee :: Term s (PAsData PLovelace)
  , ptxInfo'mint :: Term s (PAsData PMintValue)
  , ptxInfo'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , ptxInfo'withdrawals :: Term s (PAsData (PUnsortedMap PAccountId PLovelace))
  , ptxInfo'directDeposits :: Term s (PAsData (PUnsortedMap PAccountId PLovelace))
  , ptxInfo'accountBalanceIntervals :: Term s (PAsData PAccountBalanceIntervals)
  , ptxInfo'validRange :: Term s (PInterval PPosixTime)
  , ptxInfo'guards :: Term s (PAsData (PBuiltinList (PAsData PCredential)))
  , ptxInfo'requiredTopLevelGuards :: Term s (PAsData (PUnsortedMap PCredential (PMaybeData PDatum)))
  , ptxInfo'redeemers :: Term s (PAsData (PUnsortedMap PScriptPurpose PRedeemer))
  , ptxInfo'data :: Term s (PAsData (PUnsortedMap PDatumHash PDatum))
  , ptxInfo'votes :: Term s (PAsData (PUnsortedMap PVoter (PUnsortedMap PGovernanceActionId PVote)))
  , ptxInfo'proposalProcedures :: Term s (PAsData (PBuiltinList (PAsData PProposalProcedure)))
  , ptxInfo'currentTreasuryAmount :: Term s (PMaybeData PLovelace)
  , ptxInfo'treasuryDonation :: Term s (PAsData PLovelace)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxInfo)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxInfo Plutus.TxInfo
  instance
    PLiftable PTxInfo

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxInfo)

-- | @since 3.8.0
data PTopTxInfoSimplified (s :: S) = PTopTxInfoSimplified
  { pttis'ids :: Term s (PAsData (PBuiltinList (PAsData PTxId)))
  , pttis'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , pttis'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , pttis'outputs :: Term s (PAsData (PBuiltinList (PAsData PTxOut)))
  , pttis'mints :: Term s (PAsData PMintValue)
  , pttis'burns :: Term s (PAsData PMintValue)
  , pttis'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , pttis'withdrawals :: Term s (PAsData (PUnsortedMap PAccountId PLovelace))
  , pttis'directDeposits :: Term s (PAsData (PUnsortedMap PAccountId PLovelace))
  , pttis'validRange :: Term s (PAsData (PInterval PPosixTime))
  , pttis'guards :: Term s (PAsData (PUnsortedMap PCredential PUnit))
  , pttis'scriptPurposes :: Term s (PAsData (PUnsortedMap PScriptPurpose PUnit))
  , pttis'data :: Term s (PAsData (PUnsortedMap PDatumHash PDatum))
  , pttis'votes :: Term s (PAsData (PUnsortedMap PVoter (PUnsortedMap PGovernanceActionId PVote)))
  , pttis'proposalProcedures :: Term s (PAsData (PBuiltinList (PAsData PProposalProcedure)))
  , pttis'currentTreasuryAmount :: Term s (PMaybeData PLovelace)
  , pttis'treasuryDonations :: Term s (PAsData PLovelace)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTopTxInfoSimplified)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTopTxInfoSimplified Plutus.TopTxInfoSimplified
  instance
    PLiftable PTopTxInfoSimplified

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTopTxInfoSimplified)

-- | @since 3.8.0
data PTopTxInfo (s :: S) = PTopTxInfo
  { topTxInfo'subTransactions :: Term s (PAsData (PBuiltinList (PAsData PTxInfo)))
  , topTxInfo'datums :: Term s (PAsData (PUnsortedMap PInteger PDatum))
  , topTxInfo'startingBalanceIntervals :: Term s (PAsData PAccountBalanceIntervals)
  , topTxInfo'simplified :: Term s (PAsData PTopTxInfoSimplified)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTopTxInfo)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTopTxInfo Plutus.TopTxInfo
  instance
    PLiftable PTopTxInfo

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTopTxInfo)

-- | @since 3.8.0
data PScriptInfo (s :: S)
  = PMintingScript (Term s (PAsData PCurrencySymbol))
  | PSpendingScript (Term s (PAsData PTxOutRef)) (Term s (PMaybeData PDatum))
  | PWithdrawingScript (Term s (PAsData PAccountId))
  | PCertifyingScript (Term s (PAsData PInteger)) (Term s (PAsData PTxCert))
  | PVotingScript (Term s (PAsData PVoter))
  | PProposingScript (Term s (PAsData PInteger)) (Term s (PAsData PProposalProcedure))
  | PGuardingScript (Term s (PAsData PInteger)) (Term s (PMaybeData PTopTxInfo))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptInfo)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PScriptInfo Plutus.ScriptInfo
  instance
    PLiftable PScriptInfo

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptInfo)

-- | @since 3.8.0
data PScriptContext (s :: S) = PScriptContext
  { pscriptContext'txInfo :: Term s (PAsData PTxInfo)
  , pscriptContext'redeemer :: Term s (PAsData PRedeemer)
  , pscriptContext'scriptInfo :: Term s (PAsData PScriptInfo)
  , pscriptContext'scriptHash :: Term s (PAsData PScriptHash)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptContext)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptContext)
