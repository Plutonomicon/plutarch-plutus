{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.LedgerApi qualified as PlutarchLA
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (S, pconstant, plift)
import PlutusLedgerApi.V3 qualified as PlutusLA
import PlutusLedgerApi.V3.Orphans ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  forAllShrinkShow,
  (===),
 )
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Type.Reflection (
  Typeable,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

main :: IO ()
main = do
  -- Pre-emptively avoid encoding issues
  setLocaleEncoding utf8
  defaultMain . adjustOption moreTests . testGroup "Laws" $
    [ testGroup
        "V1"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchLA.PAddress @PlutusLA.Address
            , punsafeLiftDeclLaw @PlutarchLA.PCredential @PlutusLA.Credential
            , punsafeLiftDeclLaw @PlutarchLA.PStakingCredential @PlutusLA.StakingCredential
            , punsafeLiftDeclLaw @PlutarchLA.PPubKeyHash @PlutusLA.PubKeyHash
            , -- We only care about POSIXTime intervals, so we don't test anything
              -- else
              punsafeLiftDeclLaw @PlutarchLA.PPosixTime @PlutusLA.POSIXTime
            , punsafeLiftDeclLaw @(PlutarchLA.PExtended PlutarchLA.PPosixTime)
                @(PlutusLA.Extended PlutusLA.POSIXTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PLowerBound PlutarchLA.PPosixTime)
                @(PlutusLA.LowerBound PlutusLA.POSIXTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PUpperBound PlutarchLA.PPosixTime)
                @(PlutusLA.UpperBound PlutusLA.POSIXTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PInterval PlutarchLA.PPosixTime)
                @(PlutusLA.Interval PlutusLA.POSIXTime)
            , punsafeLiftDeclLaw @PlutarchLA.PScriptHash @PlutusLA.ScriptHash
            , punsafeLiftDeclLaw @PlutarchLA.PDatum @PlutusLA.Datum
            , punsafeLiftDeclLaw @PlutarchLA.PRedeemer @PlutusLA.Redeemer
            , punsafeLiftDeclLaw @PlutarchLA.PDatumHash @PlutusLA.DatumHash
            , punsafeLiftDeclLaw @PlutarchLA.PRedeemerHash @PlutusLA.RedeemerHash
            , punsafeLiftDeclLaw @PlutarchLA.PCurrencySymbol @PlutusLA.CurrencySymbol
            , punsafeLiftDeclLaw @PlutarchLA.PTokenName @PlutusLA.TokenName
            , punsafeLiftDeclLaw @PlutarchLA.PLovelace @PlutusLA.Lovelace
            , -- TODO: This is definitely not correct. At the very least, the
              -- generator for Value _can_ produce zeroes!
              punsafeLiftDeclLaw @(PlutarchLA.PValue PlutarchLA.Unsorted PlutarchLA.NonZero)
                @PlutusLA.Value
            ]
        ]
    , testGroup
        "V3"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchLA.PColdCommitteeCredential @PlutusLA.ColdCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchLA.PHotCommitteeCredential @PlutusLA.HotCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchLA.PDRepCredential @PlutusLA.DRepCredential
            , punsafeLiftDeclLaw @PlutarchLA.PDRep @PlutusLA.DRep
            , punsafeLiftDeclLaw @PlutarchLA.PDelegatee @PlutusLA.Delegatee
            , punsafeLiftDeclLaw @PlutarchLA.PTxCert @PlutusLA.TxCert
            , punsafeLiftDeclLaw @PlutarchLA.PVoter @PlutusLA.Voter
            , punsafeLiftDeclLaw @PlutarchLA.PVote @PlutusLA.Vote
            , punsafeLiftDeclLaw @PlutarchLA.PGovernanceActionId @PlutusLA.GovernanceActionId
            , punsafeLiftDeclLaw @PlutarchLA.PCommittee @PlutusLA.Committee
            , punsafeLiftDeclLaw @PlutarchLA.PConstitution @PlutusLA.Constitution
            , punsafeLiftDeclLaw @PlutarchLA.PProtocolVersion @PlutusLA.ProtocolVersion
            , punsafeLiftDeclLaw @PlutarchLA.PChangedParameters @PlutusLA.ChangedParameters
            , punsafeLiftDeclLaw @PlutarchLA.PGovernanceAction @PlutusLA.GovernanceAction
            , punsafeLiftDeclLaw @PlutarchLA.PProposalProcedure @PlutusLA.ProposalProcedure
            , punsafeLiftDeclLaw @PlutarchLA.PScriptPurpose @PlutusLA.ScriptPurpose
            , punsafeLiftDeclLaw @PlutarchLA.PScriptInfo @PlutusLA.ScriptInfo
            , punsafeLiftDeclLaw @PlutarchLA.PTxInInfo @PlutusLA.TxInInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchLA.PTxInfo @PlutusLA.TxInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchLA.PScriptContext @PlutusLA.ScriptContext
            , punsafeLiftDeclLaw @PlutarchLA.PTxId @PlutusLA.TxId
            , punsafeLiftDeclLaw @PlutarchLA.PTxOutRef @PlutusLA.TxOutRef
            , punsafeLiftDeclLaw @PlutarchLA.PTxOut @PlutusLA.TxOut
            , punsafeLiftDeclLaw @PlutarchLA.POutputDatum @PlutusLA.OutputDatum
            ]
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000
    -- Currently, the TxInfo and ScriptContext generators run like treacle, so
    -- to keep the test times manageable, we cap to 500.
    -- TODO: Fix those.
    fewerTests :: QuickCheckTests -> QuickCheckTests
    fewerTests = const 500

-- Properties

-- plift . pconstant = id
punsafeLiftDeclLaw ::
  forall (a :: S -> Type) (b :: Type).
  ( Typeable a
  , Typeable b
  , PUnsafeLiftDecl a
  , PLifted a ~ b
  , Eq b
  , Show b
  , Arbitrary b
  ) =>
  TestTree
punsafeLiftDeclLaw = testProperty propName . forAllShrinkShow arbitrary shrink show $ \(x :: b) ->
  plift (pconstant x) === x
  where
    propName :: String
    propName =
      (tyConName . typeRepTyCon $ typeRep @a)
        <> " <-> "
        <> (tyConName . typeRepTyCon $ typeRep @b)
