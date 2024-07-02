{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Builtin (pforgetData)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.LedgerApi qualified as PlutarchLA
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (
  PAsData,
  PData,
  PIsData,
  PTryFrom,
  S,
  pconstant,
  pdata,
  pfromData,
  plift,
  ptryFrom,
 )
import PlutusLedgerApi.V3 qualified as PlutusLA
import PlutusLedgerApi.V3.Orphans ()
import Prettyprinter (Pretty (pretty), layoutCompact)
import Prettyprinter.Render.String (renderString)
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
            [ punsafeLiftDeclLaw @PlutarchLA.PAddress
            , punsafeLiftDeclLaw @PlutarchLA.PCredential
            , punsafeLiftDeclLaw @PlutarchLA.PStakingCredential
            , punsafeLiftDeclLaw @PlutarchLA.PPubKeyHash
            , -- We only care about POSIXTime intervals, so we don't test anything
              -- else
              punsafeLiftDeclLaw @PlutarchLA.PPosixTime
            , punsafeLiftDeclLaw @(PlutarchLA.PExtended PlutarchLA.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PLowerBound PlutarchLA.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PUpperBound PlutarchLA.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchLA.PInterval PlutarchLA.PPosixTime)
            , punsafeLiftDeclLaw @PlutarchLA.PScriptHash
            , punsafeLiftDeclLaw @PlutarchLA.PDatum
            , punsafeLiftDeclLaw @PlutarchLA.PRedeemer
            , punsafeLiftDeclLaw @PlutarchLA.PDatumHash
            , punsafeLiftDeclLaw @PlutarchLA.PRedeemerHash
            , punsafeLiftDeclLaw @PlutarchLA.PCurrencySymbol
            , punsafeLiftDeclLaw @PlutarchLA.PTokenName
            , punsafeLiftDeclLaw @PlutarchLA.PLovelace
            , -- TODO: This is definitely not correct. At the very least, the
              -- generator for Value _can_ produce zeroes!
              punsafeLiftDeclLaw @(PlutarchLA.PValue PlutarchLA.Unsorted PlutarchLA.NonZero)
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchLA.PAddress
            , pIsDataLaws @PlutarchLA.PCredential
            , pIsDataLaws @PlutarchLA.PStakingCredential
            , pIsDataLaws @PlutarchLA.PPubKeyHash
            , pIsDataLaws @PlutarchLA.PPosixTime
            , pIsDataLaws @(PlutarchLA.PExtended PlutarchLA.PPosixTime)
            , pIsDataLaws @(PlutarchLA.PLowerBound PlutarchLA.PPosixTime)
            , pIsDataLaws @(PlutarchLA.PUpperBound PlutarchLA.PPosixTime)
            , pIsDataLaws @(PlutarchLA.PInterval PlutarchLA.PPosixTime)
            , pIsDataLaws @PlutarchLA.PScriptHash
            , pIsDataLaws @PlutarchLA.PDatum
            , pIsDataLaws @PlutarchLA.PRedeemer
            , pIsDataLaws @PlutarchLA.PDatumHash
            , pIsDataLaws @PlutarchLA.PRedeemerHash
            , pIsDataLaws @PlutarchLA.PCurrencySymbol
            , pIsDataLaws @PlutarchLA.PTokenName
            , pIsDataLaws @PlutarchLA.PLovelace
            , pIsDataLaws @(PlutarchLA.PValue PlutarchLA.Unsorted PlutarchLA.NonZero)
            ]
        , adjustOption slightlyFewerTests $
            testGroup
              "PTryFrom"
              [ ptryFromLaws @PlutarchLA.PAddress
              , ptryFromLaws @PlutarchLA.PCredential
              , ptryFromLaws @PlutarchLA.PStakingCredential
              , ptryFromLaws @PlutarchLA.PPubKeyHash
              , ptryFromLaws @PlutarchLA.PPosixTime
              , ptryFromLaws @(PlutarchLA.PExtended PlutarchLA.PPosixTime)
              , ptryFromLaws @(PlutarchLA.PLowerBound PlutarchLA.PPosixTime)
              , ptryFromLaws @(PlutarchLA.PUpperBound PlutarchLA.PPosixTime)
              , ptryFromLaws @(PlutarchLA.PInterval PlutarchLA.PPosixTime)
              , ptryFromLaws @PlutarchLA.PScriptHash
              , ptryFromLaws @PlutarchLA.PDatum
              , ptryFromLaws @PlutarchLA.PRedeemer
              , ptryFromLaws @PlutarchLA.PDatumHash
              , ptryFromLaws @PlutarchLA.PRedeemerHash
              , ptryFromLaws @PlutarchLA.PCurrencySymbol
              , ptryFromLaws @PlutarchLA.PTokenName
              , ptryFromLaws @PlutarchLA.PLovelace
              -- , ptryFromLaws @(PlutarchLA.PValue PlutarchLA.Unsorted PlutarchLA.NonZero)
              -- Need PAsData handler because Plutarch is special
              ]
        ]
    , testGroup
        "V3"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchLA.PColdCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchLA.PHotCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchLA.PDRepCredential
            , punsafeLiftDeclLaw @PlutarchLA.PDRep
            , punsafeLiftDeclLaw @PlutarchLA.PDelegatee
            , punsafeLiftDeclLaw @PlutarchLA.PTxCert
            , punsafeLiftDeclLaw @PlutarchLA.PVoter
            , punsafeLiftDeclLaw @PlutarchLA.PVote
            , punsafeLiftDeclLaw @PlutarchLA.PGovernanceActionId
            , punsafeLiftDeclLaw @PlutarchLA.PCommittee
            , punsafeLiftDeclLaw @PlutarchLA.PConstitution
            , punsafeLiftDeclLaw @PlutarchLA.PProtocolVersion
            , punsafeLiftDeclLaw @PlutarchLA.PChangedParameters
            , punsafeLiftDeclLaw @PlutarchLA.PGovernanceAction
            , punsafeLiftDeclLaw @PlutarchLA.PProposalProcedure
            , punsafeLiftDeclLaw @PlutarchLA.PScriptPurpose
            , punsafeLiftDeclLaw @PlutarchLA.PScriptInfo
            , punsafeLiftDeclLaw @PlutarchLA.PTxInInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchLA.PTxInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchLA.PScriptContext
            , punsafeLiftDeclLaw @PlutarchLA.PTxId
            , punsafeLiftDeclLaw @PlutarchLA.PTxOutRef
            , punsafeLiftDeclLaw @PlutarchLA.PTxOut
            , punsafeLiftDeclLaw @PlutarchLA.POutputDatum
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchLA.PColdCommitteeCredential
            , pIsDataLaws @PlutarchLA.PHotCommitteeCredential
            , pIsDataLaws @PlutarchLA.PDRepCredential
            , pIsDataLaws @PlutarchLA.PDRep
            , pIsDataLaws @PlutarchLA.PDelegatee
            , pIsDataLaws @PlutarchLA.PTxCert
            , pIsDataLaws @PlutarchLA.PVoter
            , pIsDataLaws @PlutarchLA.PVote
            , pIsDataLaws @PlutarchLA.PGovernanceActionId
            , pIsDataLaws @PlutarchLA.PCommittee
            , pIsDataLaws @PlutarchLA.PConstitution
            , pIsDataLaws @PlutarchLA.PProtocolVersion
            , pIsDataLaws @PlutarchLA.PChangedParameters
            , pIsDataLaws @PlutarchLA.PGovernanceAction
            , pIsDataLaws @PlutarchLA.PProposalProcedure
            , pIsDataLaws @PlutarchLA.PScriptPurpose
            , pIsDataLaws @PlutarchLA.PScriptInfo
            , pIsDataLaws @PlutarchLA.PTxInInfo
            , adjustOption fewerTests $ pIsDataLaws @PlutarchLA.PTxInfo
            , adjustOption fewerTests $ pIsDataLaws @PlutarchLA.PScriptContext
            , pIsDataLaws @PlutarchLA.PTxId
            , pIsDataLaws @PlutarchLA.PTxOutRef
            , pIsDataLaws @PlutarchLA.PTxOut
            , pIsDataLaws @PlutarchLA.POutputDatum
            ]
        , adjustOption slightlyFewerTests $
            testGroup
              "PTryFrom"
              [ ptryFromLaws @PlutarchLA.PColdCommitteeCredential
              , ptryFromLaws @PlutarchLA.PHotCommitteeCredential
              , ptryFromLaws @PlutarchLA.PDRepCredential
              , ptryFromLaws @PlutarchLA.PDRep
              , ptryFromLaws @PlutarchLA.PDelegatee
              , ptryFromLaws @PlutarchLA.PTxCert
              , ptryFromLaws @PlutarchLA.PVoter
              , ptryFromLaws @PlutarchLA.PVote
              , ptryFromLaws @PlutarchLA.PGovernanceActionId
              , ptryFromLaws @PlutarchLA.PCommittee
              , ptryFromLaws @PlutarchLA.PConstitution
              , ptryFromLaws @PlutarchLA.PProtocolVersion
              , ptryFromLaws @PlutarchLA.PChangedParameters
              , ptryFromLaws @PlutarchLA.PGovernanceAction
              , ptryFromLaws @PlutarchLA.PProposalProcedure
              , ptryFromLaws @PlutarchLA.PScriptPurpose
              , ptryFromLaws @PlutarchLA.PScriptInfo
              , ptryFromLaws @PlutarchLA.PTxInInfo
              , adjustOption fewerTests $ ptryFromLaws @PlutarchLA.PTxInfo
              , adjustOption fewerTests $ ptryFromLaws @PlutarchLA.PScriptContext
              , ptryFromLaws @PlutarchLA.PTxId
              , ptryFromLaws @PlutarchLA.PTxOutRef
              , ptryFromLaws @PlutarchLA.PTxOut
              , ptryFromLaws @PlutarchLA.POutputDatum
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
    -- PTryFrom tests run slow too
    slightlyFewerTests :: QuickCheckTests -> QuickCheckTests
    slightlyFewerTests = (`quot` 2)

-- Properties

-- plift . pconstant = id
punsafeLiftDeclLaw ::
  forall (a :: S -> Type).
  ( Typeable a
  , Typeable (PLifted a)
  , PUnsafeLiftDecl a
  , Eq (PLifted a)
  , Show (PLifted a)
  , Arbitrary (PLifted a)
  ) =>
  TestTree
punsafeLiftDeclLaw = testProperty propName . forAllShrinkShow arbitrary shrink show $ \(x :: PLifted a) ->
  plift (pconstant x) === x
  where
    propName :: String
    propName =
      (tyConName . typeRepTyCon $ typeRep @a)
        <> " <-> "
        <> (tyConName . typeRepTyCon $ typeRep @(PLifted a))

pIsDataLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  , PIsData a
  , Eq (PLifted a)
  , Typeable a
  , PlutusLA.ToData (PLifted a)
  ) =>
  TestTree
pIsDataLaws =
  testGroup
    groupName
    [ fromToProp
    , toDataProp
    , coerceProp
    ]
  where
    groupName :: String
    groupName = tyConName . typeRepTyCon $ typeRep @a
    fromToProp :: TestTree
    fromToProp =
      testProperty "pfromData . pdata = id"
        . forAllShrinkShow arbitrary shrink show
        $ \(x :: PLifted a) ->
          plift (pfromData . pdata . pconstant $ x) === x
    toDataProp :: TestTree
    toDataProp =
      testProperty "plift . pforgetData . pdata . pconstant = toData"
        . forAllShrinkShow arbitrary shrink show
        $ \(x :: PLifted a) ->
          plift (pforgetData . pdata . pconstant $ x) === PlutusLA.toData x
    coerceProp :: TestTree
    coerceProp =
      testProperty coerceName
        . forAllShrinkShow arbitrary shrink show
        $ \(x :: PLifted a) ->
          plift (pfromData . punsafeCoerce @_ @_ @(PAsData a) . pconstant . PlutusLA.toData $ x) === x
    coerceName :: String
    coerceName = "plift . pfromData . punsafeCoerce @(PAsData " <> groupName <> ") . pconstant . toData = id"

ptryFromLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  , Eq (PLifted a)
  , Typeable a
  , PTryFrom PData a
  , PlutusLA.ToData (PLifted a)
  , Pretty (PLifted a)
  ) =>
  TestTree
ptryFromLaws = testGroup groupName [pDataAgreementProp]
  where
    groupName :: String
    groupName = tyConName . typeRepTyCon $ typeRep @a
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: PLifted a) ->
        plift (ptryFrom @a (pconstant . PlutusLA.toData $ x) fst) === x

-- Helpers

prettyShow :: forall (a :: Type). Pretty a => a -> String
prettyShow = renderString . layoutCompact . pretty
