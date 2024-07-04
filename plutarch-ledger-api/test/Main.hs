{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

{-
import Data.Kind (Type)
import Plutarch.Builtin (pforgetData)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.LedgerApi.V1 qualified as PlutarchV1
import Plutarch.LedgerApi.V2 qualified as PlutarchV2
import Plutarch.LedgerApi.V3 qualified as PlutarchV3
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
import PlutusLedgerApi.V1 qualified as PlutusLA
import PlutusLedgerApi.V1.Orphans ()
import PlutusLedgerApi.V2.Orphans ()
import PlutusLedgerApi.V3 qualified as PlutusV3
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
-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)
import V1 qualified
import V2 qualified
import V3 qualified

main :: IO ()
main = do
  -- Pre-emptively avoid encoding issues
  setLocaleEncoding utf8
  defaultMain . adjustOption moreTests . testGroup "Laws" $
    [ V1.tests
    , V2.tests
    , V3.tests
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

{-
  defaultMain . adjustOption moreTests . testGroup "Laws" $
    [ testGroup
        "V1"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchV1.PAddress
            , punsafeLiftDeclLaw @PlutarchV1.PCredential
            , punsafeLiftDeclLaw @PlutarchV1.PStakingCredential
            , punsafeLiftDeclLaw @PlutarchV1.PPubKeyHash
            , -- We only care about POSIXTime intervals, so we don't test anything
              -- else
              punsafeLiftDeclLaw @PlutarchV1.PPosixTime
            , punsafeLiftDeclLaw @(PlutarchV1.PExtended PlutarchV1.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV1.PLowerBound PlutarchV1.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV1.PUpperBound PlutarchV1.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV1.PInterval PlutarchV1.PPosixTime)
            , punsafeLiftDeclLaw @PlutarchV1.PScriptHash
            , punsafeLiftDeclLaw @PlutarchV1.PDatum
            , punsafeLiftDeclLaw @PlutarchV1.PRedeemer
            , punsafeLiftDeclLaw @PlutarchV1.PDatumHash
            , punsafeLiftDeclLaw @PlutarchV1.PRedeemerHash
            , punsafeLiftDeclLaw @PlutarchV1.PCurrencySymbol
            , punsafeLiftDeclLaw @PlutarchV1.PTokenName
            , punsafeLiftDeclLaw @PlutarchV1.PLovelace
            , -- TODO: This is definitely not correct. At the very least, the
              -- generator for Value _can_ produce zeroes!
              punsafeLiftDeclLaw @(PlutarchV1.PValue PlutarchV1.Unsorted PlutarchV1.NonZero)
            , punsafeLiftDeclLaw @PlutarchV1.PDCert
            , punsafeLiftDeclLaw @PlutarchV1.PTxId
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV1.PTxInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV1.PScriptContext
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchV1.PAddress
            , pIsDataLaws @PlutarchV1.PCredential
            , pIsDataLaws @PlutarchV1.PStakingCredential
            , pIsDataLaws @PlutarchV1.PPubKeyHash
            , pIsDataLaws @PlutarchV1.PPosixTime
            , pIsDataLaws @(PlutarchV1.PExtended PlutarchV1.PPosixTime)
            , pIsDataLaws @(PlutarchV1.PLowerBound PlutarchV1.PPosixTime)
            , pIsDataLaws @(PlutarchV1.PUpperBound PlutarchV1.PPosixTime)
            , pIsDataLaws @(PlutarchV1.PInterval PlutarchV1.PPosixTime)
            , pIsDataLaws @PlutarchV1.PScriptHash
            , pIsDataLaws @PlutarchV1.PDatum
            , pIsDataLaws @PlutarchV1.PRedeemer
            , pIsDataLaws @PlutarchV1.PDatumHash
            , pIsDataLaws @PlutarchV1.PRedeemerHash
            , pIsDataLaws @PlutarchV1.PCurrencySymbol
            , pIsDataLaws @PlutarchV1.PTokenName
            , pIsDataLaws @PlutarchV1.PLovelace
            , pIsDataLaws @(PlutarchV1.PValue PlutarchV1.Unsorted PlutarchV1.NonZero)
            , pIsDataLaws @PlutarchV1.PDCert
            , pIsDataLaws @PlutarchV1.PTxId
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV1.PTxInfo
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV1.PScriptContext
            ]
        ]
    , testGroup
        "V2"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchV2.PTxId
            , punsafeLiftDeclLaw @PlutarchV2.PTxOut
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV2.PTxInfo
            , punsafeLiftDeclLaw @PlutarchV2.PTxInInfo
            , punsafeLiftDeclLaw @PlutarchV2.POutputDatum
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV2.PScriptContext
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchV2.PTxId
            , pIsDataLaws @PlutarchV2.PTxOut
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV2.PTxInfo
            , pIsDataLaws @PlutarchV2.PTxInInfo
            , pIsDataLaws @PlutarchV2.POutputDatum
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV2.PScriptContext
            ]
        , adjustOption slightlyFewerTests $
            testGroup
              "PTryFrom"
              [ ptryFromLaws @PlutarchV2.PAddress
              , ptryFromLaws @PlutarchV2.PCredential
              , ptryFromLaws @PlutarchV2.PStakingCredential
              , ptryFromLaws @PlutarchV2.PPubKeyHash
              , ptryFromLaws @PlutarchV2.PPosixTime
              , ptryFromLaws @(PlutarchV2.PExtended PlutarchV2.PPosixTime)
              , ptryFromLaws @(PlutarchV2.PLowerBound PlutarchV2.PPosixTime)
              , ptryFromLaws @(PlutarchV2.PUpperBound PlutarchV2.PPosixTime)
              , ptryFromLaws @(PlutarchV2.PInterval PlutarchV2.PPosixTime)
              , ptryFromLaws @PlutarchV2.PScriptHash
              , ptryFromLaws @PlutarchV2.PDatum
              , ptryFromLaws @PlutarchV2.PRedeemer
              , ptryFromLaws @PlutarchV2.PDatumHash
              , ptryFromLaws @PlutarchV2.PRedeemerHash
              , ptryFromLaws @PlutarchV2.PCurrencySymbol
              , ptryFromLaws @PlutarchV2.PTokenName
              , ptryFromLaws @PlutarchV2.PLovelace
              -- , ptryFromLaws @(PlutarchLA.PValue PlutarchLA.Unsorted PlutarchLA.NonZero)
              -- Need PAsData handler because Plutarch is special
              ]
        ]
    , testGroup
        "V3"
        [ testGroup
            "PUnsafeLiftDecl"
            [ punsafeLiftDeclLaw @PlutarchV3.PColdCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchV3.PHotCommitteeCredential
            , punsafeLiftDeclLaw @PlutarchV3.PDRepCredential
            , punsafeLiftDeclLaw @PlutarchV3.PDRep
            , punsafeLiftDeclLaw @PlutarchV3.PDelegatee
            , punsafeLiftDeclLaw @PlutarchV3.PTxCert
            , punsafeLiftDeclLaw @PlutarchV3.PVoter
            , punsafeLiftDeclLaw @PlutarchV3.PVote
            , punsafeLiftDeclLaw @PlutarchV3.PGovernanceActionId
            , punsafeLiftDeclLaw @PlutarchV3.PCommittee
            , punsafeLiftDeclLaw @PlutarchV3.PConstitution
            , punsafeLiftDeclLaw @PlutarchV3.PProtocolVersion
            , punsafeLiftDeclLaw @PlutarchV3.PChangedParameters
            , punsafeLiftDeclLaw @PlutarchV3.PGovernanceAction
            , punsafeLiftDeclLaw @PlutarchV3.PProposalProcedure
            , punsafeLiftDeclLaw @PlutarchV3.PScriptPurpose
            , punsafeLiftDeclLaw @PlutarchV3.PScriptInfo
            , punsafeLiftDeclLaw @PlutarchV3.PTxInInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV3.PTxInfo
            , adjustOption fewerTests $ punsafeLiftDeclLaw @PlutarchV3.PScriptContext
            , punsafeLiftDeclLaw @PlutarchV3.PTxId
            , punsafeLiftDeclLaw @PlutarchV3.PTxOutRef
            , punsafeLiftDeclLaw @PlutarchV3.PTxOut
            , punsafeLiftDeclLaw @PlutarchV3.POutputDatum
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchV3.PColdCommitteeCredential
            , pIsDataLaws @PlutarchV3.PHotCommitteeCredential
            , pIsDataLaws @PlutarchV3.PDRepCredential
            , pIsDataLaws @PlutarchV3.PDRep
            , pIsDataLaws @PlutarchV3.PDelegatee
            , pIsDataLaws @PlutarchV3.PTxCert
            , pIsDataLaws @PlutarchV3.PVoter
            , pIsDataLaws @PlutarchV3.PVote
            , pIsDataLaws @PlutarchV3.PGovernanceActionId
            , pIsDataLaws @PlutarchV3.PCommittee
            , pIsDataLaws @PlutarchV3.PConstitution
            , pIsDataLaws @PlutarchV3.PProtocolVersion
            , pIsDataLaws @PlutarchV3.PChangedParameters
            , pIsDataLaws @PlutarchV3.PGovernanceAction
            , pIsDataLaws @PlutarchV3.PProposalProcedure
            , pIsDataLaws @PlutarchV3.PScriptPurpose
            , pIsDataLaws @PlutarchV3.PScriptInfo
            , pIsDataLaws @PlutarchV3.PTxInInfo
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV3.PTxInfo
            , adjustOption fewerTests $ pIsDataLaws @PlutarchV3.PScriptContext
            , pIsDataLaws @PlutarchV3.PTxId
            , pIsDataLaws @PlutarchV3.PTxOutRef
            , pIsDataLaws @PlutarchV3.PTxOut
            , pIsDataLaws @PlutarchV3.POutputDatum
            ]
        , adjustOption slightlyFewerTests $
            testGroup
              "PTryFrom"
              [ ptryFromLaws @PlutarchV3.PColdCommitteeCredential
              , ptryFromLaws @PlutarchV3.PHotCommitteeCredential
              , ptryFromLaws @PlutarchV3.PDRepCredential
              , ptryFromLaws @PlutarchV3.PDRep
              , ptryFromLaws @PlutarchV3.PDelegatee
              , ptryFromLaws @PlutarchV3.PTxCert
              , ptryFromLaws @PlutarchV3.PVoter
              , ptryFromLaws @PlutarchV3.PVote
              , ptryFromLaws @PlutarchV3.PGovernanceActionId
              , ptryFromLaws @PlutarchV3.PCommittee
              , ptryFromLaws @PlutarchV3.PConstitution
              , ptryFromLaws @PlutarchV3.PProtocolVersion
              , ptryFromLaws @PlutarchV3.PChangedParameters
              , ptryFromLaws @PlutarchV3.PGovernanceAction
              , ptryFromLaws @PlutarchV3.PProposalProcedure
              , ptryFromLaws @PlutarchV3.PScriptPurpose
              , ptryFromLaws @PlutarchV3.PScriptInfo
              , ptryFromLaws @PlutarchV3.PTxInInfo
              , adjustOption fewerTests $ ptryFromLaws @PlutarchV3.PTxInfo
              , adjustOption fewerTests $ ptryFromLaws @PlutarchV3.PScriptContext
              , ptryFromLaws @PlutarchV3.PTxId
              , ptryFromLaws @PlutarchV3.PTxOutRef
              , ptryFromLaws @PlutarchV3.PTxOut
              , ptryFromLaws @PlutarchV3.POutputDatum
              ]
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000
    -- Currently, the TxInfo and ScriptContext generators run like treacle, so
    -- to keep the test times manageable, we cap to 250.
    -- TODO: Fix those.
    fewerTests :: QuickCheckTests -> QuickCheckTests
    fewerTests = const 250
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
  , PlutusV3.ToData (PLifted a)
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
          plift (pforgetData . pdata . pconstant $ x) === PlutusV3.toData x
    coerceProp :: TestTree
    coerceProp =
      testProperty coerceName
        . forAllShrinkShow arbitrary shrink show
        $ \(x :: PLifted a) ->
          plift (pfromData . punsafeCoerce @_ @_ @(PAsData a) . pconstant . PlutusV3.toData $ x) === x
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
-}
