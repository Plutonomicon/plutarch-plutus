{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Builtin (pforgetData)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.LedgerApi.V3 qualified as PlutarchV3
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (
  PAsData,
  PIsData,
  S,
  pconstant,
  pdata,
  pfromData,
  plift,
 )
import PlutusLedgerApi.V3 qualified as PlutusV3
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
            [ punsafeLiftDeclLaw @PlutarchV3.PAddress
            , punsafeLiftDeclLaw @PlutarchV3.PCredential
            , punsafeLiftDeclLaw @PlutarchV3.PStakingCredential
            , punsafeLiftDeclLaw @PlutarchV3.PPubKeyHash
            , -- We only care about POSIXTime intervals, so we don't test anything
              -- else
              punsafeLiftDeclLaw @PlutarchV3.PPosixTime
            , punsafeLiftDeclLaw @(PlutarchV3.PExtended PlutarchV3.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV3.PLowerBound PlutarchV3.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV3.PUpperBound PlutarchV3.PPosixTime)
            , punsafeLiftDeclLaw @(PlutarchV3.PInterval PlutarchV3.PPosixTime)
            , punsafeLiftDeclLaw @PlutarchV3.PScriptHash
            , punsafeLiftDeclLaw @PlutarchV3.PDatum
            , punsafeLiftDeclLaw @PlutarchV3.PRedeemer
            , punsafeLiftDeclLaw @PlutarchV3.PDatumHash
            , punsafeLiftDeclLaw @PlutarchV3.PRedeemerHash
            , punsafeLiftDeclLaw @PlutarchV3.PCurrencySymbol
            , punsafeLiftDeclLaw @PlutarchV3.PTokenName
            , punsafeLiftDeclLaw @PlutarchV3.PLovelace
            , -- TODO: This is definitely not correct. At the very least, the
              -- generator for Value _can_ produce zeroes!
              punsafeLiftDeclLaw @(PlutarchV3.PValue PlutarchV3.Unsorted PlutarchV3.NonZero)
            ]
        , testGroup
            "PIsData"
            [ pIsDataLaws @PlutarchV3.PAddress
            , pIsDataLaws @PlutarchV3.PCredential
            , pIsDataLaws @PlutarchV3.PStakingCredential
            , pIsDataLaws @PlutarchV3.PPubKeyHash
            , pIsDataLaws @PlutarchV3.PPosixTime
            , pIsDataLaws @(PlutarchV3.PExtended PlutarchV3.PPosixTime)
            , pIsDataLaws @(PlutarchV3.PLowerBound PlutarchV3.PPosixTime)
            , pIsDataLaws @(PlutarchV3.PUpperBound PlutarchV3.PPosixTime)
            , pIsDataLaws @(PlutarchV3.PInterval PlutarchV3.PPosixTime)
            , pIsDataLaws @PlutarchV3.PScriptHash
            , pIsDataLaws @PlutarchV3.PDatum
            , pIsDataLaws @PlutarchV3.PRedeemer
            , pIsDataLaws @PlutarchV3.PDatumHash
            , pIsDataLaws @PlutarchV3.PRedeemerHash
            , pIsDataLaws @PlutarchV3.PCurrencySymbol
            , pIsDataLaws @PlutarchV3.PTokenName
            , pIsDataLaws @PlutarchV3.PLovelace
            , pIsDataLaws @(PlutarchV3.PValue PlutarchV3.Unsorted PlutarchV3.NonZero)
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
