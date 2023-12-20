{-# LANGUAGE AllowAmbiguousTypes #-}

-- NOTE: This module also contains ScriptContext mocks, which should ideally
-- moved to a module of its own after cleaning up to expose a easy to reason
-- about API.
module Plutarch.ApiSpec (
  spec,
  ctx,
  validContext0,
  validOutputs0,
  invalidContext1,
  d0Dat,
  d0DatValue,
  inp,
) where

import Test.Tasty.HUnit

import Control.Monad (forM_)
import Control.Monad.Trans.Cont (cont, runCont)
import Data.Bifunctor (bimap)
import Data.String (fromString)
import GHC.Exts (IsList (fromList))
import Numeric (showHex)
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as PlutusMap
import PlutusTx.Monoid (inv)

import Plutarch.Api.V1 (
  AmountGuarantees (NoGuarantees, NonZero, Positive),
  PCredential,
  PCurrencySymbol,
  PMaybeData,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PStakingCredential,
  PTxInInfo,
  PTxInfo,
  PValue,
 )
import Plutarch.Api.V1.AssocMap (
  BothPresentHandlerCommutative_ (..),
  BothPresentHandler_ (..),
  Commutativity (..),
  KeyGuarantees (..),
  MergeHandlerCommutative_ (..),
  MergeHandler_ (..),
  OnePresentHandler_ (DropOne, HandleOne, PassOne),
  PMap (..),
  SomeMergeHandler,
  SomeMergeHandler_ (..),
 )
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Test.Property.Gen ()

import Test.Hspec
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  arbitrary,
  chooseInteger,
  elements,
  forAll,
  oneof,
  property,
  shuffle,
  withMaxSuccess,
  (===),
 )

import Data.ByteString (ByteString)
import Data.List (sort)
import Plutarch.Lift (PConstanted, PLifted, PUnsafeLiftDecl (PLifted))

newtype EnclosedTerm (p :: PType) = EnclosedTerm {getEnclosedTerm :: ClosedTerm p}

spec :: Spec
spec = do
  describe "api" $ do
    describe "ctx" $ do
      pgoldenSpec $ do
        "term" @| ctx
        "get" @\ do
          "txInfo"
            @| pfromData (getTxInfo # ctx)
            @-> \p ->
              plift p @?= info
          "mint"
            @| pforgetData (getMint #$ getTxInfo # ctx)
            @-> \p ->
              plift p @?= toData mint
          "credentials"
            @| getCredentials ctx
            @-> \p ->
              plift p @?= [toData validator]
          "sym"
            @| pfromData (getSym #$ PValue.pnormalize #$ pfromData $ getMint #$ getTxInfo # ctx)
            @-> \p -> plift p @?= sym
        "ScriptPurpose" @\ do
          "literal" @| pconstant @PScriptPurpose (Minting dummyCurrency)
          "decode"
            @| pmatch (pconstant @PScriptPurpose (Minting dummyCurrency))
            $ \case
              PMinting c -> popaque c
              _ -> perror
    describe "value" $ do
      pgoldenSpec $ do
        let pmint = PValue.pconstantPositiveSingleton (pconstant "c0") (pconstant "sometoken") 1
            pmintOtherToken = PValue.pconstantPositiveSingleton (pconstant "c0") (pconstant "othertoken") 1
            pmintOtherSymbol = PValue.pconstantPositiveSingleton (pconstant "c7") (pconstant "sometoken") 1
            pada = PValue.pconstantPositiveSingleton PValue.padaSymbol PValue.padaToken 10_000_000
            growingSymbols, symbols :: [EnclosedTerm (PValue 'Sorted 'Positive)]
            growingSymbols =
              scanl
                (\s v -> EnclosedTerm $ getEnclosedTerm s <> getEnclosedTerm v)
                (EnclosedTerm pmint)
                symbols
            symbols = EnclosedTerm . toSymbolicValue <$> [0 .. 15]
            toSymbolicValue :: Integer -> ClosedTerm (PValue 'Sorted 'Positive)
            toSymbolicValue n =
              PValue.pconstantPositiveSingleton (pconstant $ fromString $ "c" <> showHex n "") (pconstant "token") 1
        "singleton"
          @| pmint
          @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pforgetPositive p) @?= mint
        "singletonData"
          @| PValue.psingletonData
          # pdata (pconstant "c0")
          # pdata (pconstant "sometoken")
          # pdata 1
          @-> \p -> plift (PValue.pforgetSorted p) @?= mint
        "valueOf" @\ do
          "itself" @| PValue.pvalueOf @-> \v -> plift (v # pmint # pconstant "c0" # pconstant "sometoken") @?= 1
          "applied" @| PValue.pvalueOf # pmint # pconstant "c0" # pconstant "sometoken" @-> \p ->
            plift p @?= 1
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| PValue.pvalueOf
                    # getEnclosedTerm v
                    # pconstant "c7"
                    # pconstant "token"
                    @-> \p -> plift p @?= if size < 9 then 0 else 1
              )
        "leftBiasedCurrencyUnion" @\ do
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| PValue.pleftBiasedCurrencyUnion
                    # getEnclosedTerm v
                    # pmintOtherSymbol
                    @-> pshouldReallyBe
                      ( pcon $
                          PValue.PValue
                            ( AssocMap.punionResolvingCollisionsWith Commutative
                                # plam (\_ _ -> ptraceError "unexpected collision")
                                # pto (getEnclosedTerm v)
                                # (AssocMap.pdifference # pto pmintOtherSymbol # pto (getEnclosedTerm v))
                            )
                      )
              )
        "leftBiasedTokenUnion" @\ do
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| PValue.pleftBiasedTokenUnion
                    # getEnclosedTerm v
                    # pmintOtherSymbol
                    @-> \v' ->
                      passert
                        ( v'
                            #== PValue.pleftBiasedTokenUnion
                            # pmintOtherSymbol
                            # getEnclosedTerm v
                        )
              )
        "unionResolvingCollisionsWith" @\ do
          "const" @| PValue.punionResolvingCollisionsWith NonCommutative # plam const # pmint # pmint @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint
          "(+)" @\ do
            "itself" @| PValue.punionResolvingCollisionsWith Commutative # plam (+) @-> \plus ->
              plift (PValue.pforgetSorted $ PValue.pnormalize #$ plus # pmint # pmint) @?= mint <> mint
            "applied" @| PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmint # pmint @-> \p ->
              plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mint
          "tokens" @| PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmint # pmintOtherToken @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mintOtherToken
          "symbols" @| PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmint # pmintOtherSymbol @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint <> mintOtherSymbol
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| PValue.punionResolvingCollisionsWith NonCommutative
                    # plam const
                    # getEnclosedTerm v
                    # pmintOtherSymbol
                    @-> \v' ->
                      passert
                        ( v'
                            #== PValue.punionResolvingCollisionsWith NonCommutative
                            # plam const
                            # pmintOtherSymbol
                            # getEnclosedTerm v
                        )
              )
        "unionResolvingCollisionsWithData const" @\ do
          "itself"
            @| PValue.punionResolvingCollisionsWithData NonCommutative
            @-> \u ->
              plift (PValue.pforgetSorted $ PValue.pnormalize #$ u # plam const # pmint # pmint) @?= mint
          "applied" @| PValue.punionResolvingCollisionsWithData NonCommutative # plam const # pmint # pmint @-> \p ->
            plift (PValue.pforgetSorted $ PValue.pnormalize # p) @?= mint
        "inv"
          @| inv (PValue.pforgetPositive pmint :: Term _ (PValue 'Sorted 'NonZero))
          @-> \p -> plift (PValue.pforgetSorted p) @?= inv mint
        "equality" @\ do
          "itself" @| plam ((#==) @(PValue 'Sorted 'Positive)) @-> \eq -> passert (eq # pmint # pmint)
          "triviallyTrue" @| pmint #== pmint @-> passert
          "triviallyFalse" @| pmint #== pmintOtherToken @-> passertNot
          "swappedTokensTrue"
            @| pto (PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmint # pmintOtherToken)
            #== pto (PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmintOtherToken # pmint)
            @-> passert
          "swappedSymbolsTrue"
            @| pto (PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmint # pmintOtherSymbol)
            #== pto (PValue.punionResolvingCollisionsWith Commutative # plam (+) # pmintOtherSymbol # pmint)
            @-> passert
          "growing"
            @\ forM_
              (zip [1 :: Int .. length growingSymbols] growingSymbols)
              ( \(size, v) ->
                  fromString (show size)
                    @| getEnclosedTerm v
                    #== getEnclosedTerm v
                    @-> passert
              )
        "normalize" @\ do
          "identity"
            @| PValue.passertPositive
            # (PValue.pnormalize # (pmint <> pmintOtherSymbol))
            @-> \v -> passert (v #== pmint <> pmintOtherSymbol)
          "empty"
            @| PValue.pnormalize
            # (PValue.punionResolvingCollisionsWith NonCommutative # plam (-) # pmint # pmint)
            @-> \v -> passert (v #== mempty)
        "assertSorted" @\ do
          "succeeds" @| PValue.passertSorted # (pmint <> pmintOtherSymbol) @-> psucceeds
          "fails on malsorted symbols"
            @| PValue.passertSorted
            # pcon
              ( PValue.PValue $
                  pcon $
                    AssocMap.PMap $
                      pconcat # pto (pto pmintOtherSymbol) # pto (pto pmint)
              )
            @-> pfails
          "fails on zero quantities"
            @| PValue.passertSorted
            # (PValue.punionResolvingCollisionsWith NonCommutative # plam (-) # pmint # pmint)
            @-> pfails
          "fails on empty token map"
            @| PValue.passertSorted
            # pcon (PValue.PValue $ AssocMap.psingleton # pconstant "c0" # AssocMap.pempty)
            @-> pfails
        "Ada" @\ do
          "adaSymbol" @| PValue.padaSymbol @-> psucceeds
          "adaToken" @| PValue.padaToken @-> psucceeds
          "lovelaceValueOf" @| PValue.plovelaceValueOf @-> \p -> passert (p # pada #== 10_000_000)
          "isAdaOnlyValue" @\ do
            "itself" @| PValue.pisAdaOnlyValue @-> \p -> passert (p # pada)
            "true on empty" @| PValue.pisAdaOnlyValue # (mempty :: Term _ (PValue 'Sorted 'Positive)) @-> passert
            "trivially false" @| PValue.pisAdaOnlyValue # pmint @-> passertNot
            "less trivially false" @| PValue.pisAdaOnlyValue # (pmint <> pada) @-> passertNot
          "adaOnlyValue" @\ do
            "itself" @| PValue.padaOnlyValue @-> \p -> passert (p # (pada <> pmint) #== pada)
            "on empty"
              @| PValue.padaOnlyValue
              # (mempty :: Term _ (PValue 'Sorted 'Positive))
              @-> \p -> passert (p #== mempty)
            "on non-Ada" @| PValue.padaOnlyValue # pmint @-> \p -> passert (p #== mempty)
            "on Ada" @| PValue.padaOnlyValue # pada @-> \p -> passert (p #== pada)
          "noAdaValue" @\ do
            "itself" @| PValue.pnoAdaValue @-> \p -> passert (p # (pada <> pmint) #== pmint)
            "on empty"
              @| PValue.pnoAdaValue
              # (mempty :: Term _ (PValue 'Sorted 'Positive))
              @-> \p -> passert (p #== mempty)
            "on non-Ada" @| PValue.pnoAdaValue # pmint @-> \p -> passert (p #== pmint)
            "on Ada" @| PValue.pnoAdaValue # pada @-> \p -> passert (p #== mempty)
    describe "map" $ do
      pgoldenSpec $ do
        let pmap, pdmap, emptyMap, doubleMap, otherMap :: Term _ (AssocMap.PMap 'Sorted PByteString PInteger)
            pmap = AssocMap.psingleton # pconstant "key" # 42
            pmap' = AssocMap.psingleton # pconstant "key" # 23
            psumMap = AssocMap.psingleton # pconstant "key" # 65
            pdmap = AssocMap.psingletonData # pdata (pconstant "key") # pdata 42
            emptyMap = AssocMap.pempty
            doubleMap = AssocMap.psingleton # pconstant "key" # 84
            otherMap = AssocMap.psingleton # pconstant "newkey" # 6
            pmapunionResolvingCollisions = fromList [(pconstant "key", 42), (pconstant "newkey", 6)]
            mkTestMap :: forall (s :: S). [(ByteString, Integer)] -> Term s (AssocMap.PMap 'Sorted PByteString PInteger)
            mkTestMap = fromList . fmap (bimap pconstant pconstant)
        "lookup" @\ do
          "itself"
            @| AssocMap.plookup
            @-> \lookup -> passert $ lookup # pconstant "key" # pmap #== pcon (PJust 42)
          "hit"
            @| AssocMap.plookup
            # pconstant "key"
            # pmap
            @-> \result -> passert $ result #== pcon (PJust 42)
          "miss"
            @| AssocMap.plookup
            # pconstant "nokey"
            # pmap
            @-> \result -> passert $ result #== pcon PNothing
        "lookupData" @\ do
          "hit"
            @| AssocMap.plookupData
            # pdata (pconstant "key")
            # pmap
            @-> \result -> passert $ result #== pcon (PJust $ pdata 42)
          "miss"
            @| AssocMap.plookupData
            # pdata (pconstant "nokey")
            # pmap
            @-> \result -> passert $ result #== pcon PNothing
        "findWithDefault" @\ do
          "itself"
            @| AssocMap.pfindWithDefault
            @-> \find -> (find # 12 # pconstant "key" # pmap) #@?= (42 :: Term _ PInteger)
          "hit"
            @| AssocMap.pfindWithDefault
            # 12
            # pconstant "key"
            # pmap
            @-> \result -> passert $ result #== 42
          "hit2"
            @| AssocMap.pfindWithDefault
            # 12
            # pconstant "newkey"
            # (AssocMap.punionResolvingCollisionsWith NonCommutative # plam const # pmap # otherMap)
            @-> \result -> passert $ result #== 6
          "miss"
            @| AssocMap.pfindWithDefault
            # 12
            # pconstant "nokey"
            # pmap
            @-> \result -> passert $ result #== 12
        "singleton" @| pmap @-> pshouldReallyBe pdmap
        "singletonData" @| pdmap @-> pshouldReallyBe pmap
        "insert" @\ do
          "empty" @| AssocMap.pinsert # pconstant "key" # 42 # emptyMap @-> pshouldReallyBe pmap
          "replace" @| AssocMap.pinsert # pconstant "key" # 84 # pmap @-> pshouldReallyBe doubleMap
        "delete" @\ do
          "empty" @| AssocMap.pdelete # pconstant "key" # emptyMap @-> pshouldReallyBe emptyMap
          "only" @| AssocMap.pdelete # pconstant "key" # pmap @-> pshouldReallyBe emptyMap
          "miss" @| AssocMap.pdelete # pconstant "nokey" # pmap @-> pshouldReallyBe pmap
          "new"
            @| AssocMap.pdelete
            # pconstant "newkey"
            # (AssocMap.pinsert # pconstant "newkey" # 6 # pmap)
            @-> pshouldReallyBe pmap
          "old"
            @| AssocMap.pdelete
            # pconstant "key"
            # (AssocMap.pinsert # pconstant "newkey" # 6 # pmap)
            @-> pshouldReallyBe otherMap
        "difference" @\ do
          "emptyLeft" @| AssocMap.pdifference # emptyMap # pmap @-> pshouldReallyBe emptyMap
          "emptyRight" @| AssocMap.pdifference # pmap # emptyMap @-> pshouldReallyBe pmap
          "emptyResult" @| AssocMap.pdifference # pmap # doubleMap @-> pshouldReallyBe emptyMap
          "partialOverlap"
            @| AssocMap.pdifference
            # mkTestMap [("a", 42), ("b", 23)]
            # mkTestMap [("b", 10), ("c", 8)]
            @-> pshouldReallyBe (mkTestMap [("a", 42)])
        "zipMapsWith" @\ do
          "(-)"
            @| AssocMap.pzipWithDefaults 1 0
            # plam (-)
            # mkTestMap [("a", 42), ("b", 23)]
            # mkTestMap [("b", 10), ("c", 8)]
            @-> pshouldReallyBe (mkTestMap [("a", 42), ("b", 13), ("c", -7)])
        "leftBiasedUnion" @\ do
          "const"
            @| AssocMap.pleftBiasedUnion
            # mkTestMap [("a", 42), ("b", 6)]
            # mkTestMap [("b", 7), ("c", 23)]
            @-> pshouldReallyBe (mkTestMap [("a", 42), ("b", 6), ("c", 23)])
        "unionResolvingCollisionsWith" @\ do
          "const"
            @| AssocMap.punionResolvingCollisionsWith NonCommutative
            # plam const
            # mkTestMap [("a", 42), ("b", 6)]
            # mkTestMap [("b", 7), ("c", 23)]
            @-> pshouldReallyBe (mkTestMap [("a", 42), ("b", 6), ("c", 23)])
          "flip const"
            @| AssocMap.punionResolvingCollisionsWith NonCommutative
            # plam (const id)
            # mkTestMap [("a", 42), ("b", 6)]
            # mkTestMap [("b", 7), ("c", 23)]
            @-> pshouldReallyBe (mkTestMap [("a", 42), ("b", 7), ("c", 23)])
          "double"
            @| AssocMap.punionResolvingCollisionsWith Commutative
            # plam (+)
            # pmap
            # pmap
            @-> pshouldReallyBe doubleMap
          "(+)"
            @| AssocMap.punionResolvingCollisionsWith Commutative
            # plam (+)
            # pmap
            # pmap'
            @-> pshouldReallyBe psumMap
          "preservesCombineCommutativity"
            @| AssocMap.punionResolvingCollisionsWith Commutative
            # plam (+)
            # pmap'
            # pmap
            @-> \p -> passert (p #== AssocMap.punionResolvingCollisionsWith Commutative # plam (+) # pmap # pmap')
        "unionResolvingCollisionsWithData" @\ do
          "const"
            @| AssocMap.punionResolvingCollisionsWithData NonCommutative
            # plam const
            # pmap
            # pmap
            @-> pshouldReallyBe pmap
          "emptyLeft"
            @| AssocMap.punionResolvingCollisionsWithData NonCommutative
            # plam const
            # emptyMap
            # pmap
            @-> pshouldReallyBe pmap
          "emptyRight"
            @| AssocMap.punionResolvingCollisionsWithData NonCommutative
            # plam const
            # pmap
            # emptyMap
            @-> pshouldReallyBe pmap
          "distinctKeys"
            @| AssocMap.punionResolvingCollisionsWithData NonCommutative
            # plam const
            # pmap
            # otherMap
            @-> pshouldReallyBe pmapunionResolvingCollisions
        "intersectionWith" @\ do
          "const"
            @| AssocMap.pintersectionWith NonCommutative
            # plam const
            # mkTestMap [("a", 42), ("b", 6)]
            # mkTestMap [("b", 7), ("c", 23)]
            @-> pshouldReallyBe (mkTestMap [("b", 6)])
          "flip const"
            @| AssocMap.pintersectionWith NonCommutative
            # plam (const id)
            # mkTestMap [("a", 42), ("b", 6)]
            # mkTestMap [("b", 7), ("c", 23)]
            @-> pshouldReallyBe (mkTestMap [("b", 7)])
          "double" @| AssocMap.pintersectionWith Commutative # plam (+) # pmap # pmap @-> pshouldReallyBe doubleMap
          "(+)"
            @| AssocMap.pintersectionWith Commutative
            # plam (+)
            # pmap
            # pmap'
            @-> pshouldReallyBe psumMap
          "preservesCombineCommutativity"
            @| AssocMap.pintersectionWith Commutative
            # plam (+)
            # pmap'
            # pmap
            @-> pshouldReallyBe (AssocMap.pintersectionWith Commutative # plam (+) # pmap # pmap')
          "partialKeyMismatch"
            @| AssocMap.pintersectionWith Commutative
            # plam (+)
            # (AssocMap.punionResolvingCollisionsWithData NonCommutative # plam const # pmap # otherMap)
            # pmap'
            @-> pshouldReallyBe psumMap
        "intersectionWithData" @\ do
          "const"
            @| AssocMap.pintersectionWithData NonCommutative
            # plam const
            # pmap
            # pmap
            @-> pshouldReallyBe pmap
          "emptyLeft"
            @| AssocMap.pintersectionWithData NonCommutative
            # plam const
            # emptyMap
            # pmap
            @-> pshouldReallyBe emptyMap
          "emptyRight"
            @| AssocMap.pintersectionWithData NonCommutative
            # plam const
            # pmap
            # emptyMap
            @-> pshouldReallyBe emptyMap
          "keyMismatch"
            @| AssocMap.pintersectionWithData NonCommutative
            # plam const
            # pmap
            # otherMap
            @-> pshouldReallyBe emptyMap
    describe "example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      -- See Plutarch.MonadicSpec for GHC9 only syntax.
      describe "signatory" . pgoldenSpec $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        "cont" @\ do
          "succeeds" @| checkSignatoryCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryCont # pconstant "41" # ctx @-> pfails
        "termcont" @\ do
          "succeeds" @| checkSignatoryTermCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryTermCont # pconstant "41" # ctx @-> pfails
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields
      describe "data recovery" $ do
        describe "succeeding property tests" $ do
          it "recovering PAddress succeeds" $
            property (propPlutarchtypeCanBeRecovered @Address)
          it "recovering PTokenName succeeds" $
            property (propPlutarchtypeCanBeRecovered @TokenName)
          it "recovering PCredential succeeds" $
            property (propPlutarchtypeCanBeRecovered @Credential)
          it "recovering PStakingCredential succeeds" $
            property (propPlutarchtypeCanBeRecovered @StakingCredential)
          it "recovering PPubKeyHash succeeds" $
            property (propPlutarchtypeCanBeRecovered @PubKeyHash)
          it "recovering PScriptHash succeeds" $
            property (propPlutarchtypeCanBeRecovered @ScriptHash)
          it "recovering PValue succeeds" $
            property (propPlutarchtypeCanBeRecovered @Value)
          it "recovering PCurrencySymbol succeeds" $
            property (propPlutarchtypeCanBeRecovered @CurrencySymbol)
          it "recovering PMaybeData succeeds" $
            property prop_pmaybedata_can_be_recovered
    describe "AssocMap.pzipWith" $ do
      it "matches independently constructed expectation" $
        withMaxSuccess 1000 $
          forAll genSomeMergeHandler $
            \mh -> forAll genSets . prop_pzipWith $ mh

--------------------------------------------------------------------------------

{- |
  An example 'PScriptContext' Term,
  lifted with 'pconstant'
-}
ctx :: Term s PScriptContext
ctx =
  pconstant
    (ScriptContext info purpose)

-- | Simple script context, with minting and a single input
info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = "b0"
    }

-- | A script input
inp :: TxInInfo
inp =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datum
          }
    }

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

mintOtherToken :: Value
mintOtherToken = Value.singleton sym "othertoken" 1

mintOtherSymbol :: Value
mintOtherSymbol = Value.singleton "c7" "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ScriptHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

--------------------------------------------------------------------------------

getTxInfo :: Term s (PScriptContext :--> PAsData PTxInfo)
getTxInfo =
  plam $ \ctx ->
    pfield @"txInfo" # ctx

getMint :: Term s (PAsData PTxInfo :--> PAsData (PValue 'Sorted 'NoGuarantees))
getMint =
  plam $ \info ->
    pfield @"mint" # info

-- | Get validator from first input in ScriptContext's TxInfo
getCredentials :: Term s PScriptContext -> Term s (PBuiltinList PData)
getCredentials ctx =
  let inp = pfield @"inputs" #$ pfield @"txInfo" # ctx
   in pmap # inputCredentialHash # inp

{- |
  Get the hash of the Credential in an input, treating
  PubKey & ValidatorHash identically.
-}
inputCredentialHash :: Term s (PTxInInfo :--> PData)
inputCredentialHash =
  phoistAcyclic $
    plam $ \inp ->
      let credential :: Term _ (PAsData PCredential)
          credential =
            (pfield @"credential")
              #$ (pfield @"address")
              #$ (pfield @"resolved" # inp)
       in phead #$ psndBuiltin #$ pasConstr # pforgetData credential

-- | Get first CurrencySymbol from Value
getSym :: Term s (PValue 'Sorted 'NonZero :--> PAsData PCurrencySymbol)
getSym =
  plam $ \v -> pfstBuiltin #$ phead # pto (pto v)

-- | `checkSignatory` implemented using `runCont`
checkSignatoryCont :: forall s. Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryCont = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> (`runCont` id) $ do
    purpose <- cont (pmatch $ getField @"purpose" ctx)
    pure $ case purpose of
      PSpending _ ->
        let signatories :: Term s (PBuiltinList (PAsData PPubKeyHash))
            signatories = pfield @"signatories" # getField @"txInfo" ctx
         in pif
              (pelem # pdata ph # signatories)
              -- Success!
              (pconstant ())
              -- Signature not present.
              perror
      _ ->
        ptraceError "checkSignatoryCont: not a spending tx"

-- | `checkSignatory` implemented using `runTermCont`
checkSignatoryTermCont :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryTermCont = plam $ \ph ctx' -> unTermCont $ do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  tcont (pmatch $ getField @"purpose" ctx) >>= \case
    PSpending _ -> do
      let signatories = pfield @"signatories" # getField @"txInfo" ctx
      pure $
        pif
          (pelem # pdata ph # pfromData signatories)
          -- Success!
          (pconstant ())
          -- Signature not present.
          perror
    _ ->
      pure $ ptraceError "checkSignatoryCont: not a spending tx"

getFields :: Term s (PData :--> PBuiltinList PData)
getFields = phoistAcyclic $ plam $ \addr -> psndBuiltin #$ pasConstr # addr

dummyCurrency :: CurrencySymbol
dummyCurrency = Value.currencySymbol "\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"

------------------- Mocking a ScriptContext ----------------------------------------

validContext0 :: Term s PScriptContext
validContext0 = mkCtx validOutputs0 validDatums1

invalidContext1 :: Term s PScriptContext
invalidContext1 = mkCtx invalidOutputs1 validDatums1

mkCtx :: [TxOut] -> [(DatumHash, Datum)] -> Term s PScriptContext
mkCtx outs l = pconstant (ScriptContext (info' outs l) purpose)
  where
    info' :: [TxOut] -> [(DatumHash, Datum)] -> TxInfo
    info' outs dat =
      info
        { txInfoData = dat
        , txInfoOutputs = outs
        }

validOutputs0 :: [TxOut]
validOutputs0 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  ]

invalidOutputs1 :: [TxOut]
invalidOutputs1 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  , TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Nothing
      }
  ]

validDatums1 :: [(DatumHash, Datum)]
validDatums1 =
  [("d0", d0Dat)]

-- | Mock datum that is a list of integers.
d0Dat :: Datum
d0Dat = Datum $ toBuiltinData d0DatValue

d0DatValue :: [Integer]
d0DatValue = [1 .. 10]

------------------- Property tests -------------------------------------------------

propPlutarchtypeCanBeRecovered ::
  forall a.
  ( Eq a
  , Show a
  , PConstant a
  , PIsData (PConstanted a)
  , PTryFrom PData (PAsData (PConstanted a))
  , PLifted (PConstanted a) ~ a
  , ToData a
  ) =>
  a ->
  Property
propPlutarchtypeCanBeRecovered addr =
  addr
    === plift
      ( unTermCont $
          pfromData . fst <$> tcont (ptryFrom @(PAsData (PConstanted a)) $ pforgetData $ pconstantData addr)
      )

prop_pmaybedata_can_be_recovered :: Maybe StakingCredential -> Property
prop_pmaybedata_can_be_recovered addr =
  addr
    === plift
      ( unTermCont $
          pfromData . fst
            <$> tcont
              (ptryFrom @(PAsData (PMaybeData PStakingCredential)) $ pforgetData $ pconstantData addr)
      )

pshouldReallyBe :: ClosedTerm a -> ClosedTerm a -> Expectation
pshouldReallyBe a b = pshouldBe b a

---------- AssocMap pzipWith property test infrastructure --------

prop_pzipWith :: SomeMergeHandler_ DummyFun DummyKeyType DummyValueType -> RelatedSets -> Property
prop_pzipWith mh sets@RelatedSets {left, right} =
  expectationZipWith mh sets
    === pMapToKVs
      ( AssocMap.pzipWith (unDummifySomeMergeHandler mh)
          # keysToPMap True left
          # keysToPMap False right
      )

data RelatedSets = RelatedSets
  { leftOnly :: [Integer]
  -- ^ left \ right
  , rightOnly :: [Integer]
  -- ^ right \ left
  , intersection :: [Integer]
  -- ^ intersection of left and right
  , left :: [Integer]
  , right :: [Integer]
  }
  deriving stock (Show)

genSets :: Gen RelatedSets
genSets = do
  -- ensuring case coverage by picking sections of Venn diagram
  haveLeftOnly <- arbitrary @Bool
  haveRightOnly <- arbitrary @Bool
  haveIntersection <- arbitrary @Bool

  let mkCount pred = if pred then chooseInteger (1, 10) else pure 0
  leftOnlyCount <- mkCount haveLeftOnly
  rightOnlyCount <- mkCount haveRightOnly
  intersectionCount <- mkCount haveIntersection
  holeCount <- chooseInteger (0, 10)
  let distinctCount = leftOnlyCount + rightOnlyCount + intersectionCount + holeCount

  let sorted = [1 .. distinctCount]
  unsorted <- shuffle sorted

  let (leftOnly, unsorted') = splitAt (fromIntegral leftOnlyCount) unsorted
      (intersection, unsorted'') = splitAt (fromIntegral intersectionCount) unsorted'
      (rightOnly, _) = splitAt (fromIntegral rightOnlyCount) unsorted''
      left = leftOnly <> intersection
      right = rightOnly <> intersection

  pure RelatedSets {leftOnly, rightOnly, intersection, left, right}

-- carefully chosen to yield unique results with the ops/factors below
leftDummyVal :: Integer
leftDummyVal = 2

-- carefully chosen to yield unique results with the ops/factors below
rightDummyVal :: Integer
rightDummyVal = 3

-- | True ~ left, False ~ right
dummyVal :: Bool -> Integer
dummyVal side = if side then leftDummyVal else rightDummyVal

mhOneFactorLeft :: Integer
mhOneFactorLeft = 10

mhOneFactorRight :: Integer
mhOneFactorRight = 100

mhcOneFactor :: Integer
mhcOneFactor = 1000

commutativeOp :: Num a => a -> a -> a
commutativeOp = (+)

nonCommutativeOp :: Num a => a -> a -> a
nonCommutativeOp = (-)

data DummyKeyType
data DummyValueType
data DummyFun a b = DummyFun deriving stock (Show)

genOnePresentHandler :: Gen (OnePresentHandler_ DummyFun DummyKeyType DummyValueType)
genOnePresentHandler =
  elements
    [ DropOne
    , PassOne
    , HandleOne DummyFun
    ]

genBothPresentHandler :: Gen (BothPresentHandler_ DummyFun DummyKeyType DummyValueType)
genBothPresentHandler = do
  side <- elements [True, False]
  elements
    [ DropBoth
    , PassArg side
    , HandleBoth DummyFun
    ]

genBothPresentHandlerCommutative ::
  Gen (BothPresentHandlerCommutative_ DummyFun DummyKeyType DummyValueType)
genBothPresentHandlerCommutative = do
  elements
    [ DropBothCommutative
    , HandleBothCommutative DummyFun
    ]

genMergeHandler :: Gen (MergeHandler_ DummyFun DummyKeyType DummyValueType)
genMergeHandler =
  MergeHandler <$> genBothPresentHandler <*> genOnePresentHandler <*> genOnePresentHandler

genMergeHandlerCommutative :: Gen (MergeHandlerCommutative_ DummyFun DummyKeyType DummyValueType)
genMergeHandlerCommutative =
  MergeHandlerCommutative <$> genBothPresentHandlerCommutative <*> genOnePresentHandler

genSomeMergeHandler :: Gen (SomeMergeHandler_ DummyFun DummyKeyType DummyValueType)
genSomeMergeHandler =
  oneof
    [ SomeMergeHandler <$> genMergeHandler
    , SomeMergeHandlerCommutative <$> genMergeHandlerCommutative
    ]

unDummifySomeMergeHandler ::
  forall (s :: S).
  SomeMergeHandler_ DummyFun DummyKeyType DummyValueType ->
  SomeMergeHandler PInteger PInteger s
unDummifySomeMergeHandler = \case
  SomeMergeHandler mh -> SomeMergeHandler $ unMH mh
  SomeMergeHandlerCommutative mh -> SomeMergeHandlerCommutative $ unMHC mh
  where
    unMH MergeHandler {mhBoth, mhLeft, mhRight} =
      MergeHandler
        { mhBoth = unBoth mhBoth
        , mhLeft =
            unOne mhOneFactorLeft mhLeft
        , mhRight = unOne mhOneFactorRight mhRight
        }
    unMHC MergeHandlerCommutative {mhcBoth, mhcOne} =
      MergeHandlerCommutative
        { mhcBoth = unBothC mhcBoth
        , mhcOne = unOne mhcOneFactor mhcOne
        }
    unBoth = \case
      DropBoth -> DropBoth
      PassArg arg -> PassArg arg
      HandleBoth DummyFun -> HandleBoth (const nonCommutativeOp)
    unBothC = \case
      DropBothCommutative -> DropBothCommutative
      HandleBothCommutative DummyFun -> HandleBothCommutative (const commutativeOp)
    unOne factor = \case
      DropOne -> DropOne
      PassOne -> PassOne
      HandleOne DummyFun -> HandleOne (const (* pconstant factor))

keysToPMap ::
  forall (s :: S).
  Bool ->
  [Integer] ->
  Term s (PMap 'Sorted PInteger PInteger)
keysToPMap side keys =
  fromList $
    fmap (\k -> (pconstant k, pconstant $ dummyVal side)) keys

pMapToKVs :: ClosedTerm (PMap 'Sorted PInteger PInteger) -> [(Integer, Integer)]
pMapToKVs pm = PlutusMap.toList $ plift $ AssocMap.pforgetSorted pm

expectationZipWith ::
  SomeMergeHandler_ DummyFun DummyKeyType DummyValueType -> RelatedSets -> [(Integer, Integer)]
expectationZipWith
  (SomeMergeHandler MergeHandler {mhBoth, mhLeft, mhRight})
  RelatedSets {leftOnly, rightOnly, intersection} =
    let b = case mhBoth of
          DropBoth -> []
          PassArg side -> fmap (,dummyVal side) intersection
          HandleBoth DummyFun ->
            fmap (,leftDummyVal `nonCommutativeOp` rightDummyVal) intersection
        l = case mhLeft of
          DropOne -> []
          PassOne -> fmap (,leftDummyVal) leftOnly
          HandleOne DummyFun -> fmap (,mhOneFactorLeft * leftDummyVal) leftOnly
        r = case mhRight of
          DropOne -> []
          PassOne -> fmap (,rightDummyVal) rightOnly
          HandleOne DummyFun -> fmap (,mhOneFactorRight * rightDummyVal) rightOnly
     in sort (b <> l <> r)
expectationZipWith
  (SomeMergeHandlerCommutative MergeHandlerCommutative {mhcBoth, mhcOne})
  RelatedSets {leftOnly, rightOnly, intersection} =
    let b = case mhcBoth of
          DropBothCommutative -> []
          HandleBothCommutative _ ->
            fmap (,leftDummyVal `commutativeOp` rightDummyVal) intersection
        o = case mhcOne of
          DropOne -> []
          PassOne ->
            fmap (,leftDummyVal) leftOnly <> fmap (,rightDummyVal) rightOnly
          HandleOne DummyFun ->
            fmap (,mhcOneFactor * leftDummyVal) leftOnly
              <> fmap (,mhcOneFactor * rightDummyVal) rightOnly
     in sort (b <> o)
