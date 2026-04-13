module Plutarch.Test.Suite.PlutarchLedgerApi.V3.Value (
  tests,
) where

import Plutarch.Builtin.Value (PBuiltinValue)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PMintValue, PTokenName)
import Plutarch.LedgerApi.V3.Value (
  pdeleteAmount,
  pemptyBuiltinValue,
  pfromMintValue,
  plovelaceValueOf,
  preplaceAmountNegative,
  preplaceAmountPositive,
  psingletonBuiltinValue,
  ptoLedgerValue,
  ptoMintValue,
  ptoSortedValue,
  pvalueOf,
 )
import Plutarch.LedgerApi.Value (
  padaSymbol,
  padaToken,
  pforgetSorted,
  ppromoteToSortedValue,
 )
import Plutarch.Prelude (
  PAsData,
  PBool (PTrue),
  PInteger,
  S,
  Term,
  pcon,
  pconstant,
  pfromData,
  pif,
  plam,
  plet,
  plift,
  pnegate,
  pnot,
  pone,
  (#),
  (#$),
  (#==),
 )
import Plutarch.Test.Utils (precompileTerm)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Value qualified as PlutusCore
import PlutusLedgerApi.V3 qualified as PLA
import PlutusLedgerApi.V3.Orphans ()
import Test.QuickCheck (
  Positive (Positive),
  Property,
  arbitrary,
  forAllShrinkShow,
  shrink,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "PBuiltinValue"
    [ testProperty "PMintValues convert to PBuiltinValues without error" propMintToBuiltin
    , testProperty "PBuiltinValues convert to PSortedValues without error" propBuiltinToSorted
    , testProperty "pemptyBuiltinValue does not contain anything" propEmptyNoContents
    , testProperty "psingletonBuiltinValue contains the stated amount" propSingletonContents
    , testProperty "preplaceAmountPositive replaces correctly" propReplacePositive
    , testProperty "preplaceAmountNegative replaces correctly" propReplaceNegative
    , testProperty "pdeleteAmount removes the amount" propDeleteAmount
    , testProperty "plovelaceValueOf = pvalueOf padaSymbol padaTokenName" propLovelaceValueOf
    , testProperty "ptoLedgerValue works correctly" propToLedgerValue
    , testProperty "ptoMintValue works correctly" propToMintValue
    ]

-- Properties

propToMintValue :: Property
propToMintValue = forAllShrinkShow arbitrary shrink show $ \(v, coin) ->
  plift (precompileTerm (plam go) # pconstant v # pconstant coin)
  where
    go ::
      forall (s :: S).
      Term s PBuiltinValue ->
      Term s PBool ->
      Term s PBool
    go v hasAdaEntry =
      plet (pif hasAdaEntry (preplaceAmountPositive # padaSymbol # padaToken # pone # v) (pdeleteAmount # padaSymbol # padaToken # v)) $ \v' ->
        plet (ptoMintValue v' hasAdaEntry (plam $ const (pnot # hasAdaEntry))) id

propToLedgerValue :: Property
propToLedgerValue = forAllShrinkShow arbitrary shrink show $ \(v, coin) ->
  plift (precompileTerm (plam go) # pconstant v # pconstant coin)
  where
    go ::
      forall (s :: S).
      Term s PBuiltinValue ->
      Term s PBool ->
      Term s PBool
    go v hasAdaEntry =
      plet (pif hasAdaEntry (preplaceAmountPositive # padaSymbol # padaToken # pone # v) (pdeleteAmount # padaSymbol # padaToken # v)) $ \v' ->
        plet (ptoLedgerValue v' (pnot # hasAdaEntry) (plam $ const hasAdaEntry)) id

propEmptyNoContents :: Property
propEmptyNoContents = forAllShrinkShow arbitrary shrink show $ \(cs, tn) ->
  plift (precompileTerm (plam go) # pconstant cs # pconstant tn)
  where
    go ::
      forall (s :: S).
      Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PBool
    go cs tn = pvalueOf # cs # tn # pemptyBuiltinValue #== 0

propSingletonContents :: Property
propSingletonContents = forAllShrinkShow arbitrary shrink show $ \(cs, tn, i) ->
  plift (precompileTerm (plam go) # pconstant cs # pconstant tn # pconstant i)
  where
    go ::
      forall (s :: S).
      Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PInteger ->
      Term s PBool
    go cs tn i = (pvalueOf # cs # tn #$ psingletonBuiltinValue # cs # tn # i) #== i

propReplacePositive :: Property
propReplacePositive = forAllShrinkShow arbitrary shrink show $ \(cs, tn, Positive p, v) ->
  plift (precompileTerm (plam go) # pconstant cs # pconstant tn # pconstant p # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PInteger ->
      Term s PBuiltinValue ->
      Term s PBool
    go cs tn i v =
      let asPositive = punsafeCoerce i
          actual = pvalueOf # cs # tn #$ preplaceAmountPositive # cs # tn # asPositive # v
       in i #== actual

propReplaceNegative :: Property
propReplaceNegative = forAllShrinkShow arbitrary shrink show $ \(cs, tn, Positive p, v) ->
  plift (precompileTerm (plam go) # pconstant cs # pconstant tn # pconstant p # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PInteger ->
      Term s PBuiltinValue ->
      Term s PBool
    go cs tn i v =
      let asPositive = punsafeCoerce i
          actual = pvalueOf # cs # tn #$ preplaceAmountNegative # cs # tn # asPositive # v
       in (pnegate # i) #== actual

propDeleteAmount :: Property
propDeleteAmount = forAllShrinkShow arbitrary shrink show $ \(cs, tn, v) ->
  plift (precompileTerm (plam go) # pconstant cs # pconstant tn # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PBuiltinValue ->
      Term s PBool
    go cs tn v = (pvalueOf # cs # tn #$ pdeleteAmount # cs # tn # v) #== 0

propLovelaceValueOf :: Property
propLovelaceValueOf = forAllShrinkShow arbitrary shrink show $ \v ->
  plift (precompileTerm (plam go) # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PBuiltinValue ->
      Term s PBool
    go v = pvalueOf # padaSymbol # padaToken # v #== plovelaceValueOf # v

propMintToBuiltin :: Property
propMintToBuiltin = forAllShrinkShow arbitrary shrink show $ \(mv :: PLA.MintValue) ->
  plift (precompileTerm (plam go) # pconstant mv)
  where
    go ::
      forall (s :: S).
      Term s (PAsData PMintValue) ->
      Term s PBool
    go mv = plet (pfromMintValue mv) $ \_ -> pcon PTrue

propBuiltinToSorted :: Property
propBuiltinToSorted = forAllShrinkShow arbitrary shrink show $ \(v :: PlutusCore.Value) ->
  plift (precompileTerm (plam go) # pconstant v)
  where
    go ::
      forall (s :: S).
      Term s PBuiltinValue ->
      Term s PBool
    go v = plet (pfromData . ptoSortedValue $ v) $ \sv ->
      plet (ppromoteToSortedValue # pforgetSorted sv) $ \_ ->
        pcon PTrue
