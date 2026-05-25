{- | Conversions between ledger API representations of @Value@s and the new
'PBuiltinValue', plus some utility functions.

= Why is this module necessary?

In almost all situations, @Value@s will be provided to a script as their
@Data@ encodings, whether this is through an input or a @PScriptContext@.
@plutarch-ledger-api@ has an extensive collection of functions for dealing
with such representations, as well as a range of @newtype@ wrappers to
provide guarantees of form when doing so.

However, as these operations have to work over (effectively) nested cons
lists, they are not efficient for many operations, especially simpler ones
like lookups or primitive insertions. In this regard, 'PBuiltinValue' excels,
as these are calls to builtin operations, which are much more efficient.
However, some conversion cost must be paid to go from the @Data@ encoding to
a 'PBuiltinValue'.

In addition to this, the @UnValueData@ primitive has quite specific
expectations of any @Data@ it is given. More precisely, the argument must:

* Be made using the @Map@ constructor;
* Every \'key\' must be encoded using the @B@ constructor, and be at most 32
  bytes in length;
* Every \'value\' must be a non-empty list of key-value pairs.
* Every \'inner key\' must be encoded using the @B@ constructor, and be at
  most 32 bytes in length;
* Every \'inner value\' must be encoded using the @I@ constructor, and be
  non-zero, as well as fitting within a 128-bit signed integer;
* \'Keys\', both \'outer\' and \'inner\', must be in strictly ascending
  order, which implies no duplicates.

Any deviation from this will cause the builtin to error. Thus, of all the
@newtype@s provided for structural safety by @plutarch-ledger-api@, only
'PMintValue' is suitable.

To make matters worse, the inverse @ValueData@ builtin does not guarantee
that an arbitrary 'PBuiltinValue' will satisfy the structural requirements of
a 'PMintValue', as it may contain an Ada entry. Thus, if \'converting back\'
is required, we can only guarantee a @PSortedValue@. Any other conversion is
technically speaking unsafe and may error. Worse still, there is no
straightforward way to verify what kind of \'inner\' or \'outer\' keys a
'PBuiltinValue' contains without converting it first!

Thus, this module provides clearly labelled conversions, safe and unsafe, as
well as some helper wrapper functions to make 'PBuiltinValue' easier to use
with other types provided by @plutarch-ledger-api@.

@since 3.7.0
-}
module Plutarch.LedgerApi.V3.Value (
  -- * Conversions

  -- ** Safe
  pfromRawValue,
  pfromSortedValue,
  pfromMintValue,
  ptoSortedValue,
  ptoLedgerValue,
  ptoLedgerValue',
  ptoMintValue,

  -- ** Unsafe
  punsafeFromRawValue,
  punsafeFromSortedValue,
  punsafeToLedgerValue,
  punsafeToMintValue,

  -- * Construction
  pemptyBuiltinValue,
  psingletonBuiltinValue,

  -- * Queries
  pvalueOf,
  plovelaceValueOf,

  -- * Updates
  preplaceAmountPositive,
  preplaceAmountNegative,
  pdeleteAmount,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Value (
  PBuiltinValue,
  pinsertCoin,
  plookupCoin,
  punValueData,
  pvalueData,
 )
import Plutarch.LedgerApi.AssocMap (PUnsortedMap)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3.MintValue (PMintValue)
import Plutarch.LedgerApi.Value (
  PLedgerValue,
  PRawValue,
  PSortedValue,
  pinsertAdaEntry,
 )
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol)
import Plutarch.LedgerApi.Value.TokenName (PTokenName)
import Plutarch.Prelude (
  PAsData,
  PBool (PFalse, PTrue),
  PBuiltinList (PCons, PNil),
  PBuiltinPair (PBuiltinPair),
  PInteger,
  PPositive,
  S,
  Term,
  pcon,
  pconstant,
  pdata,
  pfix,
  pforgetData,
  pfromData,
  phoistAcyclic,
  pif,
  plam,
  plet,
  pmatch,
  pnegate,
  pto,
  pupcast,
  (#),
  (#$),
  (#<),
  (#==),
  (:-->),
 )
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Value qualified as PlutusCore

{- | Convert the @Data@ representation of a 'PMintValue' to a 'PBuiltinValue'.
This is done via a builtin (thus efficient), and is safe.

@since 3.7.0
-}
pfromMintValue ::
  forall (s :: S).
  Term s (PAsData PMintValue) -> Term s PBuiltinValue
pfromMintValue x = punValueData # pforgetData x

{- | After verifying the conversion is safe, convert a 'PRawValue' @Data@
representation into a 'PBuiltinValue'. To save on allocations, this is
written in a continuation-passing style:

* The second argument is what should happen if validation fails; and
* The third argument is what should happen if validation succeeds.

= Important note

This operation is slow, as it has to check the entire input.

@since 3.7.0
-}
pfromRawValue ::
  forall (r :: S -> Type) (s :: S).
  Term s PRawValue ->
  Term s r ->
  Term s (PBuiltinValue :--> r) ->
  Term s r
pfromRawValue x whenFail whenSucceed =
  pif (pcanBeBuiltinValue # x) (whenSucceed #$ punValueData # pforgetData (pdata x)) whenFail

{- | Convert the @Data@ representation of a 'PRawValue' into a 'PBuiltinValue'
while not doing any checks.

This is /not/ safe, and will error if any of the invariants of
'PBuiltinValue' are violated. Use with care.

@since 3.7.0
-}
punsafeFromRawValue ::
  forall (s :: S).
  Term s (PAsData PRawValue) -> Term s PBuiltinValue
punsafeFromRawValue x = punValueData # pforgetData x

{- | As 'pfromRawValue', except for 'PSortedValue's instead. This is more
efficient, as we only need to check for the absence of zero amounts.

@since 3.7.0
-}
pfromSortedValue ::
  forall (r :: S -> Type) (s :: S).
  Term s PSortedValue ->
  Term s r ->
  Term s (PBuiltinValue :--> r) ->
  Term s r
pfromSortedValue x whenFail whenSucceed =
  pif (pnoZeroAmounts # x) (whenSucceed #$ punValueData # pforgetData (pdata x)) whenFail

{- | As 'punsafeFromRawValue', except for 'PSortedValue's instead. The same
caveats apply.

@since 3.7.0
-}
punsafeFromSortedValue :: forall (s :: S). Term s (PAsData PSortedValue) -> Term s PBuiltinValue
punsafeFromSortedValue x = punValueData # pforgetData x

{- | Convert a 'PBuiltinValue' into the @Data@ representation of a
'PSortedValue'. This is done via a builtin (thus efficient), and is safe.

If you need to convert to something less specific, use 'pupcast'.

@since 3.7.0
-}
ptoSortedValue ::
  forall (s :: S).
  Term s PBuiltinValue -> Term s (PAsData PSortedValue)
ptoSortedValue v = punsafeCoerce $ pvalueData # v

{- | Checks for an Ada entry in the input 'PBuiltinValue'. If none is found,
produce the second argument; otherwise, convert the 'PBuiltinValue' into a
'PAsData' 'PLedgerValue' and call the third argument with it.

@since 3.7.0
-}
ptoLedgerValue ::
  forall (r :: S -> Type) (s :: S).
  Term s PBuiltinValue ->
  Term s r ->
  Term s (PAsData PLedgerValue :--> r) ->
  Term s r
ptoLedgerValue v whenInvalid whenValid =
  pif (plovelaceValueOf # v #== 0) whenInvalid (whenValid # punsafeCoerce (pvalueData # v))

{- | As 'ptoLedgerValue', but \'fills in\' a zero ADA entry if one is missing.
This is a costly operation, as it must be done /after/ conversion. Use with
care.

@since 3.7.0
-}
ptoLedgerValue' ::
  forall (s :: S).
  Term s PBuiltinValue ->
  Term s PLedgerValue
ptoLedgerValue' v = punsafeCoerce $ pinsertAdaEntry # (pfromData . ptoSortedValue $ v)

{- | As 'ptoLedgerValue', except the check is for the /absence/ of an Ada entry,
and the third argument is called with a 'PAsData' 'PMintValue' instead when
appropriate.

@since 3.7.0
-}
ptoMintValue ::
  forall (r :: S -> Type) (s :: S).
  Term s PBuiltinValue ->
  Term s r ->
  Term s (PAsData PMintValue :--> r) ->
  Term s r
ptoMintValue v whenInvalid whenValid =
  pif (plovelaceValueOf # v #== 0) (whenValid # punsafeCoerce (pvalueData # v)) whenInvalid

{- | Convert a 'PBuiltinValue' into a 'PAsData' 'PLedgerValue' without checking
anything. Only use this if you are certain that the 'PBuiltinValue' does not
violate any internal invariants of 'PLedgerValue'. In particular, there /must/
be an Ada amount in the argument 'PMintValue'.

@since 3.7.0
-}
punsafeToLedgerValue ::
  forall (s :: S).
  Term s PBuiltinValue -> Term s (PAsData PLedgerValue)
punsafeToLedgerValue v = punsafeCoerce $ pvalueData # v

{- | Convert a 'PBuiltinValue' into a 'PAsData' 'PLedgerValue' without checking
anything. Only use this if you are certain that the 'PBuiltinValue' does not
violate any internal invariants of 'PMintValue'. In particular, there should
/not/ be an Ada amount in the argument 'PMintValue'.

@since 3.7.0
-}
punsafeToMintValue ::
  forall (s :: S).
  Term s PBuiltinValue -> Term s (PAsData PMintValue)
punsafeToMintValue v = punsafeCoerce $ pvalueData # v

{- | The 'PBuiltinValue' without any amounts.

@since 3.7.0
-}
pemptyBuiltinValue :: forall (s :: S). Term s PBuiltinValue
pemptyBuiltinValue = pconstant PlutusCore.empty

{- | A 'PBuiltinValue' containing an amount of a single currency-token name
combination. If the 'PInteger' argument is @0@, this will be identical to
'pemptyBuiltinValue'.

@since 3.7.0
-}
psingletonBuiltinValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PBuiltinValue)
psingletonBuiltinValue = phoistAcyclic $ plam $ \symbol token amount ->
  pinsertCoin # pupcast symbol # pupcast token # amount # pemptyBuiltinValue

{- | Look up the amount associated with a given combination of 'PCurrencySymbol'
and 'PTokenName' in the given 'PBuiltinValue'. As 'PBuiltinValue' cannot
store zero amounts, if this returns @0@, it means that no amount is
associated with the given combination of 'PCurrencySymbol' and 'PTokenName'
in this 'PBuiltinValue.

@since 3.7.0
-}
pvalueOf ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinValue :--> PInteger)
pvalueOf = phoistAcyclic $ plam $ \cs tn v ->
  plookupCoin # pupcast cs # pupcast tn # v

{- | As 'pvalueOf', but for Lovelace specifically. The same caveats apply.

@since 3.7.0
-}
plovelaceValueOf ::
  forall (s :: S).
  Term s (PBuiltinValue :--> PInteger)
plovelaceValueOf = phoistAcyclic $ plam $ \v ->
  plookupCoin # pconstant "" # pconstant "" # v

{- | Replace the amount at the given currency-token name combination in the
given 'PBuiltinValue' with the given amount. The new amount will be positive.
If the given currency-token name combination does not exist, create it.

= Note

A 'PBuiltinValue' cannot store an amount that would not fit into a 128-bit
signed integer. This function will error if the result would be forced to
store such an amount.

@since 3.7.0
-}
preplaceAmountPositive ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PPositive :--> PBuiltinValue :--> PBuiltinValue)
preplaceAmountPositive = phoistAcyclic $ plam $ \cs tn amount v ->
  pinsertCoin # pupcast cs # pupcast tn # pupcast amount # v

{- | Replace the amount at the given currency-token name combination in the
given 'PBuiltinValue' with the given amount. The new amount will be negative.
If the given currency-token name combination does not exist, create it.

= Note

A 'PBuiltinValue' cannot store an amount that would not fit into a 128-bit
signed integer. This function will error if the result would be forced to
store such an amount.

@since 3.7.0
-}
preplaceAmountNegative ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PPositive :--> PBuiltinValue :--> PBuiltinValue)
preplaceAmountNegative = phoistAcyclic $ plam $ \cs tn amount v ->
  pinsertCoin # pupcast cs # pupcast tn # (pnegate # pupcast amount) # v

{- | Remove any amount associated with the given currency-token name combination
in the given 'PBuiltinValue'. If there is no such combination in the given
'PBuiltinValue', this does nothing.

@since 3.7.0
-}
pdeleteAmount ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinValue :--> PBuiltinValue)
pdeleteAmount = phoistAcyclic $ plam $ \cs tn v ->
  pinsertCoin # pupcast cs # pupcast tn # 0 # v

-- Helpers

pcanBeBuiltinValue :: forall (s :: S). Term s (PRawValue :--> PBool)
pcanBeBuiltinValue = phoistAcyclic $ plam $ \v -> pmatch (pto (pto (pto v))) $ \case
  PNil -> pcon PTrue
  PCons x xs -> pmatch x $ \case
    PBuiltinPair prevK innerMap ->
      pif
        (verifyInner # innerMap)
        ( pmatch xs $ \case
            PNil -> pcon PTrue
            PCons y ys -> goOuter # pfromData prevK # y # ys
        )
        (pcon PFalse)
  where
    verifyInner ::
      forall (s' :: S).
      Term s' (PAsData (PUnsortedMap PTokenName PInteger) :--> PBool)
    verifyInner = phoistAcyclic $ plam $ \innerMap -> pmatch (pto (pto (pfromData innerMap))) $ \case
      -- Invalid to have an empty inner map
      PNil -> pcon PFalse
      PCons x xs -> pmatch x $ \case
        PBuiltinPair prevK amount ->
          pif
            (pfromData amount #== 0)
            (pcon PFalse)
            ( pmatch xs $ \case
                PNil -> pcon PTrue
                PCons y ys -> goInner # pfromData prevK # y # ys
            )
    goOuter ::
      forall (s' :: S).
      Term
        s'
        ( PCurrencySymbol
            :--> PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PUnsortedMap PTokenName PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PUnsortedMap PTokenName PInteger)))
            :--> PBool
        )
    goOuter = phoistAcyclic $ pfix $ \self -> plam $ \prevK curr rest -> pmatch curr $ \case
      PBuiltinPair currK currV -> plet (pfromData currK) $ \currK' ->
        pif
          (prevK #< currK')
          ( pif
              (verifyInner # currV)
              ( pmatch rest $ \case
                  PNil -> pcon PTrue
                  PCons y ys -> self # currK' # y # ys
              )
              (pcon PFalse)
          )
          (pcon PFalse)
    goInner ::
      forall (s' :: S).
      Term
        s'
        ( PTokenName
            :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger)
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBool
        )
    goInner = phoistAcyclic $ pfix $ \self -> plam $ \prevK curr rest -> pmatch curr $ \case
      PBuiltinPair currK currV -> plet (pfromData currK) $ \currK' ->
        pif
          (prevK #< currK')
          ( pif
              (pfromData currV #== 0)
              (pcon PFalse)
              ( pmatch rest $ \case
                  PNil -> pcon PTrue
                  PCons y ys -> self # currK' # y # ys
              )
          )
          (pcon PFalse)

pnoZeroAmounts :: forall (s :: S). Term s (PSortedValue :--> PBool)
pnoZeroAmounts = phoistAcyclic $ plam $ \v -> AssocMap.pall # go # AssocMap.pforgetSorted (pto v)
  where
    go :: forall (s' :: S). Term s' (AssocMap.PSortedMap PTokenName PInteger :--> PBool)
    go = phoistAcyclic $ plam $ \m ->
      AssocMap.pall # plam (\i -> pif (i #== 0) (pcon PFalse) (pcon PTrue)) # AssocMap.pforgetSorted m
