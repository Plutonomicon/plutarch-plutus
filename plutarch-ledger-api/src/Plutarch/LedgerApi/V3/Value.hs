-- Needed to ensure that we only try to convert Value-like things
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

@since wip
-}
module Plutarch.LedgerApi.V3.Value (
  -- * Conversions

  -- ** Safe
  pfromMintValue,
  ptoSortedValue,

  -- ** Unsafe
  punsafeFromAnyValue,

  -- * Construction
  pemptyBuiltinValue,
  psingletonBuiltinValue,
  padjustAmount,

  -- * Queries
  pvalueOf,
  plovelaceValueOf,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Value (
  PBuiltinValue,
  pinsertCoin,
  plookupCoin,
  punValueData,
  pvalueData,
 )
import Plutarch.LedgerApi.V3.MintValue (PMintValue)
import Plutarch.LedgerApi.Value (PRawValue, PSortedValue)
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol)
import Plutarch.LedgerApi.Value.TokenName (PTokenName)
import Plutarch.Prelude (
  PAsData,
  PInteger,
  PIsData,
  PSubtype,
  S,
  Term,
  pconstant,
  pforgetData,
  phoistAcyclic,
  plam,
  pupcast,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Value qualified as PlutusCore

{- | Convert the @Data@ representation of a 'PMintValue' to a 'PBuiltinValue'.
This is done via a builtin (thus efficient), and is safe.

@since wip
-}
pfromMintValue ::
  forall (s :: S).
  Term s (PAsData PMintValue) -> Term s PBuiltinValue
pfromMintValue x = punValueData # pforgetData x

{- | Convert /any/ @Data@ representation of a @Value@ into a 'PBuiltinValue'.
This is /not/ safe, and will error if any of the invariants of
'PBuiltinValue' are violated. Use with care.

@since wip
-}
punsafeFromAnyValue ::
  forall (a :: S -> Type) (s :: S).
  (PSubtype a PRawValue, PIsData a) => Term s (PAsData a) -> Term s PBuiltinValue
punsafeFromAnyValue x = punValueData # pforgetData x

{- | Convert a 'PBuiltinValue' into the @Data@ representation of a
'PSortedValue'. This is done via a builtin (thus efficient), and is safe.

If you need to convert to something less specific, use 'pupcast'.

@since wip
-}
ptoSortedValue ::
  forall (s :: S).
  Term s PBuiltinValue -> Term s (PAsData PSortedValue)
ptoSortedValue v = punsafeCoerce $ pvalueData # v

{- | The 'PBuiltinValue' without any amounts.

@since wip
-}
pemptyBuiltinValue :: forall (s :: S). Term s PBuiltinValue
pemptyBuiltinValue = pconstant PlutusCore.empty

{- | A 'PBuiltinValue' containing an amount of a single currency-token name
combination. If the 'PInteger' argument is @0@, this will be identical to
'pemptyBuiltinValue'.

@since wip
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

@since wip
-}
pvalueOf ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinValue :--> PInteger)
pvalueOf = phoistAcyclic $ plam $ \cs tn v ->
  plookupCoin # pupcast cs # pupcast tn # v

{- | As 'pvalueOf', but for Lovelace specifically. The same caveats apply.

@since wip
-}
plovelaceValueOf ::
  forall (s :: S).
  Term s (PBuiltinValue :--> PInteger)
plovelaceValueOf = phoistAcyclic $ plam $ \v ->
  plookupCoin # pconstant "" # pconstant "" # v

{- | Given a 'PCurrencySymbol', a 'PTokenName' and a 'PBuiltinValue', adjust the
amount associated with the combination of the given 'PCurrencySymbol' and
'PTokenName' in the given 'PBuiltinValue' by the 'PInteger' argument.
Specifically, a negative 'PInteger' argument will adjust the amount down,
while a positive 'PInteger' argument will adjust it up. If the (effective)
amount would become @0@, the given combination will be removed.

= Note

A 'PBuiltinValue' cannot store an amount that would not fit into a 128-bit
signed integer. This function will error if the result would store an amount
that exceeds this.

@since wip
-}
padjustAmount ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PBuiltinValue :--> PBuiltinValue)
padjustAmount = phoistAcyclic $ plam $ \cs tn delta v ->
  pinsertCoin # pupcast cs # pupcast tn # delta # v
