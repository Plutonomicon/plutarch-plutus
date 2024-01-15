<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE QualifiedDo #-}
module Plutarch.Docs.PDataFields (foo, foo', res, mockCtx, purpose, Vehicle (..), PVehicle (..), PVehicle' (..), PFoo (..), test) where 

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts (PScriptContext, PScriptPurpose (PSpending, PMinting, PRewarding, PCertifying))
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import PlutusLedgerApi.V1 (TxInfo (TxInfo), POSIXTime(POSIXTime), ScriptPurpose (Minting), ScriptContext (ScriptContext))
import PlutusLedgerApi.V1.Value (CurrencySymbol (CurrencySymbol))
import PlutusLedgerApi.V1.Interval (interval)
import qualified PlutusTx
import Plutarch.Docs.Run (evalWithArgsT)
```

</p>
</details>


# `PlutusType` via `PlutusTypeData` & `PDataFields`

Deriving `PlutusType` with `DPTStrat PlutusTypeData` allows for easily constructing _and_ deconstructing `Constr` 
[`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values. It allows fully type safe matching on 
[`Data` encoded](./../Concepts/Data%20and%20Scott%20encoding.md) values, without embedding type information within the generated script - unlike 
PlutusTx. `PDataFields`, on top of that, allows for ergonomic field access.

> Aside: What's a `Constr` data value? Briefly, it's how Plutus Core encodes non-trivial ADTs into `Data`/`BuiltinData`. Together with `BuiltinList`s it allows for a sum-of-products encoding. 
  Essentially, whenever you have a custom non-trivial ADT (that isn't just an integer, bytestring, string/text, list, or assoc map) - and you want to represent it as a data encoded value - 
  you should derive `PIsData` for it

For example, `PScriptContext` - which is the Plutarch synonym to [`ScriptContext`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext)
- has the necessary instances. This lets you easily keep track of its type, match on it, deconstruct it - you name it!

```haskell
foo :: Term s (PScriptContext :--> PString)
foo = plam $ \ctx -> P.do
  purpose <- pmatch $ pfield @"purpose" # ctx
  case purpose of
    PMinting _ -> "It's minting!"
    PSpending _ -> "It's spending!"
    PRewarding _ -> "It's rewarding!"
    PCertifying _ -> "It's certifying!"
```

> Note: The above snippet uses GHC 9 features (`QualifiedDo`). Be sure to check out [Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md).

Of course, just like `ScriptContext` - `PScriptContext` is represented as a `Data` value in Plutus Core. Plutarch just lets you keep track of the _exact representation_ of it within the type system.

Here's how `PScriptContext` is defined:

```hs
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
```

It's a constructor containing a [`PDataRecord`](./../Types/PDataSum%20and%20PDataRecord.md) term. It has 2 fields- `txInfo` and `purpose`.

First, we extract the `purpose` field using `pfield @"purpose"`:

```hs
pfield :: Term s (PScriptContext :--> PScriptPurpose)
```

> Note: When extracting several fields from the same variable, you should instead use `pletFields`. See: [Extracting fields](#all-about-extracting-fields)

> Aside: `pfield` is actually return type polymorhpic. It could've returned either `PAsData PScriptPurpose` and `PScriptPurpose`. In this case, GHC correctly infers that we actually want a 
> `PScriptPurpose`, since `pmatch` doesn't work on `PAsData PScriptPurpose`!
>
> Sometimes GHC isn't so smart, and you're forced to provide an explicit type annotation. Or you can simply use `pfromData $ pfield ....`.

Now, we can `pmatch` on our `Term s PScriptPurpose` to extract the Haskell ADT (`PScriptPurpose s`) out of the Plutarch term:

```hs
pmatch :: Term s PScriptPurpose -> (PScriptPurpose s -> Term s PString) -> Term s PString
```

Now that we have `PScriptPurpose s`, we can just `case` match on it! `PScriptPurpose` is defined as:

```hs
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
```

It's just a Plutarch sum type.

We're not really interested in the fields (the `PDataRecord` term), so we just match on the constructor with the familiar `case`. Easy!

Let's pass in a `ScriptContext` as a `Data` value from Haskell to this Plutarch script and see if it works!

```haskell
mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      mempty
      mempty
      ""
    )
    (Minting (CurrencySymbol ""))

res :: Either _ _
res = foo `evalWithArgsT` [PlutusTx.toData mockCtx]
-- Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf string "It's minting!"))))
```

> Aside: You can find the definition of `evalWithArgsT` at [Compiling and Running](../README.md#compiling-and-running).

## All about extracting fields

We caught a glimpse of field extraction in the example above, thanks to `pfield`. However, that barely touched the surface.

Once a type has a `PDataFields` instance, field extraction can be done with these 3 functions:

- `pletFields`
- `pfield`
- `getField` (when not using `OverloadedRecordDot` or [record dot preprocessor](https://hackage.haskell.org/package/record-dot-preprocessor))

Each has its own purpose. However, `pletFields` is arguably the most general purpose and most efficient. Whenever you need to extract several fields from the same variable, you should use `pletFields`:

```haskell
foo' :: Term s (PScriptContext :--> PUnit)
foo' = plam $ \ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  let
    _purpose = ctx.purpose
    _txInfo = ctx.txInfo
  -- <use purpose and txInfo here>
  pconstant ()
```

> Note: The above snippet uses GHC 9 features (`QualifiedDo` and `OverloadedRecordDot`). Be sure to check out [Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md) and [alternatives to `OverloadedRecordDot`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot).

In essence, `pletFields` takes in a type level list of the field names that you want to access and a continuation function that takes in an `HRec`. This `HRec` is essentially a collection of the bound fields. You don't have to worry too much about the details of `HRec`. This particular usage has type:

```hs
pletFields :: Term s PScriptContext
  -> (HRec
        (BoundTerms
           '[ "txInfo" ':= PTxInfo, "purpose" ':= PScriptPurpose]
           '[ 'Bind, 'Bind]
           s)
      -> Term s PUnit)
  -> Term s PUnit
```

You can then access the fields on this `HRec` using `OverloadedRecordDot`.

Next up is `pfield`. You should _only ever_ use this if you just want one field from a variable and no more. Its usage is simply `pfield @"fieldName" # variable`. You can, however, also use `pletFields` in this case (e.g. `pletFields @'["fieldName"] variable`). `pletFields` with a singular field has the same efficiency as `pfield`!

Finally, `getField` is merely there to supplement the lack of record dot syntax. See: [Alternative to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot).

> Note: An important thing to realize is that `pfield` and `getField` (or overloaded record dot on `HRec`) are _return type polymorphic_. They can return both `PAsData Foo` or `Foo` terms, 
  depending on the surrounding context. This is very useful in the case of `pmatch`, as `pmatch` doesn't work on `PAsData` terms. So you can simply write `pmatch $ pfield ...` and `pfield` 
  will correctly choose to _unwrap_ the `PAsData` term.

### Alternatives to `OverloadedRecordDot`

If `OverloadedRecordDot` is not available, you can also try using the [record dot preprocessor plugin](https://hackage.haskell.org/package/record-dot-preprocessor).

If you don't want to use either, you can simply use `getField`. In fact, `ctx.purpose` above just translates to `getField @"purpose" ctx`. Nothing magical there!

## All about constructing data values

We learned about type safe matching (through `PlutusType`) as well as type safe field access (through `PDataFields`) - how about construction? You can derive 
[`PlutusType`](./PlutusType,%20PCon,%20and%20PMatch.md), using a data representation by using `DPTStrat _ = PlutusTypeData` and `PlutusType` bestows the ability 
to not only _deconstruct_, but also **construct** values - you can do that just as easily!

Let's see how we could build a `PMinting` `PScriptPurpose` given a `PCurrencySymbol`:

```haskell
currSym :: Term s PCurrencySymbol
currSym = pconstant $ CurrencySymbol "foo"

purpose :: Term s PScriptPurpose
purpose = pcon $ PMinting fields
  where
    currSymDat :: Term _ (PAsData PCurrencySymbol)
    currSymDat = pdata currSym
    fields :: Term _ (PDataRecord '[ "_0" ':= PCurrencySymbol ])
    fields = pdcons # currSymDat # pdnil
```

All the type annotations are here to help!

This is just like regular `pcon` usage you've [from `PlutusType`/`PCon`](./PlutusType,%20PCon,%20and%20PMatch.md). It takes in the Haskell ADT of your Plutarch type and gives back a Plutarch term.

What's more interesting, is the `fields` binding. Recall that `PMinting` is a constructor with one argument, that argument is a [`PDataRecord`](../Types/PDataSum%20and%20PDataRecord.md) term. In particular, we want: `Term s (PDataRecord '["_0" ':= PCurrencySymbol ])`. It encodes the exact type, position, and name of the field. So, all we have to do is create a `PDataRecord` term!

Of course, we do that using `pdcons` - which is just the familiar `cons` but for `PDataRecord` terms.

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))
```

It takes a `PAsData a` and adds that `a` to the `PDataRecord` heterogenous list. We feed it a `PAsData PCurrencySymbol` term and `pdnil` - the empty data record. That should give us:

```hs
pdcons # currSymDat # pdnil :: Term _ (PDataRecord '[ label ':= PCurrencySymbol ])
```

Cool! Wait, what's `label`? It's the field name associated with the field, in our case, we want the field name to be `_0` - because that's what the `PMinting` constructor wants. You can 
either specify the label with a type application or you can just have a type annotation for the binding (which is what we do here). Or you can let GHC try and match up the `label` with 
the surrounding environment!

Now that we have `fields`, we can use it with `PMinting` to build a `PScriptPurpose s` and feed it to `pcon` - we're done!

## Implementing `PIsData` and friends

Implementing these is rather simple with generic deriving. All you need is a well formed type using `PDataRecord`. For example, suppose you wanted to implement `PIsData` for the Plutarch 
version of this Haskell type:

```haskell
data Vehicle
  = FourWheeler Integer Integer Integer Integer
  | TwoWheeler Integer Integer
  | ImmovableBox
```

You'd declare the corresponding Plutarch type as:

```haskell
data PVehicle' (s :: S)
  = PFourWheeler' (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler' (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox' (Term s (PDataRecord '[]))
```

Each field type must also have a `PIsData` instance. We've fulfilled this criteria above as `PInteger` does indeed have a `PIsData` instance. However, think of `PBuiltinList`s, as an example. `PBuiltinList`'s `PIsData` instance is restricted to only `PAsData` elements.

```hs
instance PIsData a => PIsData (PBuiltinList (PAsData a))
```

Thus, you can use `PBuiltinList (PAsData PInteger)` as a field type, but not `PBuiltinList PInteger`.

> Note: The constructor ordering in `PVehicle` matters! If you used [`makeIsDataIndexed`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#v:makeIsDataIndexed) on 
> `Vehicle` to assign an index to each constructor - the Plutarch type's constructors must follow the same indexing order.
>
> In this case, `PFourWheeler` is at the 0th index, `PTwoWheeler` is at the 1st index, and `PImmovableBox` is at the 3rd index. Thus, the corresponding `makeIsDataIndexed` usage should be:
>
> ```hs
> PlutusTx.makeIsDataIndexed ''PVehicle [('FourWheeler,0),('TwoWheeler,1),('ImmovableBox,2)]
> ```
>
> Also see: [Isomorphism between Haskell ADTs and `PIsData`](./../Tricks/makeIsDataIndexed,%20Haskell%20ADTs,%20and%20PIsDataRepr.md)

And you'd simply derive `PlutustType` with plutus data representation using generics. You can then also derive `PIsData` and if the dataype only has one ocnstructor `PDataFields`. 

Furthermore, you can also derive the following typeclasses after deriving `PlutusType` with `DPTStrat _ = PlutusTypeData`

- [`PEq`](./PEq%20and%20POrd.md)
- [`POrd`](./PEq%20and%20POrd.md)

Combine all that, and you have:

```haskell
data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PVehicle where type DPTStrat _ = PlutusTypeData
```

> Note: You cannot derive `PIsData` for types that are represented using [Scott encoding](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding). Your types must be well formed and 
  should be using `PDataRecord` terms instead.

That's it! Now you can represent `PVehicle` as a `Data` value, as well as deconstruct and access its fields super ergonomically. Let's try it!

```haskell
test :: Term s (PVehicle :--> PInteger)
test = plam $ \veh' -> P.do
  veh <- pmatch veh'
  case veh of
    PFourWheeler fwh' -> P.do
      fwh <- pletFields @'["_0", "_1", "_2", "_3"] fwh'
      fwh._0 + fwh._1 + fwh._2 + fwh._3
    PTwoWheeler twh' -> P.do
      twh <- pletFields @'["_0", "_1"] twh'
      twh._0 + twh._1
    PImmovableBox _ -> 0
```

What about types with singular constructors? It's quite similar to the sum type case. Here's how it looks:

```haskell
newtype PFoo (s :: S) = PMkFoo (Term s (PDataRecord '["foo" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)
instance DerivePlutusType PFoo where type DPTStrat _ = PlutusTypeData
```

Just an extra `PDataFields` derivation compared to the sum type usage!
