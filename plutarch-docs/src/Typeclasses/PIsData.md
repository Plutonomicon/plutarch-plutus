# `PIsData`

The `PIsData` typeclass facilitates easy and type safe conversion between Plutarch types and their corresponding 
[`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) representation. It keeps track of the type information through [`PAsData`](./../Types/PAsData.md).

```hs
class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)
```

[`PInteger`](./../Types/PInteger.md) has a `PIsData` instance. The `PData` representation of `PInteger` is, of course, an `I` data. And you can get the `PInteger` back from an `I` data using `UnIData` (i.e. `pasInt`).

```hs
instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x
```

In essence, `pdata` wraps a `PInteger` into an `I` data value. Whereas `pfromData` simply unwraps the `I` data value to get a `PInteger`.

> Aside: You might be asking, what's an "`I` data value"? This is referring to the different constructors of `Data`/`BuiltinData`. You can find a full explanation of this at 
[Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

For the simple constructors that merely wrap a builtin type into `Data`, e.g. `Integer`s, `Bytestrings`, lists, and `AssocMap`, `PIsData` works in much the same way as above. However, what 
about `Constr` data values? When you have an ADT that doesn't correspond to those simple builtin types directly - but you still need to encode it as `Data` (e.g. `PScriptContext`). In this 
case, you should [implement `PIsDataRepr`](./PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends) and you'll get the `PIsData` instance for free!
