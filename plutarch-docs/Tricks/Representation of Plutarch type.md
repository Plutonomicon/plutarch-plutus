# Figuring out the representation of a Plutarch type

We've discussed before how [Plutarch types are merely tags](./../Introduction/Plutarch%20Types.md) and don't have a direct connection to their runtime representations. It's important to be able to intuitively figure out the runtime representations from the data type declaration though. This is why most types follow certain conventions.

The representation can only be one of two categories: builtin and Scott encoded. All _trivial_ builtin types are already defined in Plutarch: `PInteger`, `PByteString`, `PString`, `PBool`, `PUnit`, `PBuiltinList`, and `PBuiltinPair`.

Now, let's discuss patterns of data declarations and what representation they _should_ hint at:

- If it's a newtype to a term containing Plutarch type - it should have the same representation as that underlying Plutarch type.

  e.g. `newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)` is just represented as `PByteString`. This is ensured by deriving all necessary instances (particularly `PlutusType`) using [`DerivePNewtype`](./../Usage/Deriving%20for%20newtypes.md).

- If it's an ADT that derives `PlutusType` generically (i.e. `derive anyclass (PlutusType)`)- it uses Scott encoding. This is typically the encoding you want for non-trivial data types that don't need to be part of datums or redeemers.

  e.g. `PList` derives `PlutusType` generically and is represented with Scott encoding.

- If it's an ADT that derives `PIsDataRepr` generically (i.e. `derive anyclass (PIsDataRepr)`), as well as `PlutusType` via `PIsDataReprInstances`, it's data encoded. Particularly, it's a [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) value - which is part of the builtin types.

  e.g. `PScriptContext` derives `PIsDataRepr` generically and `PlutusType` via `PIsDataReprInstances`.
