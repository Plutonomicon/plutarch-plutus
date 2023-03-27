# Figuring out the representation of a Plutarch type

We've discussed before how [Plutarch types are merely tags](./../Introduction/Plutarch%20Types.md) and don't have a direct connection to their runtime representations. It's important to be able to intuitively figure out the runtime representations from the data type declaration though. This is why most types follow certain conventions.

The representation can only be one of two categories: builtin and Scott encoded. All _trivial_ builtin types are already defined 
in Plutarch: `PInteger`, `PByteString`, `PString`, `PBool`, `PUnit`, `PBuiltinList` and `PBuiltinPair`.

Now, let's discuss patterns of data declarations and what representation they _should_ hint at:

- If it's an ADT that derives `PlutusType` with `DPTStrat _ = PlutusTypeScott`, then the ADTs will be scottencoded. This is 
  what you generally want for non-trivial types that are not stored in datums or redeemers. 

  e.g. `PList` derives `PlutusType` generically and is represented with Scott encoding.

- If it's an ADT that derives PlutusType with `DPTStrat _ = PlutusTypeData` it's data encoded. Particularly, it's 
  a [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) value - which 
  is part of the builtin types.

  e.g. `PScriptContext` derives `PlutusType` using `DPTStrat _ = PlutusTypeData`

- If it's a representationally equal wrapper (think Haskell `newtype`) to a term containing a Plutarch type - it should have the 
  same representation as that underlying Plutarch type.

  e.g. `newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)` is just represented as `PByteString`. This is ensured 
  by deriving `PlutusType` with [`DPTStrat _ = PlutusTypeNewtype`](./../Usage/Deriving%20for%20newtypes.md).
