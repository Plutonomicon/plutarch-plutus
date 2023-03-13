<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PAsData (fooData, fooConcrete) where 
import Plutarch.Prelude
```

</p>
</details>

# `PAsData`

This is a typed way of representing [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). It is highly encouraged you use `PAsData` to keep 
track of what "species" of `Data` value you actually have. `Data` can be a `Constr` (for sum of products - ADTs), `Map` (for wrapping assoc maps of Data to Data), `List` (for wrapping 
builtin lists of data), `I` (for wrapping builtin integers), and `B` (for wrapping builtin bytestrings).

Consider a function that takes in and returns a `B` data value - aka `ByteString` as a `Data` value. If you use the direct Plutarch synonym to `Data` - `PData`, you'd have:

```haskell
fooData :: Term s (PData :--> PData)
fooData = undefined
```

That's not very informative - you have no way to ensure that you're actually working with `B` data values. You could use `PAsData` instead:

```haskell
fooConcrete :: Term s (PAsData PByteString :--> PAsData PByteString)
fooConcrete = undefined
```

Now, you have assurance that you're working with a `Data` value that actually represents a builtin bytestring!

Wrapping and unwrapping to and from `PAsData` terms is provided by the [`PIsData`](./../Typeclasses/PIsData.md) typeclass. Specifically, by the functions- `pfromData` and `pdata`.

Some useful instances of these functions:

```hs
pfromData :: Term s (PAsData PInteger) -> Term s PInteger

pfromData :: Term s (PAsData PByteString) -> Term s PByteString

pfromData :: Term s (PAsData (PBuiltinList (PAsData a))) -> Term s (PBuiltinList (PAsData a))

pdata :: Term s PInteger -> Term s (PAsData PInteger)

pdata :: Term s PByteString -> Term s (PAsData PByteString)

pdata :: Term s (PBuiltinList (PAsData a)) -> Term s (PAsData (PBuiltinList (PAsData a)))
```

> Note: using `pfromData` and `pdata` on builtin primitive types (such as `PByteString`, `PInteger`, ...) has an associated cost. Use them sparingly and try to use them only 
> once if possible (i.e. if you used `pfromData` once, `plet` the result and reuse it.)
