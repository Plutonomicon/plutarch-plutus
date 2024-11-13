<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PData () where 
import Plutarch.Prelude ()
```

</p>
</details>

# `PData`

This is a direct synonym to [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). As such, 
it doesn't keep track of what "species" of `Data` it actually is. Is it an `I` data? Is it a `B` data? Nobody can tell for sure!

Consider using [`PAsData`](./PAsData.md) instead for simple cases, i.e. cases other than `Constr`.

Consider using [`PDataSum`/`PDataList`](./PDataSum%20and%20PDataRecord.md) instead when dealing with ADTs, i.e. `Constr` data values.

You can find more information about `PData` at [Developers' Corner](../DEVGUIDE.md).
