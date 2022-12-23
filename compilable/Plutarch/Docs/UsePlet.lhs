<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.UsePlet (pfoo, pfoo') where 
import Plutarch.Prelude
```

</p>
</details>

# `plet` to avoid work duplication

Sometimes, when writing Haskell level functions working on Plutarch terms, you may find yourself needing to re-use the Haskell level function's argument(s) multiple times:

```haskell
pfoo :: forall s. Term s PString -> Term s PString
pfoo x = x <> x
```

In such cases, you should use `plet` on the argument to [avoid duplicating work](./../Tricks/Don't%20duplicate%20work.md).

```haskell
pfoo' :: forall s. Term s PString -> Term s PString
pfoo' x = plet x $ \x' -> x' <> x'
```

