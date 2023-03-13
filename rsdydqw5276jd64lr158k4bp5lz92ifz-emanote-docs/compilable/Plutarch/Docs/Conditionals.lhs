<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Conditionals (one) where 
import Plutarch.Prelude 
```

</p>
</details>

# Conditionals

You can simulate `if/then/else` at the Plutarch level using `pif`:

```hs
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
```

This has similar semantics to Haskell's `if/then/else`. That is, only the branch for which the predicate holds - is evaluated.

```haskell
one :: forall s. Term s PInteger
one = pif (pconstant True) 1 2
```

The above evaluates to `1`, which has type `Term s PInteger`.
