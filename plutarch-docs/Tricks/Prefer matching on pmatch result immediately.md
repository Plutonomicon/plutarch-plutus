<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PreferMatchingOnResult (this, this') where 
import Plutarch.Prelude
import Plutarch.Api.V1 (PScriptPurpose (PSpending, PMinting, PRewarding, PCertifying))
```

</p>
</details>

# Prefer pattern matching on the result of `pmatch` immediately

You should always try and pattern match on the result of `pmatch` _immediately_. This is because the semantics of `pmatch` will make anything you write _before_ the pattern match be inlined for every single branch:

```haskell
this :: Term s (PScriptPurpose :--> PInteger)
this = plam $ \x -> pmatch x $ \l ->
  plet (1 + 2) $ \i -> case l of
    PMinting _ -> i + 3
    PSpending _ -> i + 4
    PRewarding _ -> i + 5
    PCertifying _ -> i + 6
```

Notice how the above code `plet`s a computation _before_ matching on `l`, the `pmatch` result. This will make the `plet $ 1 + 2 $ \i -> i + <something>` be inlined in every branch of your pattern match! That is, not only will it compute the `1 + 2` every time, it will _also_ `plet` it, which introduced an extra lambda, only to immediately apply the lambda!

You _should always_ match on the result immediately, whenever possible:

```haskell
this' :: Term s (PScriptPurpose :--> PInteger)
this' = plam $ \x -> plet  (1 + 2) $ \i ->
  pmatch x $ \case
    PMinting _ -> i + 3
    PSpending _ -> i + 4
    PRewarding _ -> i + 5
    PCertifying _ -> i + 6
```

This applies much the same with `do` syntax (whether with `TermCont` or with `QualifiedDo`). Try to use inline partial pattern matching (e.g `PMinting _ <- pmatch x`), or pattern match on the very next line (e.g `l <- pmatch x; case l of ...`).
