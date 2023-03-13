<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.OptimizingUnhoistable (pfoo, pfoo') where 
import Plutarch.Prelude
```

</p>
</details>

# Optimizing unhoistable lambdas

Often times, you'll be creating utility functions inside your Plutarch level functions that use free variables. In such cases, the function is unhoistable (i.e, you cannot use `phoistAcyclic` on it). However, it is likely that your goal is to use this utility function within your primary Plutarch level function several times. At which point, your unhoisted function will be inlined every time you use it and therefore increase script size.

```haskell
pfoo :: Term s (PInteger :--> PBuiltinList PInteger :--> PInteger)
pfoo = plam $ \x l ->
  let innerf = plam $ \y -> x + y
  in innerf # 42 + plength # (pmap # innerf # l)
```

Here, both uses of `innerf` will inline the lambda and then apply. This is problematic since you probably wanted to have a single lambda that you could simply reference with a variable.

In these cases, you can simply [use `plet`](./Don't%20duplicate%20work.md) as you would have [in other places](../Usage/Avoid%20work%20duplication%20using%20plet.md)

```haskell
pfoo' :: Term s (PInteger :--> PBuiltinList PInteger :--> PInteger)
pfoo' = plam $ \x l ->
  plet (plam $ \y -> x + y) $ \innerf ->
    innerf # 42 + plength # (pmap # innerf # l)
```
