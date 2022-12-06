<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Recursion (pfac) where 
import Plutarch.Prelude
```

</p>
</details>

# Recursion

To emulate recursion in [UPLC (Untyped Plutus Core)](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md), you need to 
use [the Y combinator](https://stackoverflow.com/questions/93526/what-is-a-y-combinator). Plutarch provides the Y combinator with the name `pfix`:

```hs
pfix :: Term s (((a :--> b) :--> (a :--> b)) :--> (a :--> b))
```

It works as you would expect, though the type is scary. Think of it as the Haskell type:

```hs
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
```

The first argument is "self", or the function you want to recurse with.

The below example implements a Plutarch-level factorial function:

```haskell
pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

Note how `f` takes in a `self` and just recurses on it. All you have to do, is create a Plutarch level function by using `plam` on `f` and `pfix` 
the result - and that `self` argument will be taken care of for you.
