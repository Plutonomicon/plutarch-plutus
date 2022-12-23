<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.UsingHaskellLevelFunctions (pelimList, pelimList') where 
import Plutarch.Prelude hiding (pelimList)
```

</p>
</details>

# When to use Haskell level functions?

Although you should generally [prefer Plutarch level functions](./Prefer%20Plutarch%20functions.md), there are times when a Haskell level function is actually much better. However, figuring out _when_ that is the case - is a delicate art.

There is one simple and straightforward use case though, when you want a function argument to be lazily evaluated. In such a case, you should use a Haskell level function that `pdelay`s the argument before calling some Plutarch level function. Recall that [Plutarch level functions are strict](./Plutarch%20functions%20strict.md).

Outside of that straightforward use case, figuring out when to use Haskell level functions is quite complex. Haskell level functions will always be inlined when generating the Plutus Core. Unless the function is used _only once_, this sort of inlining will increase the script size - which is problematic.

However, if the function is used _only once_, and making it Plutarch level causes extra `plam`s and `#`s to be introduced - you should just make it Haskell level. For example, consider the `pelimList` implementation:

```haskell
pelimList :: PLift a => Term s (a :--> PBuiltinList a :--> r) -> Term s r -> Term s (PBuiltinList a) -> Term s r
pelimList match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons # x # xs
  PNil -> match_nil
```

It takes in a Plutarch level function, let's see a typical usage:

```hs
pelimList
  (plam $ \x xs -> pcons # x # (self # xs))
  pnil
  ls
```

This is rather redundant, the above snippet will translate to:

```hs
pmatch ls $ \case
  PCons x xs -> (plam $ \x xs -> pcons # x # (self # xs)) # x # xs
  PNil -> match_nil
```

Extra `plam`s and `#`s have been introduced. Really, `pelimList` could have taken a Haskell level function instead:

```haskell
pelimList' :: 
  forall (a :: PType) (r :: PType) (s :: S).
  PLift a 
  => (Term s a -> Term s (PBuiltinList a) -> Term s r) 
  -> Term s r -> Term s (PBuiltinList a) 
  -> Term s r
pelimList' match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons x xs
  PNil -> match_nil
```

Now, the following usage:

```hs
pelimList
  (\x xs -> pcons # x # (self # xs))
  pnil
  ls
```

would turn into:

```hs
pmatch ls $ \case
  PCons x xs -> pcons # x # (self # xs)
  PNil -> match_nil
```

It turns out that using `pelimList` _almost always_ involves using a one-off Haskell level function (and therefore a redundant `plam`). As such, `pelimList` benefits greatly from just taking a Haskell level function directly.

However, **not all higher order functions** benefit from taking Haskell level functions. In many higher order function use cases, you could benefit from passing a commonly used function argument, rather than a one-off function argument. Imagine `map`, you don't always map with one-off functions - often, you `map` with existing, commonly used functions. In these cases, that commonly used function ought to be a Plutarch level function, so it can be hoisted and `map` can simply reference it.
