<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.DelayAndForce (hif, pif') where 
import Plutarch.Prelude
import Plutarch.Bool (pif')
```

</p>
</details>

# Strictness and Laziness; Delayed Terms and Forcing

Plutarch, like UPLC, is strict by default; this is in contrast to Haskell, which is non-strict by default (often called "lazy"). This means that evaluating a function application in Plutarch (`#`) always evaluates the argument before executing the function.

This behavior may be undesirable. For example, it is usually unwanted to evaluate both the `then` and `else` branches of an `if` function, for efficiency reasons.

The Plutarch level function `pif'` is naturally strict in its arguments, so it does exactly that. For the purpose of this chapter we take `pif'` as a given, and create the lazy version `pif` based on that.

> Note: The example below does not correspond to the actual implementations of `pif` or `pif'`. It is for pedagogic purposes only.

```hs
-- | Strict if-then-else.
pif' :: Term s (PBool :--> a :--> a :--> a)
```

To prevent evaluation of a term when it gets used as an argument in a function application, we can use `pdelay` to mark the argument term as delayed. On the type-level, it wraps the `PType` tag of a `Term`, as can be seen in its type signature.

```hs
pdelay :: Term s a -> Term s (PDelayed a)
```

The `pforce` function is the inverse to that, it converts a delayed term such that it gets evaluated when used in a function application (`#`). Forcing a term strips the `PDelayed` wrapper on the type-level:

```hs
pforce :: Term s (PDelayed a) -> Term s a
```

We now have the tools needed to create the lazy `pif` based on `pif'`:

```haskell
-- | Utilizing Haskell level functions with `pdelay` and `pforce` to have lazy wrapper around `pif`.
hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse
```

A word of caution: Calling `pforce` on the same delayed term in multiple different places can lead to duplicate evaluation of the term. Users familiar with Haskell's handling of laziness -- where forcing a thunk twice never duplicates computation -- should note that UPLC behaves differently.

Note that `pdelay` is not the only way to get lazy behavior. Haskell-level `Term` arguments, the branches of `pmatch`, and continuation functions on the Plutarch level are all naturally lazy.
