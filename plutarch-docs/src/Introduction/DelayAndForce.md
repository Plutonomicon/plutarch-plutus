<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.DelayAndForce (hif) where 
import Plutarch.Prelude
import PlutusCore qualified as PLC
import Plutarch.Unsafe (punsafeBuiltin)
import Data.Kind (Type)
```

</p>
</details>

# Strictness and Laziness; Delayed Terms and Forcing

Plutarch, like UPLC, is strict by default; this is in contrast to Haskell, which is non-strict by default (often called "lazy"). This means that evaluating a function application in Plutarch (`#`) always evaluates the argument before executing the function.

This behavior may be undesirable. For example, it is usually unwanted to evaluate both the `then` and `else` branches of an `if` function, for efficiency reasons.

At the level of UPLC, the `IfThenElse` builtin is naturally strict in its
arguments, as UPLC is strictly evaluated. If we want to avoid this, we will need
to write a wrapper around the builtin. To make the presentation a bit more
straightforward, we will first 'directly' wrap the UPLC builtin:

```haskell
-- Don't worry about the specifics here. We have to use this for both efficiency
-- and correctness, as the `IfThenElse` builtin is polymorphic.
pbuiltinITE :: forall (a :: S -> Type) (s :: S) . Term s (PBool :--> a :--> a :--> a)
pbuiltinITE = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse
```

To prevent evaluation of a term when it gets used as an argument in a function
application, we can use `pdelay` to mark the argument term as delayed. On the
type-level, it wraps the `P -> Type` tag of a `Term`, as can be seen in its type signature.

```hs
pdelay :: forall (a :: S -> Type) (s :: S) . Term s a -> Term s (PDelayed a)
```

The `pforce` function is the inverse to that, it converts a delayed term such that it gets evaluated when used in a function application (`#`). Forcing a term strips the `PDelayed` wrapper on the type-level:

```hs
pforce :: forall (a :: S -> Type) (s :: S) . Term s (PDelayed a) -> Term s a
```

We now have the tools needed to create the lazy `pif` based on `pif'`:

```haskell
-- | Utilizing Haskell level functions with `pdelay` and `pforce` to have lazy wrapper around `pif`.
hif :: forall (a :: S -> Type) (s :: S) . 
  Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pbuiltinITE # cond # pdelay whenTrue # pdelay whenFalse
```

A word of caution: Calling `pforce` on the same delayed term in multiple different places can lead to duplicate evaluation of the term. Users familiar with Haskell's handling of laziness -- where forcing a thunk twice never duplicates computation -- should note that UPLC behaves differently.

Note that `pdelay` is not the only way to get lazy behavior. Haskell-level `Term` arguments, the branches of `pmatch`, and continuation functions on the Plutarch level are all naturally lazy.
