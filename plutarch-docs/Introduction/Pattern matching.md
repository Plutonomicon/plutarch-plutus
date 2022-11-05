<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PatternMatching (pisJust) where 
import Plutarch.Prelude
```

</p>
</details>
# Pattern matching constant `Term`s with `pmatch`.

We've shown how to construct `Term`s out of the data constructors of types with kind `PType` (i.e., `pcon . PJust`). Next, it is natural that we may want to pattern match on `Term` with a known `PType` tag (i.e., of a value with type `Term s (PMaybe a)`) to produce another `Term` (i.e., depending on whether the value matches `PJust _` or `Nothing`.)

The function that we need is a method of the `PMatch` typeclass. For the time being, we will ignore the details of implementation and only look at the type:

```hs
pmatch :: 
  forall (a :: PType) (b :: PType) (s :: S).
  ( PlutusType a       -- `a` has to be a `PlutusType` instance. 
  ) => Term s a        -- Given a `Term` tagged with `a` and
  -> (a s -> Term s b) -- a continuation from `a s` to a Term s b`, 
  -> Term s b          -- produce a `Term s b`.
```

The annotation of the second argument deserves some focus; the second argument has its type displayed as `(a s -> Term s b)`. First, recall that `a` is declared to have kind `PType`, and `PType` is a kind synonym for `S -> Type`. Thus, since `s` has kind `S`, we have that `a s` has the _kind_ `Type`. That is, it is a regular Haskell type.

What this means, in practice, is that `pmatch` matches on the possible values of the _result_ of evaluating a `Term s a` -- specifically, it matches on _values_ of a _type_ that has _kind `PType`_ -- and branches accordingly. The second argument to `pmatch` is a _continuation_; it determines how the program continues once `pmatch` has done its work.

We have already introduced a type with kind `PType` suitable for branching: `PMaybe`. Here is an example:

```haskell
{- | This function takes in a Haskell-level `PMaybe s` value (specifically, _not_ a `Term`)
     and returns a `Term` depending on the Haskell-level pattern match on `PMaybe`s data
     constructors.
-}
continuation :: PMaybe a s -> Term s PBool
continuation = \case
  PJust _ -> pcon PTrue
  PNothing -> pcon PFalse

{- | A Haskell-level `isJust` on Plutarch `Term`s. `pmatch` can match on
     the possibilities of `PJust _` or `PNothing` being the result of an evaluated
     `Term`.
-}
hisJust :: Term s (PMaybe a) -> Term s PBool
hisJust x = pmatch x continuation

-- | A Plutarch-level `isJust`
pisJust :: Term s (PMaybe a :--> PBool)
pisJust = plam hisJust
```

Readers should note that this is not the most ergonomic way to deal with pattern matching (Plutarch provides two versions of `do` syntax), but it _is_ how the more ergonomic methods work under the hood.
