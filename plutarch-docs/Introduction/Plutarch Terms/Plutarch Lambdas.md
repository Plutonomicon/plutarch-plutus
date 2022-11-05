<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Plutarch.Docs.PlutarchLambdas (pid, pid') where 
import Plutarch.Prelude
```

</p>
</details>
# Lambdas; Plutarch-level Function `Term`s.

Lambdas are the second form of Plutarch `Term`s. Lambda terms are represented at the type level by the infix type constructor `:-->`; a value of type `Term s (a :--> b)` evaluates to a function that takes a value of type `a` and produces a value of type `b`.

You can create Plutarch lambda `Term`s by applying the `plam` function to a Haskell-level function that works on Plutarch terms. The true type of `plam` itself is unimportant to end-users of Plutarch, but it should be thought of as

```hs
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

To create the identity function as a Plutarch lambda, we would thus use:

```haskell
-- | Haskell-level `id` function specialized to the `Term s a` type``
termId :: Term s a -> Term s a
termId x = x

-- | Plutarch-level `id` lambda
pid :: Term s (a :--> a)
pid = plam termId

-- | Equivalently:
pid' :: Term s (a :--> a)
pid' = plam $ \x -> x
```

Notice the type. A Plutarch lambda `Term` uses the `:-->` infix operator to encode a function type. So in the above case, `pid` is a Plutarch level function that takes a type `a` and returns the same type. As one would expect, `:-->` is right-associative, and things curry like a charm.

Guess what this Plutarch level function does:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
```

It takes in an integer, a string, and a type `a` and returns the same type `a`. Notice that the types are all of kind `PType`. This means that when faced with filling out the gap:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

We know that the argument to `plam` here is a Haskell function `g` with type `Term s PInteger -> Term s PString -> Term s a -> Term s a`.

## Function Application

Once we construct a Plutarch lambda `Term` using `plam`, it is rather useless unless we apply it to an argument. Plutarch provides two operators to do so

```hs
{- |
  High precedence infixl function application, to be used like
  function juxtaposition. e.g.:

  >>> f # x # y
  Conceptually: f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
infixl 8 #

{- |
  Low precedence infixr function application, to be used like
  `$`, in combination with `#`. e.g.:

  >>> f # x #$ g # y # z
  Conceptually: f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
infixr 0 #$
```

The types of each operator match our intuition. Applying a lambda `Term` to a `Term` (tagged with the `PType` of the domain of the lambda) produces a `Term` (tagged with the `PType` of the codomain.).
