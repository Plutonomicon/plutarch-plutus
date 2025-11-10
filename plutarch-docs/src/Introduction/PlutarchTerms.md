# Plutarch `Term`s

Plutarch `Term`s are terms in the sense of simply-typed lambda calculus terms. In a lambda calculus, we can construct terms as either "constants" or "lambdas," and terms can either be "open" (having free variables) or "closed" (having no free variables). We compose Plutarch `Term`s to build up increasingly complex computations. Once all free variables are eliminated from a `Term` (making it a `Closed Term`), we can compile it using the eponymous function from the `Plutarch` module:

```hs
-- | Compile operates on closed terms to produce usable UPLC scripts.
compile :: forall (a :: S -> Type). Config -> (forall (s :: S). Term s a) -> Either Text Script
```

`Term`s are constructed from Haskell values and are tagged with `S -> Type`s.

- [Plutarch Constant `Term`s](./PlutarchTerms/PlutarchConstants.md)
  - [Static building of constant `Term`s with `pconstant`](./PlutarchTerms/PlutarchConstants.md#static-building-of-constant-terms-with-pconstant)
  - [Dynamic building of constant `Term`s with `pcon`](./PlutarchTerms/PlutarchConstants.md#dynamic-building-of-constant-terms-with-pcon)
  - [Overloaded literals](./PlutarchTerms/PlutarchConstants.md#overloaded-literals)
  - [Helper functions](./PlutarchTerms/PlutarchConstants.md#helper-functions)
- [Lambdas; Plutarch-level Function `Term`s.](./PlutarchTerms/PlutarchLambdas.md#lambdas-plutarch-level-function-terms)
  - [Function Application](./PlutarchTerms/PlutarchLambdas.md#function-application)
