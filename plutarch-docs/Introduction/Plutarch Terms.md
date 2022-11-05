# Plutarch `Term`s

Plutarch `Term`s are terms in the sense of simply-typed lambda calculus terms. In a lambda calculus, we can construct terms as either "constants" or "lambdas," and terms can either be "open" (having free variables) or "closed" (having no free variables). We compose Plutarch `Term`s to build up increasingly complex computations. Once all free variables are eliminated from a `Term` (making it a `Closed Term`), we can compile it using the eponymous function from the `Plutarch` module:

```hs
-- | Closed term is a type synonym
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

-- | Compile operates on closed terms to produce usable UPLC scripts.
compile :: ClosedTerm a -> Script
```

`Term`s are constructed from Haskell values and are tagged with `PType`s.

- [Plutarch Constant `Term`s](./Plutarch%20Terms/Plutarch%20Constants.md)
  - [Static building of constant `Term`s with `pconstant`](./Plutarch%20Terms/Plutarch%20Constants.md#static-building-of-constant-terms-with-pconstant)
  - [Dynamic building of constant `Term`s with `pcon`](./Plutarch%20Terms/Plutarch%20Constants.md#dynamic-building-of-constant-terms-with-pcon)
  - [Overloaded literals](./Plutarch%20Terms/Plutarch%20Constants.md#overloaded-literals)
  - [Helper functions](./Plutarch%20Terms/Plutarch%20Constants.md#helper-functions)
- [Lambdas; Plutarch-level Function `Term`s.](./Plutarch%20Terms/Plutarch%20Lambdas.md#lambdas-plutarch-level-function-terms)
  - [Function Application](./Plutarch%20Terms/Plutarch%20Lambdas.md#function-application)
