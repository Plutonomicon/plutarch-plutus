<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE QualifiedDo #-}

module Plutarch.Docs.QDo (f) where 
import Plutarch.Api.V1.Contexts
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

```

</p>
</details>

# Do syntax with `QualifiedDo` and `Plutarch.Monadic`

In `ghc92`+ we can use do notation without using the `Monad` instances for some type by using overloaded syntax. 
This overloaded syntax is provided by the `-XQualifiedDo` extension

The `Plutarch.Monadic` module exports `>>=`, `>>`, and `fail` functions suitable to be used with `QualifiedDo`.

```haskell
f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> P.do
  PSpending _ <- pmatch x
  ptrace "matched spending script purpose"
  pconstant ()
```

In essence, `P.do { x; y }` simply translates to `x y`; where `x :: a -> Term s b` and `y :: a`.

Similarly, `P.do { y <- x; z }` translates to `x $ \case { y -> z; _ -> ptraceError <msg> }`; where `x :: (a -> Term s b) -> Term s b`, `y :: a`, and `z :: Term s b`. 
Of course, if `y` is a fully exhaustive pattern match (e.g. singular constructor), the extra `_ -> ptraceError <msg>` case will not be generated at all and you'd simply 
get `x $ \y -> z`. 

> Note: if a pattern match fails, e.g. when using the `PJust a <- ...` syntax, it will use `Plutarch.Monadic`'s implementation of `fail`

Finally, `P.do { x }` is just `x`.
