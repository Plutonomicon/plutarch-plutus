# Do syntax with `QualifiedDo` and `Plutarch.Monadic`

There's another way of having `do` syntax. Though this one doesn't use a lawful monad. Instead, it uses `QualifiedDo` - and therefore requires GHC 9.

The `Plutarch.Monadic` module exports `>>=`, `>>`, and `fail` functions suitable to be used with `QualifiedDo`.

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> P.do
  PSpending _ <- pmatch x
  ptrace "matched spending script purpose"
  pconstant ()
```

In essence, `P.do { x; y }` simply translates to `x y`; where `x :: a -> Term s b` and `y :: a`.

Similarly, `P.do { y <- x; z }` translates to `x $ \case { y -> z; _ -> ptraceError <msg> }`; where `x :: (a -> Term s b) -> Term s b`, `y :: a`, and `z :: Term s b`. Of course, if `y` is a fully exhaustive pattern match (e.g. singular constructor), the extra `_ -> ptraceError <msg>` case will not be generated at all and you'd simply get `x $ \y -> z`.

Finally, `P.do { x }` is just `x`.