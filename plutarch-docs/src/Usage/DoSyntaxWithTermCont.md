<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.TermCont (test, testC, foo) where
import Plutarch.LedgerApi.V3 (PScriptPurpose (PSpending))
import Plutarch.Prelude hiding (pmatchC, ptraceC)
```

</p>
</details>

# Do syntax with `TermCont`

> Note: The use of qualified do is preferred compared to the use of `TermCont` due to some shortcomings of the implementation
> of the `Monad` typeclass in `base`

Continuation functions like `pmatch`, `plet`, and `pletFields` aren't exactly the most convenient, are they? Fortunately,
`TermCont` makes it much easier to use. `TermCont` is the familiar
[`Cont`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html) monad, specialized for Plutarch terms.

`TermCont @b s a` essentially represents `(a -> Term s b) -> Term s b`. `a` being the input to the continuation, and `Term s b`
being the output. Notice the type application - `b` must have been brought into scope through another binding first.

Consider the snippet:

```haskell
test :: Term s (PScriptPurpose :--> PUnit)
test = plam $ \x -> pmatch x $ \case
  PSpending _ -> ptraceInfo "matched spending script purpose" $ pconstant ()
  _ -> ptraceInfoError "pattern match failure"
```

That's rather ugly! [`pmatch`](./../Typeclasses/PlutusType,PCon,PMatch.md) takes in a continuation as its second argument. Can we make this a bit more ergonomic?

```haskell
pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

ptraceInfoC :: Term s PString -> TermCont s ()
ptraceInfoC s = tcont $ \f -> ptraceInfo s (f ())

testC :: Term s (PScriptPurpose :--> PUnit)
testC = plam $ \x -> unTermCont $ do
  PSpending _ <- pmatchC x
  ptraceInfoC "matched spending script purpose"
  pure $ pconstant ()
```

How cool is that? You can use regular `do` syntax on the `TermCont` monad. All the continuations are flattened! Just remember to `unTermCont` the result.

Furthermore, this is very similar to the `Cont` monad - it just operates on Plutarch level terms. This means you can draw parallels to utilities and patterns
one would use when utilizing the `Cont` monad. Here's an example:

```haskell
-- | Terminate with given value on empty list, otherwise continue with head and tail.
nonEmpty :: Term s r -> PList a s -> TermCont @r s (Term s a, Term s (PList a))
nonEmpty x0 list = tcont $ \k ->
  case list of
    PSCons x xs -> k (x, xs)
    PSNil -> x0

foo :: Term s (PList PInteger :--> PInteger)
foo = plam $ \l -> unTermCont $ do
  (x, xs) <- nonEmpty 0 =<< tcont (pmatch l)
  pure $ x + plength # xs
```

`foo` adds up the first element of the given list with the length of its tail. Unless the list was empty, in which case, it just returns 0. It uses
continuations with the `do` syntax to elegantly utilize short circuiting!
