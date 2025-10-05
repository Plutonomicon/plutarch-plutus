<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Recursion (pfac, pfac') where 
import Plutarch.Prelude
```

</p>
</details>

# Recursion

To emulate recursion in [UPLC (untyped Plutus Core)][uplc], we have to use the
[Z combinator][z-combinator]. This is the strict analog to the more famous [Y
combinator][y-combinator], but effectively does the same thing.

Plutarch provides several versions of the Z combinator: each version does
essentially the same thing, but has different tradeoffs regarding performance
versus script size. Briefly:

* `pfixHoisted` produces the smallest code, but has the least performance.
* `pfix` produces slightly larger code, but performs better.
* `pfixInline` produces the largest code, but has the best performance.

More precisely, let's consider the type of `pfixHoisted`:

```hs
pfixHoisted :: Term s (((a :--> b) :--> (a :--> b)) :--> (a :--> b))
```

The type is a little intimidating. You can see this as analogous to the Haskell
`fix` function:

```hs
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
```

The first argument to `fix` (and technically, `pfixHoisted`) is the 'self', or
the function you want to recurse with.

To demonstrate how we could use `pfixHoisted`, we implement a Plutarch-level
factorial function:

```haskell
pfac :: Term s (PInteger :--> PInteger)
pfac = pfixHoisted #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

Note how `f` takes in a `self` and just recurses on it. All you have to do is 
create a Plutarch-level function by using `plam` on `f` and `pfixHoisted` 
the result - and that `self` argument will be taken care of for you.

`pfix` and `pfixInline` have slightly different signatures to `pfixHoisted`, as
they are parameterized at the Haskell level:

```hs
-- pfixInline's signature is identical
pfix :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
```

We can see once again the similarity to the Haskell-level `fix`. The difference
is that `pfix` is secretly a code generator: its argument is a function
parameterized by a 'self' operation, and its result is that same function with
'self' handled. This is the reason why `pfix` and `pfixInline` can produce
faster code at the cost of larger code: we have access to what we are
transforming.

To see the difference in use, here is `pfac` implemented with `pfix` instead:

```haskell
pfac' :: Term s (PInteger :--> PInteger)
pfac' = pfix f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s (PInteger :--> PInteger)
    f self = plam $ \n -> pif (n #== 1) n $ n * (self #$ n - 1)
```

We can see that the primary difference is where `plam` is used: with
`pfixHoisted`, we use it next to the call, while with `pfix`, we use it in the
`where` bind for whatever we want to fixpoint. 

So, which one should you use? In general, `pfix` gives the best tradeoff between
script size and performance. If you really need the smallest possible scripts,
`pfixHoisted` is the right choice; on the other hand, if performance trumps
concerns of size, `pfixInline` is the go-to. Ultimately, we recommend
benchmarking to be sure, as in some cases, the improvements may not be major or
worth it, but the costs are _always_ there.

[uplc]: https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md
[z-combinator]: https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed-point_combinator
[y-combinator]: https://stackoverflow.com/questions/93526/what-is-a-y-combinator 
