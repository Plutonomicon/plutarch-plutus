<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Hoisting (hor, (#||)) where 

import Plutarch.Prelude hiding ((#||))
import Plutarch.Bool (pif')
```

</p>
</details>

# Hoisting, metaprogramming, and fundamentals

Plutarch has a two-stage compilation process. First GHC compiles our code, then our code generates an _AST_ of our Plutus script, which is then serialized using `compile`.

The important thing to note, is that when you have a definition like:

```hs
x :: Term s PInteger
x = something complex
```

Any use of `x` will inline the **full definition** of `x`. `x + x` will duplicate `something complex` in the AST. To avoid this, you should [use `plet` in order to avoid duplicate work](./../Tricks/Don't%20duplicate%20work.md). Do note that this is **strictly evaluated, and hence isn't always the best solution.**

There is however still a problem: what about top-level functions like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this is error-prone. To do this perfectly means that each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.

To solve this problem, Plutarch supports _hoisting_. Hoisting only works for _closed terms_, that is, terms that don't reference any free variables (introduced by `plam`).

Hoisted terms are essentially moved to a top-level `plet`, i.e. it's essentially common sub-expression elimination. Do note that because of this, your hoisted term is **also strictly evaluated**, meaning that you _shouldn't_ hoist non-lazy complex computations (use [`pdelay`](./../Introduction/Delay%20and%20Force.md) to avoid this).

In general, you should use `phoistAcyclic` on every top level function:

```hs
foo = phoistAcyclic $ plam $ \x -> <something complex>
```

As long as the Plutarch lambda you're hoisting does not have [free variables](https://wiki.haskell.org/Free_variable) (as Plutarch terms), you will be able to hoist it!

## Hoisting Operators

For the sake of convenience, you often would want to use operators - which must be Haskell level functions. This is the case for `+`, `-`, `#==` and many more.

Choosing convenience over efficiency is difficult, but if you notice that your operator uses complex logic and may end up creating big terms - you can trivially factor out the logic into a Plutarch level function, hoist it, and simply apply that function within the operator.

Consider "boolean or":

```haskell
hor :: Term s PBool -> Term s PBool -> Term s PBool
x `hor` y = pif x (pconstant True) $ pif y (pconstant True) $ pconstant False
```

You can factor out most of the logic to a Plutarch level function, and apply that in the operator definition:

```haskell
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

por :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # pdelay (pconstant True) # y
```

In general the pattern goes like this:

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = f # x # y

f :: Term s (x :--> y :--> z)
f = phoistAcyclic $ plam $ \x y -> <complex computation>
```

(OR, simply inlined)

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = (\f -> f # x # y) $ phoistAcyclic $ plam $ \x y -> <complex computation>
```

> Note: You don't even need to export the Plutarch level function or anything! You can simply have that complex logic factored out into a _hoisted, internal Plutarch function_ and everything will work just fine!
