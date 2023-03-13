<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE RankNTypes #-}
module Plutarch.Docs.DataAndScottEncoding (nothing, just, foo) where 

import Prelude (Integer, (+))
```

</p>
</details>

# Data encoding and Scott encoding

In Plutus Core, there are really two (conflicting) ways to represent non-trivial ADTs: [`Constr`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) data encoding, or Scott encoding. You should use only one of these representations for your non-trivial types.

> Aside: What's a "trivial" type? The non-data builtin types! `PInteger`, `PByteString`, `PBuiltinList`, `PBuiltinPair`, and `PMap` (actually just a builtin list of builtin pairs). It's important to note that [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) (`Constr` or otherwise) is also a builtin type.

## Data encoding

`Constr` data is essentially a sum-of-products representation. However, it can only contain other `Data` values (not necessarily just `Constr` data, could be `I` data, `B` data etc.) as its fields. Plutus Core famously lacks the ability to represent functions using this encoding, and thus `Constr` encoded values simply cannot contain functions.

> Note: You can find out more about the deep details of `Data`/`BuiltinData` at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

With that said, `Data` encoding is _ubiquitous_ on the chain. It's the encoding used by the ledger api types, it's the type of the arguments that can be passed to a script on the chain etc. As a result, your datums and redeemers _must_ use data encoding.

## Scott encoding

On the opposite (and conflicting) end, is Scott encoding. [The internet](https://crypto.stanford.edu/~blynn/compiler/scott.html) can explain Scott encoding way better than I can. But I'll be demonstrating Scott encoding with an example anyway.

Firstly, what good is Scott encoding? Well it doesn't share the limitation of not being able to contain functions! However, you cannot use Scott encoded types within, for example, your datums and redeemers.

Briefly, Scott encoding is a way to represent data with functions. The Scott encoded representation of `Maybe a` would be:

```hs
(a -> b) -> b -> b
```

`Just 42`, for example, would be represented as this function:

```hs
\f _ -> f 42
```

Whereas `Nothing` would be represented as this function:

```hs
\_ n -> n
```

We covered construction. What about usage/deconstruction? That's also just as simple. Let's say you have a function, `foo :: Maybe Integer -> Integer`, it takes in a Scott encoded `Maybe Integer`, and adds `42` to its `Just` value. If it's `Nothing`, it just returns `0`.

```haskell
type Maybe a = forall b. (a -> b) -> b -> b

just :: a -> Maybe a
just x = \f _ -> f x

nothing :: Maybe a
nothing = \_ n -> n

foo :: Maybe Integer -> Integer
foo mb = mb (\x -> x + 42) 0
```

How does that work? Recall that `mb` is really just a function. Here's how the application of `f` would work:

```hs
foo (just 1)
foo (\f _ -> f 1)
(\f _ -> f 1) (\x -> x + 42) 0
(\x -> x + 42) 1
43
```

```hs
foo nothing
foo (\_ n -> n)
(\_ n -> n) (\x -> x + 42) 0
0
```

Neat!

This is the same recipe followed in the implementation of `PMaybe`. See its [PlutusType impl](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)!
