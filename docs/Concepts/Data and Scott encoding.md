# Data encoding and Scott encoding

In Plutus Core, there are really two (conflicting) ways to represent non-trivial ADTs: [`Constr`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) data encoding or Scott encoding. You should use only one of these representations for your non-trivial types.

> Aside: What's a "trivial" type? The non-data builtin types! `PInteger`, `PByteString`, `PBuiltinList`, `PBuiltinPair`, and `PMap` (actually just a builtin list of builtin pairs). It's important to note that [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) (`Constr` or otherwise) is also a builtin type.

## Data encoding

`Constr` data is a sum-of-products representation. It is defined as

```hs
data Data =
      Constr Integer [Data] 
      ^ -- `Integer`` refers to the constructor index,
        -- `[Data]` is the fields of the constructor
    | Map [(Data, Data)] 
      ^ -- A list-of-pairs repesentation of a map, 
        -- add as a constructor for convenience
    | List [Data]
      ^ -- A list of `Data` values
    | I Integer
      ^ -- Primitive Integer values
    | B BS.ByteString
      ^ -- Primitive bytestring values
```

`Constr` can only contain other `Data` values (i.e., `I` data, `B` data, or recursively nested `Constr`s, `Map`s, or `Lists`) as its fields. 

Note that Plutus Core lacks the ability to represent functions using this encoding, and thus values encoded as `Constr` cannot contain functions.

> Note: You can find out more about the deep details of `Data`/`BuiltinData` at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

`Data` encoding is _ubiquitous_ in ledger API types. It is the type of the arguments that are passed to a script on the chain (including the datum, redeemer, and script context.) As a result, your datums and redeemers _must_ use `Data` encoding.

## Scott encoding 

Scott encoding is an alternative way to encode ADTs in Plutus. Scott encoding uses functions (in the lambda calculus) to
encode types.

The idea behind Scott encoding is similar to pattern matching: a given value with a given constructor of a given type 
brings into scope specific values that can be used to determine how to continue execution of the program. This is a
verbose way of putting it, but captures the key idea: determining "what to do" when a program encounters a given
constructor is all we need.

We will now break this concept down in detail by examining a hypothetical program. This program knows that it will
encounter a value of type `Maybe a` at a specific point in it's execution. When it does, it has to decide how to 
continue execution.

We'll start with the  `Just a` case. The `Just a` value has the `Just` constructor of the `Maybe a` type,
and brings into scope the value `a`. In order to decide what to do next, the program has to _do_ something 
with the `Just a`; it needs to apply a function to it. Let's call this function `f`. We want to the semantics of the 
program to be as follows: "I've be given a `Just a`. The next step is to continue the program by running a computation
represented by the function `f`. Since I have an `Just a`, I can bring an the `a` value -- and only that value -- into 
scope for `f`. Thus, `f :: a --> restOfTheProgram`, and `f a` is how the program should continue." 

With the `Maybe` type, there is a second constructor, `Nothing`, that we need to consider in order to make the program
total; our hypothetical program needs another function, `g`, to handle this case. Thus, at the point in the program
where `f` was previously dispatched to handle the `Just a` case, we'll now include the `Nothing` case with the following
semantics: "I've just be given a `Nothing`. The next step is to continue the program by running a computation represented
by the function `g`. Since I was given a `Nothing`, there are no additional values that I can bring into scope. Thus, 
`g :: restOfTheProgram`". 

`f` and `g` are called _continuations_, for the obvious reason: they repesentation how the program continues beyond
a certain point of execution.

In our regular pattern matching syntax, we might then represent this part of the program as

```hs
f :: a --> restOfTheProgram
g :: restOfTheProgram

thisPartOfTheProgram :: Maybe a -> restOfTheProgram
thisPartOfTheProgram (Just a) = f a
thisPartOfTheProgram Nothing = g
```

but the key constitutents of this computation are really:

- The program has encounter a `Maybe`, which has two constructors
- The `Just a` constructor brings into scope a value `a` and is handled by the computation `f a`
- The `Nothing` constructor brings into scope no values and is handled by the computation `g`.

With a little bit of mental stretching, it is beginning to appear that it __should be possible_ 
(even if the details are still fuzzy) to bundle these constituents up into a type `ScottEncodedMaybe a`
that can give us the semantics we want. 

Let's suppose that we have a value of this type, `m`. In order to do something with
it, we need two functions, `f :: a --> restOfTheProgram` and `g :: restOfTheProgram` that tell the
program how to continue execution depending on whether we have an `m` that __repesents the concept of_
"`Just a`" or an `m` that __represents the concept of_ `Nothing`. The phrase
"_represents the concept of_" is used because `m` is _not_ of type `Maybe a`, but it _can_ be used to
achieve the same semantics.

So, really, `m` is a function that "has a '`Maybe`' value, and is waiting to be told how to continue." 
The `m` on it's own can't _do_ anything; it needs `f` and `g` to tell it what to do. 
So `m f g` represents a "computation that has a `Maybe` value and knows what to do with it to 
continue the program." This means that `m f g :: restOfTheProgram`.

In turn, this means `m` has the type:

```hs
m :: (a -> restOfTheProgram) -> restOfTheProgram -> restOfTheProgram
{-   \____________________/     \_______________/
            |                          |
     this is `f`, dispatched      this is `g`, 
     in the `Just` case          dispatched in the
                                 `Nothing` case`
-}

```

This is the Scott encoding of "Maybe":

```hs
type ScottEncodedMaybe a b = (a -> b) -> b -> b 
```


### Constructing functions to operate on Scott encoded values

Given a value `m` of type `(a -> b) -> b -> b`, a value of type `f :: a -> b`, and a value of type `g :: b`, it is not 
immediately clear to how to construct a function to operate on it. Let's return to our earlier example, 
(reproduced below, with `restOfTheProgram ~ b`):

```hs
f :: a --> b
g :: b

thisPartOfTheProgram :: Maybe a -> b
thisPartOfTheProgram (Just a) = f a
thisPartOfTheProgram Nothing = g
```

Next, we'll concretize this to have the following semantics: "If I'm given an Integer, add 42 to it. If not, return 0."
We then have:

```hs
f :: Integer -> Integer
f x = x + 42

g :: Integer
g = 0

foo :: Maybe Integer -> Integer
foo (Just a) = f a
foo (Nothing) = g
```

Our sticking point for translating this into the Scott encoded version is that with `foo`, we immediately have a 
function on hand that takes a `Maybe Integer`, makes a `a :: Integer` in-scope for the `Just` case, and returns
0 otherwise.

Now suppose that we wanted to write an identical function, operating on `ScottEncodedMaybe Integer` instead.
We would start with 

```hs
f :: Integer -> Integer
f x = x + 42

g :: Integer
g = 0

foo' :: ScottEncodedMaybe Integer -> Integer
```

We only have one option for `ScottEncodedMaybe Integer`, and that is `m`. So we must have `foo m = (...)`.
But `m` is merely a function waiting for two continuations, which we know are specified by `f` and `g`.
So to complete `foo`, we just apply `f` and `g` to `m`, and we get our full definition:


```hs
foo' :: ScottEncodedMaybe Integer -> Integer
foo' m = m f g

-- or equivalently

foo' m = m (\x -> x + 42) 0
```

### Constructing Scott encoded values

Now that we've constructed both `foo` and `foo'`, we need to apply each to an actual, optional value --
say, `Just 12` and the Scott encoded equivalent, respectively.

For `foo`, this is simple. We construct `Just 12` by combinging the constructor 
`Just :: Integer -> Maybe Integer` (remember: data constructors are just functions)
and have `foo (Just 12) == 54`. 

What about for `foo'`? What value of `m :: ScottEncodedMaybe Integer` gives us `foo' m == 54`, 
and how do we construct such a value (or equivalently, how do we define a constructor?)

The constructor, which we will call `scottEncodedJust`, should have the type 
`Integer -> ScottEncodedMaybe Integer`. So the definition of `scottEncodedJust` will begin by
pattern matching on an integer:

```hs
scottEncodedJust :: Integer -> ScottEncodedMaybe Integer
scottEncodedJust i = (...)
```

We know that a `ScottEncodedMaybe Integer` is a function that takes two functions (the continuations) --
we can thus introduce these as lambdas:

```hs
-- Recall `b ~ restOfTheProgram``

scottEncodedJust :: Integer -> ScottEncodedMaybe Integer
scottEncodedJust i = \(f :: Integer -> b) (g :: b) -> (...)
```

But in this case, we are positing that we _have_ been given an `Integer`, so the `f` continuation should be dispatched
(using the given `Integer` as an argument) and the `g` continuation can be thrown away. This gives the final definition
of the constructor as 

```hs
scottEncodedJust :: Integer -> ScottEncodedMaybe Integer
scottEncodedJust i = \(f :: Integer -> b) _ -> f i

-- or more compactly

scottEncodedJust :: Integer -> ScottEncodedMaybe Integer
scottEncodedJust i = \f _ -> f i

```

And thus `m = scottEncodedJust 12` -- or equivalently, `m = \f _ -> f 12`.

Correspondingly, we would have: 

```hs
scottEncodedNothing :: ScottEncodedMaybe Integer
scottEncodedNothing = \_ g -> g
```

### A Recipe for Scott Encoding

In Plutarch, Scott encoding of data types is handled through generic deriving. Thus, you will likely never need to
write a type like `ScottEncodedMaybe` by hand. This section is thus included for completeness only.

There is a basic recipe to convert from a regular Haskell type `MyType` to a Scott encoded version `SEMyType`. 
First, we recall that `SEMyType` is a _function type_. The recipe has three steps:

- All variables of `MyType` become type variables of `SEMyType`. 
  - `SEMyType` has an additional type variable indicating the result of the continuation; i.e. `restOfTheProgram`.
- Each data constructor of `MyType` becomes an argument to `SEMyType`; if `MyType` has `m` data constructors,
`SEMyType` will be a function type that takes `m` arguments. These functions are the _continuations_
  - `SEMyType` will always return a value of type `restOfTheProgram`.
- Each parameter of each data constructor in `MyType` becomes an argument to the arguments of `SEMyType`; if
the constructor `Foo` of `MyType` has `n` arguments, then the corresponding argument of the `SEMyType` will
be a function type taking `n` arguments.
  - These continuation functions always return `restOfTheProgram`

Thus, give a type such as 

```hs
data MyType a b c = 
  Foo a b
  | Bar Integer
  | Baz c
  | Qux
```

We would Scott encode this as 

```hs
-- Step one: three type variables, `a`, `b`, and `c``
data SEMyType a b c restOfTheProgram = (...)

-- Step two: four constructors that become our continuations
data SEMyType a b c restOfTheProgram =
  ( _ -> restOfTheProgram) -> -- Continuation in the `Foo` case
  ( _ -> restOfTheProgram) -> -- in the `Bar` case
  ( _ -> restOfTheProgram ) -> -- in the `Baz` case
  ( _ -> restOfTheProgram ) -> -- in the `Qux` case
  -> restOfTheProgram
  
-- Step three: constructor arguments become function arguments
data SEMyType a b c restOfTheProgram =
  (a -> b -> restOfTheProgram) -> -- Continuation in the `Foo` case
  (Integer -> restOfTheProgram) -> -- in the `Bar` case
  (c -> restOfTheProgram ) -> -- in the `Baz` case
  restOfTheProgram -> -- in the `Qux` case
  -> restOfTheProgram
```

The reverse recipe is straightforward:

- All type variables besides `restOfTheProgram` in `SEMyType` become type variables in `MyType`
- Each argument of `SEMyType` becomes a data constructor in `MyType`
- If an argument `SEMyType` is function, then the number of arguments becomes the number 
of arguments of the corresponding data constructor in `MyType`.


### Scott Encoding representation in Plutarch

> Note, this section does not cover how to derive a `PlutusType` Scott encoding repesentation for your types.
> See [PlutusType, PCon, and PMatch](../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding).

In Plutarch, Scott encoding is a way of encoding values to be use on-chain -- that is, Scott encoding is one of the 
options we have for the representation associed with a `PType` via the `PlutusType` type class.

As an example, printing a Scott encoded optional integer term (`Term s (PMaybe PInteger)`):

```hs
ghci> import Plutarch
ghci> justOne = pcon $ PJust (pconstant @PInteger 1)
ghci> printTerm justOne
"(program 1.0.0 (\\i0 -> \\i0 -> i2 1))"
```

The result of `printTerm justOne` displays the term using [De Bruijn indexing](https://en.wikipedia.org/wiki/De_Bruijn_index). 
Using De Bruijn indexing means that variables are specified by how many "layers of abstraction" need to be peeled back. In the 
above, this means that `i2` (for "index 2") refers to the lambda abstraction "two layers back":


```hs
                    `i2` refers to...
                   ___________
                  |           |
                  ^           |
(\\i0 -> \\i0 -> i2 1))"      |
   ^                          |
   |                          |
   |__________________________|
      the "second abstraction"
        back
```

We see that this agrees with out exploration above.
