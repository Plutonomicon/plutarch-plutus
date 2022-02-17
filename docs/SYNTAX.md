This document describes the syntax of Plutarch through code examples.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

-   [Constants](#constants)
    -   [Static building with `pconstant` and `pconstantData`](#static-building-with-pconstant-and-pconstantdata)
    -   [Dynamic building with `pcon`](#dynamic-building-with-pcon)
    -   [Overloaded literals](#overloaded-literals)
    -   [Miscellaneous](#miscellaneous)
-   [Lambdas](#lambdas)
-   [Delayed terms and Forcing](#delayed-terms-and-forcing)

</details>

A Plutarch script is a `Term`. This can consist of-

> Jack: replace - with :

# Constants

There are several ways of building Plutarch constants. We discuss the primary ones below:-

> Jack: delete -

## Static building with `pconstant` and `pconstantData`

Plutarch constant terms can be built directly from Haskell synonyms using `pconstant` (requires [`PConstant`/`PLift`](#pconstant--plift) instance). `pconstant` always takes in a regular Haskell value to create its Plutarch synonym.

```hs
import Plutarch.Prelude

-- | A plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pconstant True
```

Similarly, `PAsData` constant terms can be built using `pconstantData`. If you want to build a `Term s (PAsData PBool)` from a Haskell boolean - you can use `pconstantData True`.

> Jack: replace - with ,

> Aside: If you've already read through \[`PIsData`]\(TODO: LINK) and \[`PAsData`]\(TODO: LINK), you might know that `pdata . pconstant` would achieve the same thing as `pconstantData`. But it won't actually be as efficient! See, `pconstantData` builds a constant directly - wheras `pdata` _potentially_ dispatches to a builtin function call. Also see: \[Prefer statically building constants]\(TODO: LINK).

## Dynamic building with `pcon`

Plutarch constant terms can also be built from Plutarch terms within other constructors using `pcon` (requires \[`PlutusType`/`PCon`]\(TODO: LINK) instance)-

```haskell
import Plutarch.Prelude

-- | Create a plutarch level optional value from given value.
f :: Term s (a :--> PMaybe a)
f = plam $ \x -> pcon $ PJust x
-- Note that 'PMaybe' has a 'PlutusType' instance.
```

> `PMaybe` declaration: `data PMaybe a s = PJust (Term s a) | PNothing`

Notice that `pcon` actually takes in a Plutarch type to create a Plutarch term. In particular, `PJust x`, where `x :: Term s a`, has type `PMaybe a s`.

```hs
-- Example
> :t x
Term s PInteger
> :t PJust x
PMaybe PInteger s
> :t pcon (PJust x)
Term s (PMaybe PInteger)
```

Thus, within the `f` definition above, `pcon` has type `PMaybe a s -> Term s (PMaybe a)`. Similarly, `pcon PNothing` would yield `forall x. Term s (PMaybe x)`, since `PNothing` has type `PMaybe x s`.

```hs
-- Example
> :t PNothing
PMaybe a s
> :t pcon PNothing
Term s (PMaybe a)
```

## Overloaded literals

Just like in the Haskell world, certain literals are overloaded to help build Plutarch constants.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Plutarch.Prelude

-- | A plutarch level integer. Its value is 1, in this case.
x :: Term s PInteger
x = 1

-- | A plutarch level string (this is actually 'Text'). Its value is "foobar", in this case.
y :: Term s PString
y = "foobar"
```

## Miscellaneous

Finally, other miscellaneous functions provided by Plutarch also sometimes build constants-

> Jack: Replace - with :

```haskell
import qualified Data.ByteString as BS
import Plutarch.Prelude

-- | A plutarch level bytestring. Its value is [65], in this case.
x :: Term s PByteString
x = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.
```

# Lambdas

You can create Plutarch level lambdas by apply `plam` over a Haskell level lambda/function.

> Jack: applying `plam` to a Haskell level function.

```haskell
pid :: Term s (a :--> a)
pid = plam $ \x -> x
```

The identity function! Notice the type. A Plutarch level lambda uses the funny arrows `:-->` to encode a function type. In the above case, `pid` is a Plutarch level function that takes a type `a`, and returns the same type - `a`. As one would expect, `:-->` is right associative and things curry like a charm (at least, they should).

> Jack: same type: `a`.

Guess what this Plutarch level function does-

> Jack: 'does: '

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
```

That's right! It takes in an integer, a string, and a type `a` and returns the same type `a`. Notice that all of those types are Plutarch level types.

This is the type of the Haskell level function, `plam`-

> Jack: `plam`:

```haskell
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

(That's actually a lie! But we are going to ignore the _real_  `plam` type for simplicity)

It just converts a Haskell level function, which operates on purely Plutarch terms, into a Plutarch level function.

This means that when faced with filling out the gap-

> Jack: gap:

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

You know that the argument to `plam` here will just be a Haskell function that takes in - `Term s PInteger`, `Term s PString`, and `Term s a` (in  that order), and spits out a `Term s a` back.

> Jack: takes in:

> Jack: consider removing the first comma (...`PString` and `Term s a`, and spits...)

# Delayed terms and Forcing

You can use `pdelay` to create a delayed term and `pforce` to force on it. These will help you emulate laziness in an otherwise strict language. More details at \[Delay and Force]\(TODO: LINK).

> Jack: Replace 'force on it' with something more descriptive e.g. 'call it' or 'run it'.

```hs
pdelay :: Term s a -> Term s (PDelayed a)

pforce :: Term s (PDelayed a) -> Term s a
```
