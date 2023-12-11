<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Introduction (hf) where 
import Plutarch.Prelude
```

</p>
</details>
> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high-quality documentation for Plutarch users!

# Overview

Plutarch is an eDSL in Haskell for writing on-chain scripts for Cardano. With some caveats, Plutarch is a [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (or STLC). Writing a script in Plutarch allows us to leverage the language features provided by Haskell while retaining the ability to compile to compact Untyped Plutus Core (or UPLC, which is an untyped lambda calculus).

When we talk about "Plutarch scripts," we are referring to values of type `Term (s :: S) (a :: PType)`. `Term` is a `newtype` wrapper around a more complex type, the details of which Plutarch end-users can ignore. A `Term` is a typed lambda term; it can be thought of as representing a computation that, if successfully evaluated, will return a value of type `a`.

The two type variables of the `Term s a` declaration have particular kinds:

- `s :: S` is like the `s` of `ST s a`. It represents the computation context in a manner that mimics mutable state while providing a familiar functional interface. Sections 1 through 4 of \[[1](#references)] give an accessible introduction to how this works. `s` is never instantiated with a concrete value; it is merely a type-level way to ensure that computational contexts remain properly encapsulated (i.e., different state threads don't interact). For more in-depth coverage of this and other eDSL design principles used in Plutarch, see \[[2](#references)].
- `a :: PType` is short-hand for "Plutarch Type". We prefix these types with a capital `P`, such as `PInteger`, `PBool`, and so forth. _Tagging_ a `Term` with a `PType` indicates the type of the `Term`'s return value. Doing this allows us to bridge between the simple type system of Plutarch and the untyped UPLC.

Note that we _should not_ think of a type of kind `PType` as carrying a value; it is a tag for a computation that may produce a value. For instance, the definition of `PInteger` is simply

```hs
data PInteger s
```

That is, there are no data constructors. _If_ a value of type `Term s PInteger` successfully executes the computation within context `s`, the value _computed_ will be an integer. We will never encounter values such as `y :: PInteger; y = 3`; they simply do not exist. While readers new to Plutarch may need some time to fit this into their mental model, it is a crucial distinction to keep in mind.

> For brevity, we will say a "value of type `Term s a` will evaluate to (...)". This phrase will carry two implicit notions: one, that `Term s a` represents a computation executed in the context `s`; two, evaluating `Term s a` is not guaranteed to succeed.

In brief, when writing Plutarch scripts, we have a few tasks:

- A.) Defining _Plutarch Types_ (or _`PType`s_). We prefix these types with a capital `P`, such as `PInteger`, `PMaybe a`, `PBool`, and so forth. As previously mentioned, these form the "tags" for Plutarch `Term`'s, representing the type of the result of compiling and evaluating a Plutarch Script.
- B.) Working with _Plutarch `Terms`_, which are values of the type `Term (s :: S) (a :: PType)`. These are the Plutarch scripts themselves, from which we build up more complex scripts before compiling and executing them on-chain.
- C.) Writing Haskell-level functions _between Plutarch Terms_ (i.e., with types like `Term s a -> Term s b`). Doing so allows us to leverage Haskell's language features and ecosystem.
- D.) Efficiently Converting the functions from _(C.)_ to _Plutarch-level functions_, which are of the type `Term s (a :--> b)`. We can _directly_ convert the functions from (C.) to Plutarch-level functions at the most naive level using `plam`. Additional Plutarch utilities provide for optimization opportunities.
- E.) Compiling and executing the functions from _(D.)_, targeting UPLC for on-chain usage.

As a preview, the bridge Plutarch provides between Haskell and UPLC looks something like this:

    ------------------------------------------------------
    |                  *Haskell World*                   |
    ------------------------------------------------------
    | Values with types like `Bool`, `Integer`, `Maybe a`|
    | and regular Haskell functions like a -> b          |
    ------------------------------------------------------
                               ^                     |
     (functions like `plift`)--|                     |--(functions like `pconstant` or `plam`)
                               |                     |
                               |                     v         (`pcon`)
    -------------------------------------------------------    |                     -------------------------------------------------------
    |                  *Plutarch Term World*              | <----------------------- |                  *Plutarch Type World*              |
    -------------------------------------------------------                          -------------------------------------------------------
    | STLC terms; constants like `Term s PInteger` and    | -----------------------> | Types like `PInteger`, `PMaybe a`                   |
    | lambdas like `Term s (PInteger :--> PBool)`         |        |                 |                                                     |
    -------------------------------------------------------        (`pmatch`)        -------------------------------------------------------
                                  |
                                  |
                                  |--(`compile`)
                                  |
                                  |
                                  v
    -------------------------------------------------------
    |                    *UPLC World*                     |
    -------------------------------------------------------
    | Untyped lambda calculus terms. Values of type `Data`|
    |                                                     |
    -------------------------------------------------------

Further, you may notice two general categories of functions in Plutarch: "Haskell-level" functions between terms, and "Plutarch-level"
functions _as_ lambda terms. By convention, we will prefix the Haskell-level functions with `h` and the Plutarch-level lambdas
with `p`, for example

```haskell
-- This example is listed here as a preview; the unfamiliar parts will
-- be detailed below.

-- A Plutarch-level lambda term
pf :: Term s (a :--> b :--> c)
pf = undefined

-- Recovering a Haskell level function from a Plutarch level function
hf :: Term s a -> Term s b -> Term s c
hf x y = pf # x # y
```

Note that `pf` is truly just a Plutarch `Term` and should not be treated specially.

The remainder of this document cover the bridge between Haskell and Plutarch at a high level. It will _not_ cover all techniques necessary to write production-ready scripts. Nor will it cover the bridge between Plutarch and UPLC beyond the minimum. Nonetheless, it should provide sufficient background to prepare the reader for further study.

Sections:-

- [Untyped Plutus Core (UPLC)](./Introduction/Untyped%20Plutus%20Core.md)
- [Plutarch Types](./Introduction/Plutarch%20Types.md)
- [Plutarch `Term`s](./Introduction/Plutarch%20Terms.md)
  - [Plutarch Constant `Term`s](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md)
    - [Static building of constant `Term`s with `pconstant`](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#static-building-of-constant-terms-with-pconstant)
    - [Dynamic building of constant `Term`s with `pcon`](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#dynamic-building-of-constant-terms-with-pcon)
    - [Overloaded literals](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#overloaded-literals)
    - [Helper functions](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#helper-functions)
  - [Lambdas; Plutarch-level Function `Term`s.](./Introduction/Plutarch%20Terms/Plutarch%20Lambdas.md#lambdas-plutarch-level-function-terms)
    - [Function Application](./Introduction/Plutarch%20Terms/Plutarch%20Lambdas.md#function-application)
- [Pattern matching constant `Term`s with `pmatch`.](./Introduction/Pattern%20matching.md)
- [Strictness and Laziness; Delayed Terms and Forcing](./Introduction/Delay%20and%20Force.md)

# References

- \[1][Lazy Functional State Threads, by John Launchbury and Simon L Peyton Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf)
- \[2][Unembedding Domain-Specific Languages, by Robert Atkey, Sam Lindley, and Jeremy Yallop](https://bentnib.org/unembedding.pdf)
- \[3][Matt Parson: Basic Type Level Programming in Haskell](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html)
