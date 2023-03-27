The Plutarch guide is your one-stop shop for getting up to speed on Plutarch!

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Overview](#overview)
  - [Compiling and Running](#compiling-and-running)
  - [Introduction and Basic Syntax](#introduction-and-basic-syntax)
  - [Practical Usage](#practical-usage)
  - [Concepts](#concepts)
  - [Typeclasses](#typeclasses)
  - [Working with Types](#working-with-types)
- [Examples](#examples)
- [Rules of thumb, Tips, and Tricks](#rules-of-thumb-tips-and-tricks)
- [Common Issues and Troubleshooting](#common-issues-and-troubleshooting)
- [Useful Links](#useful-links)

</details>

> Aside: Not interested in the details? Skip straight to [examples](#examples)!

# Overview

## Compiling and Running

- [Common Extensions and GHC options](./Run.md#common-extensions-and-ghc-options)
- [Evaluation](./Run.md#evaluation)

## Introduction and Basic Syntax

The [Introduction section](./Introduction.md) serves as a introduction to Plutarch's basic concepts and core syntactic constructs. It will help build a mental model of Plutarch, but is insufficient to write production-ready code.

- [Overview](./Introduction.md#overview)
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
- [References](./Introduction.md#references)

## Practical Usage

The [Usage section](./Usage.md) fills in the gaps left by the previous. It illustrates techniques that make Plutarch easier to work with.

- [Conditionals](./Usage/Conditionals.md)
- [Recursion](./Usage/Recursion.md)
- [Using the Plutarch Prelude](./Usage/Prelude%20mixin.md)
- [Do syntax with `TermCont`](./Usage/Do%20syntax%20with%20TermCont.md)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](./Usage/Do%20syntax%20with%20QualifiedDo.md)
- [Deriving typeclasses for `newtype`s](./Usage/Deriving%20for%20newtypes.md)
- [Deriving typeclasses with generics](./Usage/Deriving%20with%20generics.md)
- [`plet` to avoid work duplication](./Usage/Avoid%20work%20duplication%20using%20plet.md)
- [Tracing](./Usage/Tracing.md)
- [Raising errors](./Usage/Raising%20errors.md)
- [Unsafe functions](./Usage/Unsafe%20functions.md)
- [Interoperability with PlutusTx](./Usage/FFI.md)

## Concepts

The [Concepts section](./Concepts.md) details additional concepts.

- [Hoisting, metaprogramming, and fundamentals](./Concepts/Hoisting.md)
  - [Hoisting Operators](./Concepts/Hoisting.md#hoisting-operators)
- [What is the `s`?](./Concepts/What%20is%20the%20S.md#what-is-the-s)
- [Data encoding and Scott encoding](./Concepts/Data%20and%20Scott%20encoding.md)
  - [Data encoding](./Concepts/Data%20and%20Scott%20encoding.md#data-encoding)
  - [Scott encoding](./Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)
- [Haskell synonym of Plutarch types](./Concepts/Haskell%20Synonym.md)

## Typeclasses

The [Typeclasses section](./Typeclasses.md) discusses the primary typeclasses related to Plutarch.

- [`PEq` & `POrd`](./Typeclasses/PEq%20and%20POrd.md)
- [`PIntegral`](./Typeclasses/PIntegral.md)
- [`PIsData`](./Typeclasses/PIsData.md)
- [`PlutusType`, `PCon`, and `PMatch`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [Implementing `PlutusType` for your own types (`newtype`)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md)
  - [Implementing `PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift)
  - [Implementing `PConstant` & `PLift` for types with type variables (generic types)](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./Typeclasses/PListLike.md)
- [`PIsDataRepr` & `PDataFields`](./Typeclasses/PIsDataRepr%20and%20PDataFields.md)
  - [All about extracting fields](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-constructing-data-values)
- [`PTryFrom`](./Typeclasses/PTryFrom.md)
  - [Laws](./Typeclasses/PTryFrom.md#laws)
  - [Implementing `PTryFrom`](./Typeclasses/PTryFrom.md#implementing-ptryfrom)
  - [`PTryFromExcess`](./Typeclasses/PTryFrom.md#ptryfromexcess)
  - [Recovering only partially](./Typeclasses/PTryFrom.md#recovering-only-partially)

## Working with Types

The [Types section](./Types.md) discusses the core types of Plutarch.

- [`PInteger`](./Types/PInteger.md)
- [`PBool`](./Types/PBool.md)
- [`PString`](./Types/PString.md)
- [`PByteString`](./Types/PByteString.md)
- [`PUnit`](./Types/PUnit.md)
- [`PBuiltinList`](./Types/PBuiltinList.md)
- [`PList`](./Types/PList.md)
- [`PBuiltinPair`](./Types/PBuiltinPair.md)
- [`PTuple`](./Types/PTuple.md)
- [`PAsData`](./Types/PAsData.md)
- [`PDataSum` & `PDataRecord`](./Types/PDataSum%20and%20PDataRecord.md)
- [`PData`](./Types/PData.md)

# Examples

- [Basic examples](./examples/BASIC.md)
- [Validator & Minting policies](./examples/VALIDATOR.md)

Also see: [examples](https://github.com/Plutonomicon/plutarch/tree/master/plutarch-test).

# Rules of thumb, Tips, and Tricks

Outside of the fundamental user guide, there are rules of thumb and general guidelines you can follow to improve your Plutarch experience. The [Tricks section](./Tricks.md) discusses ways of writing efficient and high quality Plutarch code, as well as rules that can help auditing Plutarch easier.

- [Plutarch functions are strict](./Tricks/Plutarch%20functions%20strict.md)
- [Don't duplicate work](./Tricks/Don't%20duplicate%20work.md)
  - [Where should arguments be `plet`ed?](./Tricks/Don't%20duplicate%20work.md#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](./Tricks/Prefer%20Plutarch%20functions.md)
- [When to use Haskell level functions?](./Tricks/Using%20Haskell%20level%20functions.md)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./Tricks/Difference%20between%20pcon%20and%20pconstant.md)
- [Let Haskell level functions take responsibility of evaluation](./Tricks/Responsibility%20of%20evaluation%20in%20Haskell%20functions.md)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./Tricks/makeIsDataIndexed,%20Haskell%20ADTs,%20and%20PIsDataRepr.md)
- [Prefer statically building constants whenever possible](./Tricks/Prefer%20statically%20building%20constants.md)
- [Figuring out the representation of a Plutarch type](./Tricks/Representation%20of%20Plutarch%20type.md)
- [Prefer pattern matching on the result of `pmatch` immediately](./Tricks/Prefer%20matching%20on%20pmatch%20result%20immediately.md)
- [Working with bound fields yielded by `pletFields`](./Tricks/Working%20with%20bound%20fields.md)

# Common Issues and Troubleshooting

Due to the highly abstracted nature of Plutarch and its utilization of advanced type level concepts, you might face unfamiliar errors. Don't worry, the guide is here to help!

- [No instance for `PUnsafeLiftDecl a`](./Troubleshooting.md#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](./Troubleshooting.md#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](./Troubleshooting.md#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `getField`, or `pletFields`)](./Troubleshooting.md#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-getField-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](./Troubleshooting.md#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](./Troubleshooting.md#lifting-pasdata)
- [Couldn't match type `PLifted (PConstanted Foo)` with `Foo`](./Troubleshooting.md#couldnt-match-type-plifted-pconstanted-foo-with-foo)
- [Type match errors when using `pfield`/`getField` (or `OverloadedRecordDot` to access field)](./Troubleshooting.md#type-match-errors-when-using-pfieldgetField-or-overloadedrecorddot-to-access-field)

# Useful Links

- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
