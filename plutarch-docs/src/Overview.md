The Plutarch guide is your one-stop shop for getting up to speed on Plutarch!

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

> Aside: Not interested in the details? Skip straight to [examples](#examples)!

# Overview

## Haddock
Haddock documentation of `plutus-core`, `plutus-ledger-api`, `plutus-tx`, and few other upstream library with correctly matched version to latest Plutarch is available:

[Go to Haddock](https://plutonomicon.github.io/plutarch-plutus/haddock/)

## Compiling and Running

- [Common Extensions and GHC options](./Run.md#common-extensions-and-ghc-options)
- [Evaluation](./Run.md#evaluation)

## Introduction and Basic Syntax

The [Introduction section](./Introduction.md) serves as a introduction to Plutarch's basic concepts and core syntactic constructs. It will help build a mental model of Plutarch, but is insufficient to write production-ready code.

- [Overview](./Introduction.md#overview)
- [Untyped Plutus Core (UPLC)](./Introduction/UntypedPlutusCore.md)
- [Plutarch Types](./Introduction/PlutarchTypes.md)
- [Plutarch `Term`s](./Introduction/PlutarchTerms.md)
  - [Plutarch Constant `Term`s](./Introduction/PlutarchTerms/PlutarchConstants.md)
    - [Static building of constant `Term`s with `pconstant`](./Introduction/PlutarchTerms/PlutarchConstants.md#static-building-of-constant-terms-with-pconstant)
    - [Dynamic building of constant `Term`s with `pcon`](./Introduction/PlutarchTerms/PlutarchConstants.md#dynamic-building-of-constant-terms-with-pcon)
    - [Overloaded literals](./Introduction/PlutarchTerms/PlutarchConstants.md#overloaded-literals)
    - [Helper functions](./Introduction/PlutarchTerms/PlutarchConstants.md#helper-functions)
  - [Lambdas; Plutarch-level Function `Term`s.](./Introduction/PlutarchTerms/PlutarchLambdas.md#lambdas-plutarch-level-function-terms)
    - [Function Application](./Introduction/PlutarchTerms/PlutarchLambdas.md#function-application)
- [Pattern matching constant `Term`s with `pmatch`.](./Introduction/Patternmatching.md)
- [Strictness and Laziness; Delayed Terms and Forcing](./Introduction/DelayAndForce.md)
- [References](./Introduction.md#references)

## Practical Usage

The [Usage section](./Usage.md) fills in the gaps left by the previous. It illustrates techniques that make Plutarch easier to work with.

- [Conditionals](./Usage/Conditionals.md)
- [Recursion](./Usage/Recursion.md)
- [Using the Plutarch Prelude](./Usage/PreludeMixin.md)
- [Do syntax with `TermCont`](./Usage/DoSyntaxWithTermCont.md)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](./Usage/DoSyntaxWithQualifiedDo.md)
- [Deriving typeclasses for `newtype`s](./Usage/DerivingForNewtypes.md)
- [Deriving typeclasses with generics](./Usage/DerivingWithGenerics.md)
- [`plet` to avoid work duplication](./Usage/AvoidWorkDuplicationUsingPlet.md)
- [Tracing](./Usage/Tracing.md)
- [Raising errors](./Usage/RaisingErrors.md)
- [Unsafe functions](./Usage/UnsafeFunctions.md)
- [Interoperability with PlutusTx](./Usage/FFI.md)

## Concepts

The [Concepts section](./Concepts.md) details additional concepts.

- [Hoisting, metaprogramming, and fundamentals](./Concepts/Hoisting.md)
  - [Hoisting Operators](./Concepts/Hoisting.md#hoisting-operators)
- [What is the `s`?](./Concepts/WhatIsTheS.md#what-is-the-s)
- [Data encoding and Scott encoding](./Concepts/DataAndScottEncoding.md)
  - [Data encoding](./Concepts/DataAndScottEncoding.md#data-encoding)
  - [Scott encoding](./Concepts/DataAndScottEncoding.md#scott-encoding)
- [Haskell synonym of Plutarch types](./Concepts/HaskellSynonym.md)

## Typeclasses

The [Typeclasses section](./Typeclasses.md) discusses the primary typeclasses related to Plutarch.

- [`PEq` & `POrd`](./Typeclasses/PEqAndPOrd.md)
- [`PIntegral`](./Typeclasses/PIntegral.md)
- [`PIsData`](./Typeclasses/PIsData.md)
- [`PlutusType`, `PCon`, and `PMatch`](./Typeclasses/PlutusType,PCon,PMatch.md)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [Implementing `PlutusType` for your own types (`newtype`)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./Typeclasses/PConstant and PLift.md)
  - [Implementing `PConstant` & `PLift`](./Typeclasses/PConstantAndPLift.md#implementing-pconstant--plift)
  - [Implementing `PConstant` & `PLift` for types with type variables (generic types)](./Typeclasses/PConstantAndPLift.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./Typeclasses/PListLike.md)
- [`PIsDataRepr` & `PDataFields`](./Typeclasses/PIsDataReprAndPDataFields.md)
  - [All about extracting fields](./Typeclasses/PIsDataReprAndPDataFields.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./Typeclasses/PIsDataReprAndPDataFields.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./Typeclasses/PIsDataReprAndPDataFields.md#all-about-constructing-data-values)
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
- [`PAsData`](./Types/PAsData.md)
- [`PDataSum` & `PDataRecord`](./Types/PDataSumAndPDataRecord.md)
- [`PData`](./Types/PData.md)

# Examples

- [Basic examples](./examples/BASIC.md)
- [Validator & Minting policies](./examples/VALIDATOR.md)

# Rules of thumb, Tips, and Tricks

Outside of the fundamental user guide, there are rules of thumb and general guidelines you can follow to improve your Plutarch experience. The [Tricks section](./Tricks.md) discusses ways of writing efficient and high quality Plutarch code, as well as rules that can help auditing Plutarch easier.

- [Plutarch functions are strict](./Tricks/PlutarchFunctionsStrict.md)
- [Don't duplicate work](./Tricks/DontDuplicateWork.md)
  - [Where should arguments be `plet`ed?](./Tricks/DontDuplicateWork.md#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](./Tricks/PreferPlutarchfunctions.md)
- [When to use Haskell level functions?](./Tricks/UsingHaskellLevelfunctions.md)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./Tricks/DifferenceBetweenPconAndPconstant.md)
- [Let Haskell level functions take responsibility of evaluation](./Tricks/ResponsibilityOfEvaluationInHaskellFunctions.md)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./Tricks/makeIsDataIndexed,HaskellADTs,PIsDataRepr.md)
- [Prefer statically building constants whenever possible](./Tricks/PreferStaticallyBuildingConstants.md)
- [Figuring out the representation of a Plutarch type](./Tricks/RepresentationOfPlutarchType.md)
- [Prefer pattern matching on the result of `pmatch` immediately](./Tricks/PreferMatchingOnPmatchResultImmediately.md)
- [Working with bound fields yielded by `pletFields`](./Tricks/WorkingWithBoundFields.md)

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
