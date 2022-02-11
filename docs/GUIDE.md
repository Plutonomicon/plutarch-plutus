The Plutarch guide is your one stop shop for getting up to speed on Plutarch!

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Overview](#overview)
  - [Compiling and Running](#compiling-and-running)
  - [Syntax](#syntax)
  - [Usage](#usage)
  - [Concepts](#concepts)
  - [Typeclasses](#typeclasses)
  - [Working with Types](#working-with-types)
- [Examples](#examples)
- [Thumb rules, Tips, and Tricks](#thumb-rules-tips-and-tricks)
- [Common Issues and Troubleshooting](#common-issues-and-troubleshooting)
- [Useful Links](#useful-links)

</details>

# Overview

## Compiling and Running

- [Common Extensions and GHC options](#common-extensions-and-ghc-options)
- [Evaluation](#evaluation)

## Syntax

- [Constants](#constants)
  - [Static building with `pconstant` and `pconstantData`](#static-building-with-pconstant-and-pconstantdata)
  - [Dynamic building with `pcon`](#dynamic-building-with-pcon)
  - [Overloaded literals](#overloaded-literals)
  - [Miscellaneous](#miscellaneous)
- [Lambdas](#lambdas)
- [Delayed terms and Forcing](#delayed-terms-and-forcing)

## Usage

- [Applying functions](#applying-functions)
- [Conditionals](#conditionals)
- [Recursion](#recursion)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](#do-syntax-with-qualifieddo-and-plutarchmonadic)
  - [Translating `do` syntax with `QualifiedDo` to GHC 8](#translating-do-syntax-with-qualifieddo-to-ghc-8)
- [Do syntax with `TermCont`](#do-syntax-with-termcont)
- [Deriving typeclasses for `newtype`s](#deriving-typeclasses-for-newtypes)
- [Deriving typeclasses with generics](#deriving-typeclasses-with-generics)

## Concepts

- [Hoisting, metaprogramming,  and fundamentals](#hoisting-metaprogramming--and-fundamentals)
  - [Hoisting Operators](#hoisting-operators)
- [What is the `s`?](#what-is-the-s)
- [eDSL Types in Plutarch](#edsl-types-in-plutarch)
- [`plet` to avoid work duplication](#plet-to-avoid-work-duplication)
- [Tracing](#tracing)
- [Raising errors](#raising-errors)
- [Delay and Force](#delay-and-force)
- [Data encoding and Scott encoding](#data-encoding-and-scott-encoding)
- [Unsafe functions](#unsafe-functions)

## Typeclasses

- [Equality and Order](#equality-and-order)
- [Monoids](#monoids)
- [PIntegral](#pintegral)
- [PIsData](#pisdata)
- [PConstant & PLift](#pconstant--plift)
  - [Implementing `PConstant` & `PLift`](#implementing-pconstant--plift)
- [PlutusType, PCon, and PMatch](#plutustype-pcon-and-pmatch)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (Data Encoding)](#implementing-plutustype-for-your-own-types-data-encoding)
- [PListLike](#plistlike)
- [PIsDataRepr & PDataFields](#pisdatarepr--pdatafields)
  - [All about extracting fields](#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](#all-about-constructing-data-values)
  - [Implementing PIsDataRepr and friends](#implementing-pisdatarepr-and-friends)

## Working with Types

- [PInteger](#pinteger)
- [PBool](#pbool)
- [PString](#pstring)
- [PByteString](#pbytestring)
- [PUnit](#punit)
- [PBuiltinList](#pbuiltinlist)
- [PList](#plist)
- [PBuiltinPair](#pbuiltinpair)
- [PTuple](#ptuple)
- [PAsData](#pasdata)
- [PDataSum & PDataRecord](#pdatasum--pdatarecord)
- [PRecord](#precord)
  - [letrec](#letrec)
  - [Record Data](#record-data)
- [PData](#pdata)

# Examples
Be sure to check out [Compiling and Running](#compiling-and-running) first!

- [Basic examples](./examples/BASIC.md)
- [Validator & Minting policies](./examples/VALIDATOR.md)

Also see: [examples](./examples/).

# Thumb rules, Tips, and Tricks
Outside of the fundamental user guide, there are thumb rules and general guidelines you can follow to improve your Plutarch experience. In this section, we discuss ways of writing efficient and high quality Plutarch code, as well as rules that can help auditing Plutarch easier.

- [Plutarch functions are strict](#plutarch-functions-are-strict)
- [Don't duplicate work](#dont-duplicate-work)
  - [Where should arguments be `plet`ed?](#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](#prefer-plutarch-level-functions)
- [When to use Haskell level functions?](#when-to-use-haskell-level-functions)
- [Hoisting is great - but not a silver bullet](#hoisting-is-great---but-not-a-silver-bullet)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](#the-difference-between-plutustypepcon-and-plifts-pconstant)
- [List iteration is strict](#list-iteration-is-strict)
- [Let Haskell level functions take responsibility of evaluation](#let-haskell-level-functions-take-responsibility-of-evaluation)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)
- [Prefer statically building constants whenever possible](#prefer-statically-building-constants-whenever-possible)

# Common Issues and Troubleshooting
Due to the highly abstracted nature of Plutarch and its utilization of advanced type level concepts, you might face unfamiliar errors. Don't worry, the guide is here to help!

- [No instance for (PUnsafeLiftDecl a)](#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](#lifting-pasdata)

# Useful Links
- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
