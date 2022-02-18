The Plutarch guide is your one-stop shop for getting up to speed on Plutarch!

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Overview](#overview)
  - [Compiling and Running](#compiling-and-running)
  - [Introduction and Basic Syntax](#syntax)
  - [Usage](#usage)
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

- [Common Extensions and GHC options](./RUN.md#common-extensions-and-ghc-options)
- [Evaluation](./RUN.md#evaluation)

## Introduction and Basic Syntax

This section serves as a introduction to Plutarch's basic concepts and core syntactic constructs. It will help build a mental model of Plutarch, but is insufficient to write production-ready code.

- [Overview](./INTRO.md#overview)
- [Untyped Plutus Core (UPLC)](./INTRO.md#untyped-plutus-core--uplc-)
- [Plutarch Types](./INTRO.md#plutarch-types)
- [Plutarch `Term`s](./INTRO.md#plutarch--term-s)
  * [Plutarch Constant `Term`s](./INTRO.md#plutarch-constant--term-s)
    + [Static building of constant `Term`s with `pconstant`](./INTRO.md#static-building-of-constant--term-s-with--pconstant-)
    + [Dynamic building of constant `Term`s with `pcon`](./INTRO.md#dynamic-building-of-constant--term-s-with--pcon-)
      - [Pattern matching `Term`s on `PType`s with `pmatch`.](./INTRO.md#pattern-matching--term-s-on--ptype-s-with--pmatch-)
      - [`class (PCon a, PMatch a) => PlutusType (a :: PType)` -- with a default instance.](./INTRO.md#-class--pcon-a--pmatch-a-----plutustype--a----ptype------with-a-default-instance)
    + [Overloaded literals](./INTRO.md#overloaded-literals)
  * [Helper functions](./INTRO.md#helper-functions)
  * [Lambdas; Plutarch-level Function `Term`s.](./INTRO.md#lambdas--plutarch-level-function--term-s)
    + [Function Application](./INTRO.md#function-application)
    + [Strictness and Laziness; Delayed Terms and Forcing](./INTRO.md#strictness-and-laziness--delayed-terms-and-forcing)
- [References](./INTRO.md#references)
 
## Practical Usage

This section fills in the gaps left by the previous. It illustrates techniques that make Plutarch easier to work with.

- [Conditionals](./USAGE.md#conditionals)
- [Recursion](./USAGE.md#recursion)
- [Do syntax with `TermCont`](./USAGE.md#do-syntax-with--termcont-)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](./USAGE.md#do-syntax-with--qualifieddo--and--plutarchmonadic-)
- [Deriving typeclasses for `newtype`s](./USAGE.md#deriving-typeclasses-for--newtype-s)
- [Deriving typeclasses with generics](./USAGE.md#deriving-typeclasses-with-generics)
- [`plet` to avoid work duplication](./USAGE.md#-plet--to-avoid-work-duplication)
- [Tracing](./USAGE.md#tracing)
- [Raising errors](./USAGE.md#raising-errors)
- [Unsafe functions](./USAGE.md#unsafe-functions)

## Concepts

This section details additional concepts 

- [Hoisting, metaprogramming, and fundamentals](./CONCEPTS.md#hoisting--metaprogramming--and-fundamentals)
  * [Hoisting Operators](./CONCEPTS.md#hoisting-operators)
- [What is the `s`?](./CONCEPTS.md#what-is-the--s--)
- [Data encoding and Scott encoding](./CONCEPTS.md#data-encoding-and-scott-encoding)
  * [Data encoding](./CONCEPTS.md#data-encoding)
  * [Scott encoding](./CONCEPTS.md#scott-encoding)
- [Haskell synonym of Plutarch types](./CONCEPTS.md#haskell-synonym-of-plutarch-types)

## Typeclasses

- [`PEq` & `PORd`](./TYPECLASSES.md#peq--pord)
- [`PIntegral`](./TYPECLASSES.md#pintegral)
- [`PIsData`](./TYPECLASSES.md#pisdata)
- [`PlutusType`, `PCon`, and `PMatch`](./TYPECLASSES.md#plutustype-pcon-and-pmatch)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [Implementing `PlutusType` for your own types (`newtype`)](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./TYPECLASSES.md#pconstant--plift)
  - [Implementing `PConstant` & `PLift`](./TYPECLASSES.md#implementing-pconstant--plift)
  - [Implementing `PConstant` & `PLift` for types with type variables (generic types)](./TYPECLASSES.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./TYPECLASSES.md#plistlike)
- [`PIsDataRepr` & `PDataFields`](./TYPECLASSES.md#pisdatarepr--pdatafields)
  - [All about extracting fields](./TYPECLASSES.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./TYPECLASSES.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./TYPECLASSES.md#all-about-constructing-data-values)
  - [Implementing `PIsDataRepr` and friends](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)

## Working with Types

- [`PInteger`](./TYPES.md#pinteger)
- [`PBool`](./TYPES.md#pbool)
- [`PString`](./TYPES.md#pstring)
- [`PByteString`](./TYPES.md#pbytestring)
- [`PUnit`](./TYPES.md#punit)
- [`PBuiltinList`](./TYPES.md#pbuiltinlist)
- [`PList`](./TYPES.md#plist)
- [`PBuiltinPair`](./TYPES.md#pbuiltinpair)
- [`PTuple`](./TYPES.md#ptuple)
- [`PAsData`](./TYPES.md#pasdata)
- [`PDataSum` & `PDataRecord`](./TYPES.md#pdatasum--pdatarecord)
- [`PRecord`](./TYPES.md#precord)
  - [letrec](./TYPES.md#letrec)
  - [Record Data](./TYPES.md#record-data)
- [`PData`](./TYPES.md#pdata)

# Examples

- [Basic examples](./examples/BASIC.md)
- [Validator & Minting policies](./examples/VALIDATOR.md)

Also see: [examples](https://github.com/Plutonomicon/plutarch/tree/master/examples).

# Rules of thumb, Tips, and Tricks

Outside of the fundamental user guide, there are rules of thumb and general guidelines you can follow to improve your Plutarch experience. In this section, we discuss ways of writing efficient and high quality Plutarch code, as well as rules that can help auditing Plutarch easier.

- [Plutarch functions are strict](./TRICKS.md#plutarch-functions-are-strict)
- [Don't duplicate work](./TRICKS.md#dont-duplicate-work)
  - [Where should arguments be `plet`ed?](./TRICKS.md#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](./TRICKS.md#prefer-plutarch-level-functions)
- [When to use Haskell level functions?](./TRICKS.md#when-to-use-haskell-level-functions)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./TRICKS.md#the-difference-between-plutustypepcon-and-plifts-pconstant)
- [List iteration is strict](./TRICKS.md#list-iteration-is-strict)
- [Let Haskell level functions take responsibility of evaluation](./TRICKS.md#let-haskell-level-functions-take-responsibility-of-evaluation)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./TRICKS.md#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)
- [Prefer statically building constants whenever possible](./TRICKS.md#prefer-statically-building-constants-whenever-possible)
- [Figuring out the representation of a Plutarch type](./TRICKS.md#figuring-out-the-representation-of-a-plutarch-type)

# Common Issues and Troubleshooting

Due to the highly abstracted nature of Plutarch and its utilization of advanced type level concepts, you might face unfamiliar errors. Don't worry, the guide is here to help!

- [No instance for `PUnsafeLiftDecl a`](./TROUBLESHOOTING.md#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](./TROUBLESHOOTING.md#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](./TROUBLESHOOTING.md#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](./TROUBLESHOOTING.md#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](./TROUBLESHOOTING.md#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](./TROUBLESHOOTING.md#lifting-pasdata)
- [Couldn't match type `PLifted (PConstanted Foo)` with `Foo`](./TROUBLESHOOTING.md#couldnt-match-type-plifted-pconstanted-foo-with-foo)
- [Type match errors when using `pfield`/`hrecField` (or `OverloadedRecordDot` to access field)](./TROUBLESHOOTING.md#type-match-errors-when-using-pfieldhrecfield-or-overloadedrecorddot-to-access-field)

# Useful Links

- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
