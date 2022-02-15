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

> Aside: Not interested in the details? Skip straight to [examples](#examples)!

# Overview

## Compiling and Running

- [Common Extensions and GHC options](./RUN.md#common-extensions-and-ghc-options)
- [Evaluation](./RUN.md#evaluation)

## Syntax

- [Constants](./SYNTAX.md#constants)
  - [Static building with `pconstant` and `pconstantData`](./SYNTAX.md#static-building-with-pconstant-and-pconstantdata)
  - [Dynamic building with `pcon`](./SYNTAX.md#dynamic-building-with-pcon)
  - [Overloaded literals](./SYNTAX.md#overloaded-literals)
  - [Miscellaneous](./SYNTAX.md#miscellaneous)
- [Lambdas](./SYNTAX.md#lambdas)
- [Delayed terms and Forcing](./SYNTAX.md#delayed-terms-and-forcing)

## Usage

- [Applying functions](./USAGE.md#applying-functions)
- [Conditionals](./USAGE.md#conditionals)
- [Recursion](./USAGE.md#recursion)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](./USAGE.md#do-syntax-with-qualifieddo-and-plutarchmonadic)
  - [Translating `do` syntax with `QualifiedDo` to GHC 8](./USAGE.md#translating-do-syntax-with-qualifieddo-to-ghc-8)
- [Do syntax with `TermCont`](./USAGE.md#do-syntax-with-termcont)
- [Deriving typeclasses for `newtype`s](./USAGE.md#deriving-typeclasses-for-newtypes)
- [Deriving typeclasses with generics](./USAGE.md#deriving-typeclasses-with-generics)

## Concepts

- [Hoisting, metaprogramming,  and fundamentals](./CONCEPTS.md#hoisting-metaprogramming--and-fundamentals)
  - [Hoisting Operators](./CONCEPTS.md#hoisting-operators)
- [What is the `s`?](./CONCEPTS.md#what-is-the-s)
- [eDSL Types in Plutarch](./CONCEPTS.md#edsl-types-in-plutarch)
- [`plet` to avoid work duplication](./CONCEPTS.md#plet-to-avoid-work-duplication)
- [Tracing](./CONCEPTS.md#tracing)
- [Raising errors](./CONCEPTS.md#raising-errors)
- [Delay and Force](./CONCEPTS.md#delay-and-force)
- [Data encoding and Scott encoding](./CONCEPTS.md#data-encoding-and-scott-encoding)
- [Unsafe functions](./CONCEPTS.md#unsafe-functions)

## Typeclasses

- [`PEq` & `PORd`](./TYPECLASSES.md#peq--pord)
- [`PIntegral`](./TYPECLASSES.md#pintegral)
- [`PIsData`](./TYPECLASSES.md#pisdata)
- [`PConstant` & `PLift`](./TYPECLASSES.md#pconstant--plift)
  - [Implementing `PConstant` & `PLift`](./TYPECLASSES.md#implementing-pconstant--plift)
- [`PlutusType`, `PCon`, and `PMatch`](./TYPECLASSES.md#plutustype-pcon-and-pmatch)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-data-encoding)
- [`PListLike`](./TYPECLASSES.md#plistlike)
- [`PIsDataRepr` & `PDataFields`](./TYPECLASSES.md#pisdatarepr--pdatafields)
  - [All about extracting fields](./TYPECLASSES.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./TYPECLASSES.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./TYPECLASSES.md#all-about-constructing-data-values)
  - [Implementing `PIsDataRepr` and friends](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)

## Working with Types

- [PInteger](./TYPES.md#pinteger)
- [PBool](./TYPES.md#pbool)
- [PString](./TYPES.md#pstring)
- [PByteString](./TYPES.md#pbytestring)
- [PUnit](./TYPES.md#punit)
- [PBuiltinList](./TYPES.md#pbuiltinlist)
- [PList](./TYPES.md#plist)
- [PBuiltinPair](./TYPES.md#pbuiltinpair)
- [PTuple](./TYPES.md#ptuple)
- [PAsData](./TYPES.md#pasdata)
- [PDataSum & PDataRecord](./TYPES.md#pdatasum--pdatarecord)
- [PRecord](./TYPES.md#precord)
  - [letrec](./TYPES.md#letrec)
  - [Record Data](./TYPES.md#record-data)
- [PData](./TYPES.md#pdata)

# Examples
- [Basic examples](https://github.com/Plutonomicon/plutarch/tree/master/examples/BASIC.md)
- [Validator & Minting policies](https://github.com/Plutonomicon/plutarch/tree/master/examples/VALIDATOR.md)

Also see: [examples](https://github.com/Plutonomicon/plutarch/tree/master/examples).

# Thumb rules, Tips, and Tricks
Outside of the fundamental user guide, there are thumb rules and general guidelines you can follow to improve your Plutarch experience. In this section, we discuss ways of writing efficient and high quality Plutarch code, as well as rules that can help auditing Plutarch easier.

- [Plutarch functions are strict](./TRICKS.md#plutarch-functions-are-strict)
- [Don't duplicate work](./TRICKS.md#dont-duplicate-work)
  - [Where should arguments be `plet`ed?](./TRICKS.md#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](./TRICKS.md#prefer-plutarch-level-functions)
- [When to use Haskell level functions?](./TRICKS.md#when-to-use-haskell-level-functions)
- [Hoisting is great - but not a silver bullet](./TRICKS.md#hoisting-is-great---but-not-a-silver-bullet)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./TRICKS.md#the-difference-between-plutustypepcon-and-plifts-pconstant)
- [List iteration is strict](./TRICKS.md#list-iteration-is-strict)
- [Let Haskell level functions take responsibility of evaluation](./TRICKS.md#let-haskell-level-functions-take-responsibility-of-evaluation)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./TRICKS.md#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)
- [Prefer statically building constants whenever possible](./TRICKS.md#prefer-statically-building-constants-whenever-possible)

# Common Issues and Troubleshooting
Due to the highly abstracted nature of Plutarch and its utilization of advanced type level concepts, you might face unfamiliar errors. Don't worry, the guide is here to help!

- [No instance for (PUnsafeLiftDecl a)](./TROUBLESHOOTING.md#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](./TROUBLESHOOTING.md#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](./TROUBLESHOOTING.md#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](./TROUBLESHOOTING.md#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](./TROUBLESHOOTING.md#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](./TROUBLESHOOTING.md#lifting-pasdata)

# Useful Links
- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
