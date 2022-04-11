This document describes how to compile and run Plutarch - whether for on chain deployment or off chain testing.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Common Extensions and GHC options](#common-extensions-and-ghc-options)
- [Evaluation](#evaluation)

</details>

# Common Extensions and GHC options

You generally want to adhere to the same extensions and GHC options the [Plutarch repo](https://github.com/Plutonomicon/plutarch/blob/master/plutarch.cabal) uses.

<details>
<summary> List of GHC extensions </summary>

- `NoStarIsType`
- `BangPatterns`
- `BinaryLiterals`
- `ConstrainedClassMethods`
- `ConstraintKinds`
- `DataKinds`
- `DeriveAnyClass`
- `DeriveDataTypeable`
- `DeriveFoldable`
- `DeriveFunctor`
- `DeriveGeneric`
- `DeriveLift`
- `DeriveTraversable`
- `DerivingStrategies`
- `DerivingVia`
- `DoAndIfThenElse`
- `EmptyCase`
- `EmptyDataDecls`
- `EmptyDataDeriving`
- `ExistentialQuantification`
- `ExplicitForAll`
- `FlexibleContexts`
- `FlexibleInstances`
- `ForeignFunctionInterface`
- `GADTSyntax`
- `GeneralisedNewtypeDeriving`
- `HexFloatLiterals`
- `ImplicitPrelude`
- `InstanceSigs`
- `KindSignatures`
- `LambdaCase`
- `MonomorphismRestriction`
- `MultiParamTypeClasses`
- `NamedFieldPuns`
- `NamedWildCards`
- `NumericUnderscores`
- `OverloadedStrings`
- `PartialTypeSignatures`
- `PatternGuards`
- `PolyKinds`
- `PostfixOperators`
- `RankNTypes`
- `RelaxedPolyRec`
- `ScopedTypeVariables`
- `StandaloneDeriving`
- `StandaloneKindSignatures`
- `TraditionalRecordSyntax`
- `TupleSections`
- `TypeApplications`
- `TypeFamilies`
- `TypeOperators`
- `TypeSynonymInstances`
- `ViewPatterns`

</details>

# Evaluation

You can compile a Plutarch term using `compile` (from `Plutarch` module), making sure it has no free variables. `compile` returns a `Script`, which you can use as you would any other Plutus script. The API in [`Plutus.V1.Ledger.Scripts`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html) should prove helpful.

> For further insight into what is compiled - you can use `printTerm` or `printScript` (from `Plutarch` module).

I often use these helper functions to test Plutarch quickly:

```haskell
import Data.Text                (Text)
import Plutarch                 (ClosedTerm, compile)
import Plutarch.Evaluate        (evalScript, EvalError)
import Plutus.V1.Ledger.Api     (Data, ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), applyArguments)
import UntypedPlutusCore        (DeBruijn, DefaultFun, DefaultUni, Program)

evalT :: ClosedTerm a -> (Either EvalError (Program DeBruijn DefaultUni DefaultFun ()), ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> (Either EvalError (Program DeBruijn DefaultUni DefaultFun ()), ExBudget, [Text])
evalWithArgsT x args = (\(res, budg, trcs) -> (fmap unScript res, budg, trcs))
  . evalScript
  . flip applyArguments args
  $ compile x
```

The fields in the result triple correspond to script result, execution budget (how much memory and CPU units were used), and trace log - respectively. Often you're only interested in the script result, in that case you can use:

```haskell
evalT :: ClosedTerm a -> Either EvalError (Program DeBruijn DefaultUni DefaultFun ())
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either EvalError (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = (\(res, _, _) -> fmap unScript res)
  . evalScript
  . flip applyArguments args
  $ compile x
```

> Note: You can pretty much ignore the UPLC types involved here. All it really means is that the result is a "UPLC program". When it's printed, it's pretty legible - especially for debugging purposes. Although not necessary to use Plutarch, you may find the [Plutonomicon UPLC guide](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md) useful.
