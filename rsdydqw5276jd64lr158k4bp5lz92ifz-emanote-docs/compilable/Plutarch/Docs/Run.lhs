<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Run (applyArguments, evalT, evalSerialize, evalWithArgsT, evalWithArgsT') where 
import Data.Bifunctor (first)
import Data.ByteString.Short (ShortByteString)
import Data.Default (def)
import Data.Text (Text, pack)
import Plutarch (ClosedTerm, compile)
import Plutarch.Script (Script (Script, unScript), serialiseScript)
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V1 (Data, ExBudget)
import PlutusCore.MkPlc (mkIterApp, mkConstant)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program, progTerm)
import Control.Lens.Combinators (over)
```

</p>
</details>
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
applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
    let termArgs = mkConstant () <$> args
        applied t = mkIterApp () t termArgs
    in Script $ over progTerm applied p

evalSerialize :: ClosedTerm a -> Either Text ShortByteString
evalSerialize x = serialiseScript . (\(a, _, _) -> a) <$> evalT x

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args
```

The fields in the result triple correspond to script result, execution budget (how much memory and CPU units were used), and trace log - respectively.
Of course if you're only interested in the result of the script evaluation, you can just ignore the exbudget and tracelog just like `evalSerialize` does.
`evalSerialize` is a function that you can use to quickly obtain a serialized script.

> Note: You can pretty much ignore the UPLC types involved here. All it really means is that the result is a "UPLC program". When it's printed, it's pretty legible - especially for debugging purposes. Although not necessary to use Plutarch, you may find the [Plutonomicon UPLC guide](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md) useful.
