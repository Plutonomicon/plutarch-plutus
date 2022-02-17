This document describes how to compile and run Plutarch - whether for on chain deployment or off chain testing.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

-   [Common Extensions and GHC options](#common-extensions-and-ghc-options)
-   [Evaluation](#evaluation)

</details>

# Common Extensions and GHC options

You generally want to adhere to the same extensions and GHC options the [Plutarch repo](https://github.com/Plutonomicon/plutarch/blob/master/plutarch.cabal) uses.

> Jack: Maybe consider listing these in-full? Potentially with a link to the `.cabal` and a disclaimer that it _might_ not be up-to-date? In the old file it would've been lengthy but now you have split it up...?

# Evaluation

You can compile a Plutarch term using `compile`(from `Plutarch` module), making sure it has no free variables. `compile` returns a `Script`- you can use this as you would any other Plutus script. The API in `Plutus.V1.Ledger.Scripts` should prove helpful.

> Jack: `compile` (from `Plutarch module`) \[missing space].

> Jack: `Script` - you can \[missing space].

> Jack: consider linking to [here](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html).

> For further insight into what is compiled - you can use `printTerm` or `printScript` (from `Plutarch` module).

I often use these helper functions to test Plutarch quickly-

> Jack: replace - with : 

```haskell
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

eval :: ClosedTerm a -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
eval x = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript $ compile x

evalWithArgs :: ClosedTerm a -> [Data] -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
evalWithArgs x args = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript . flip applyArguments args $ compile x
```

The fields in the result triple correspond to execution budget (how much memory and CPU units were used), trace log, and script result - respectively. Often you're only interested in the script result, in that case you can use-

```haskell
evalT :: ClosedTerm a -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalT x = fmap (\(_, _, s) -> unScript s) . evaluateScript $ compile x

evalWithArgsT :: ClosedTerm a -> [Data] -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = fmap (\(_, _, s) -> unScript s) . evaluateScript . flip applyArguments args $ compile x
```

> Note: You can pretty much ignore the UPLC types involved here. All it really means is that the result is a "UPLC program". When it's printed, it's pretty legible - especially for debugging purposes. Although not necessary to use Plutarch, you may find the [Plutonomicon UPLC guide](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md) useful.
