<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Plutarch.Docs.ValidatorExample (alwaysSucceeds, checkSignatory, res', res, alwaysFails) where 

import Plutarch.Prelude
import Plutarch.Api.V1 (PDatum, PRedeemer, PScriptContext)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Contexts (PScriptPurpose(PSpending))
import Plutarch.Docs.Run (evalWithArgsT)
import Plutarch.Script (Script)
import qualified PlutusTx
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import qualified Plutarch.Monadic as P
import Data.Text (Text)
```

</p>
</details>
Examples of validators and minting policies written in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

- [Validator that always succeeds](#validator-that-always-succeeds)
- [Validator that always fails](#validator-that-always-fails)
- [Validator that checks whether a value is present within signatories](#validator-that-checks-whether-a-value-is-present-within-signatories)
- [Using custom datum/redeemer in your Validator](#using-custom-datumredeemer-in-your-validator)

> Aside: Be sure to check out [Compiling and Running](./../README.md#compiling-and-running) first!

# Validator that always succeeds

```haskell
alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \_datm _redm _ctx -> pconstant ()
```

All the arguments are ignored. 

Execution:

```haskell
res' :: Either Text (Script, ExBudget, [Text])
res' = alwaysSucceeds `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
-- >>> res'
-- Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

# Validator that always fails

```haskell
alwaysFails :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysFails = plam $ \_datm _redm _ctx -> perror
```

Similar to the example above.

Execution:

```haskell
res :: Either Text (Script, ExBudget, [Text])
res = alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
-- >>> res 
-- Left (EvaluationError [] "(CekEvaluationFailure,Nothing)")
```

# Validator that checks whether a value is present within signatories

```haskell

checkSignatory :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pdata ph # pfromData signatories)
    -- Success!
    (pconstant ())
    -- Signature not present.
    perror
```

> Note: The above snippet uses GHC 9 features (`QualifiedDo` and `OverloadedRecordDot`). Be sure to check out [Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md) and [alternatives to `OverloadedRecordDot`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot).

We match on the script purpose to see if its actually for _spending_ - and we get the signatories field from `txInfo` (the 7th field), check if the given pub key hash is present within the signatories and that's it!

It's important that we pass a `PPubKeyHash` _prior_ to treating `checkSignatory` as a validator script.

```hs
{-# LANGUAGE OverloadedStrings #-}

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

hashStr :: PubKeyHash
hashStr = "abce0f123e"

pubKeyHash :: Term s PPubKeyHash
pubKeyHash = pconstant hashStr

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      [fromString hashStr, "f013", "ab45"]
      mempty
      ""
    )
    (Spending (TxOutRef "" 1))

> evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

# Using custom datum/redeemer in your Validator

All you have to do is [implement `PIsDataRepr` and friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends) for your custom datum/redeemer and you can use it just like `PScriptContext` in your validators!
