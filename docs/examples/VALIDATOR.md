Examples of validators and minting policies written in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

- [Validator that always succeeds](#validator-that-always-succeeds)
- [Validator that always fails](#validator-that-always-fails)
- [Validator that checks whether a value is present within signatories](#validator-that-checks-whether-a-value-is-present-within-signatories)
- [Using custom datum/redeemer in your Validator](#using-custom-datumredeemer-in-your-validator)

> Aside: Be sure to check out [Compiling and Running](./../GUIDE.md#compiling-and-running) first!

# Validator that always succeeds

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts

alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()
```

All the arguments are ignored. So we use the generic `PDatum` and `PRedeemer` types.

Execution:

```hs
import qualified PlutusTx

> alwaysSucceeds `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

# Validator that always fails

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts

alwaysFails :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysFails = plam $ \datm redm ctx -> perror
```

Similar to the example above.

Execution:

```hs
import qualified PlutusTx

> alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Left (EvaluationError [] "(CekEvaluationFailure,Nothing)")
```

# Validator that checks whether a value is present within signatories

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Crypto
import Plutarch.Api.V1.Scripts

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

checkSignatory :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> unTermCont $ do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatchC ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pure $
    pif
      (pelem # pdata ph # pfromData signatories)
      -- Success!
      (pconstant ())
      -- Signature not present.
      perror
```

> Note: The above snippet uses GHC 9 features (`OverloadedRecordDot`). Be sure to check out [alternatives to `OverloadedRecordDot`](./../TYPECLASSES.md#alternatives-to-overloadedrecorddot).

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

All you have to do is [implement `PIsDataRepr` and friends](./../TYPECLASSES.md#implementing-pisdatarepr-and-friends) for your custom datum/redeemer and you can use it just like `PScriptContext` in your validators!
