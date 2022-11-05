# `PBool`

Plutarch level boolean terms can be constructed using `pconstant True` and `pconstant False`.

```haskell
pif (pconstant PFalse) 7 42
-- evaluates to 42
```

You can combine Plutarch booleans terms using `#&&` and `#||`, which are synonyms to `&&` and `||`. These are Haskell level operators and therefore have short circuiting. If you don't need short circuiting, you can use the Plutarch level alternatives- `pand'` and `por'` respectively.

This is synonymous to Plutus Core [builtin boolean](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool).
