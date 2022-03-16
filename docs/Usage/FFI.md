# Interoperability with PlutusTx

If you already have a codebase built using PlutusTx, you can choose to
re-write only its critical parts in Plutarch and to call them from
PlutusTx. The function to use is `Plutarch.FFI.foreignExport`:

```haskell
doubleInPlutarch :: Term s (PInteger -> PInteger)
doubleInPlutarch = plam (2 *)

doubleExported :: PlutusTx.CompiledCode (Integer -> Integer)
doubleExported = foreignExport doubleInPlutarch

doubleUseInPlutusTx :: PlutusTx.CompiledCode Integer
doubleUseInPlutusTx = doubleExported `PlutusTx.applyCode` PlutusTx.liftCode 21
```

Alternatively, you may go in the opposite direction and call an existing PlutusTx function from Plutarch using `Plutarch.FFI.foreignImport`:

```haskell
doubleInPlutusTx :: CompiledCode (Integer -> Integer)
doubleInPlutusTx = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport doubleInPlutusTx

doubleUseInPlutarch :: Term s PInteger
doubleUseInPlutarch = doubleImported # 21
```
