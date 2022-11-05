# `PByteString`

Plutarch level bytestring terms can be created using `phexByteStr` and `pbyteStr`. `phexByteStr` interprets a hex string literal as a `Term s PByteString` and `pbyteStr` merely converts a `ByteString` into a `Term s PByteString`.

```haskell
import qualified Data.ByteString as BS

phexByteStr "41"
-- yields a `Term s PByteString`, which represents [65]

pbyteStr (BS.pack [91])
-- yields a `Term s PByteString`, which represents [91]
```

Similar to `PString`, it has a `PEq` instance. As well as `Semigroup` and `Monoid` instances for its terms.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin bytestring](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinByteString).
