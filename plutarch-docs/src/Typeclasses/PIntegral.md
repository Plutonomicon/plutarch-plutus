# `PIntegral`

This is similar to the `Integral` typeclass. However, it only has the following class methods:

- `pdiv` - similar to `div`
- `pmod` - similar to `mod`
- `pquot` - similar to `quot`
- `prem` - similar to `rem`

Using these functions, you can do division/modulus etc. on Plutarch level values:

```hs
pdiv # 6 # 3
```

where `6` and `3` are `Term s PInteger`s yields `2` - also a `Term s PInteger`.
