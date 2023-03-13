<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.DifferencePconPconstant () where 
import Plutarch.Prelude
import Plutarch.Internal.PlutusType (PlutusType(pcon', pmatch'))
```

</p>
</details>

# The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`

`PlutusType` is especially useful for building up Plutarch terms _dynamically_ - i.e. from arbitrary Plutarch terms. This is when your Plutarch type's constructors contain other Plutarch terms.

Another case `PlutusType` is useful is when you want to give your Plutarch type a custom representation, Scott encoding, enum - what have you. From the `PlutusType` haddock example:

```haskell
data AB (s :: S) = A | B

instance PlutusType AB where
  type PInner AB = PInteger
  pcon' A = 0
  pcon' B = 1
  pmatch' x f = pif (x #== 0) (f A) (f B)
```

You can use the `A` and `B` constructors during building, but still have your type be represented as integers under the hood! You cannot do this with `pconstant`.

You should prefer `pconstant`/`pconstantData` (from [`PConstant`/`PLift`](./../Typeclasses/PConstant%20and%20PLift.md)) when you can build something up entirely from Haskell level constants and that _something_ has the same representation as the Haskell constant.
