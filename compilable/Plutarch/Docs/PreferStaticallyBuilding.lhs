<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PreferStaticallyBuilding (viacon, viaconstant) where 

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts (PScriptPurpose (PMinting))
import Plutarch.Api.V1.Value (PCurrencySymbol (PCurrencySymbol))
import PlutusLedgerApi.V1 (ScriptPurpose (Minting))

```

</p>
</details>

# Prefer statically building constants whenever possible

Whenever you can build a Plutarch constant out of a pure Haskell value - do it! Functions such as `pconstant`, `phexByteStr` operate on regular [Haskell synonyms](./../Concepts/Haskell%20Synonym.md) of Plutarch types. Unlike `pcon`, which potentially works on Plutarch terms (e.g. `pcon $ PJust x`, `x` is a `Term s a`). A Plutarch term is an entirely "runtime" concept. "Runtime" as in "Plutus Core Runtime". They only get evaluated during runtime!

On the other hand, whenever you transform a Haskell synonym to its corresponding Plutarch type using `pconstant`, `phexByteStr` etc. - you're _directly_ building a Plutus Core constant. This is entirely static! There are no runtime function calls, no runtime building, it's just _there_, inside the compiled script.

Here's an example, let's say you want to build a `PScriptPurpose` - `PMinting "f1e301"`. Which snippet, do you think, is better?

```haskell
viaconstant :: Term s PScriptPurpose
viaconstant = pconstant (Minting "f1e301")
-- (or)

viacon :: Term s PScriptPurpose
viacon = let currSym = pcon $ PCurrencySymbol $ phexByteStr "f1e301"
 in pcon $ PMinting $ pdcons # pdata currSym # pdnil
```

The semantics are both are the same. But the former (`pconstant`) compiles to a constant term directly. Whereas the latter compiles to some code that _builds_ the constant during Plutus Core runtime.

> Aside: Remember that Haskell runtime is actually compile-time for Plutarch! Even if you have a dynamically computed variable in the Haskell world, it's still a _constant_ in the Plutarch world. So you can use it just as well as an argument to `pconstant`!

Whenever you need to build a Plutarch term of type `a`, from a Haskell value, use `pconstant`. Whenever you need to build a Plutarch term of type `PAsData a`, use `pconstantData`!
