<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Tracing () where 
import Plutarch.Prelude ()
```

</p>
</details>

# Tracing

You can use the functions `ptrace`, `ptraceError`, `ptraceIfFalse`, `ptraceIfTrue` (from `Plutarch.Trace` or `Plutarch.Prelude`) for tracing. 
These behave similarly to the ones you're used to from [PlutusTx](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html).

If you have the `development` flag for `plutarch` turned on - you'll see the trace messages appear in the trace log during script evaluation. When not 
in development mode - these functions basically do nothing.
