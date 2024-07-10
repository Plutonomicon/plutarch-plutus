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

Fundamentally, there are two kinds of traces Plutarch can add to your code:

* Info tracing, which is the 'regular' kind of tracing; and
* Debug tracing, which is supposed to be more verbose and provide more detail
  for debugging purposes.

The basic way you can add traces to your code is using `ptraceInfo` to add an
info trace, and `ptraceDebug` to add a debug trace. `Plutarch.Trace` (and
`Plutarch.Prelude`) export additional functions for more specific cases,
including ones that include use of `PShow`, and that only inject a trace under
certain conditions. See those modules, and their documentation, for more
details.

If you have the `development` flag for `plutarch` turned on - you'll see the 
trace messages appear in the trace log during script evaluation. When not 
in development mode, these functions basically do nothing.

# Important note

Use of `PShow` is strongly discouraged in any tracing code (and any code in
general). This uses a lot of resources onchain, and can easily exhaust script
limits unless done carefully. In general, only resort to `PShow` if you
absolutely have to: otherwise, prefer static strings as outputs.
