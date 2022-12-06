<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.Unsafe () where 
import Plutarch.Prelude ()
```

</p>
</details>

# Unsafe functions

There are internal functions such as `punsafeCoerce`, `punsafeConstant` etc. that give you terms without their specific type. 
These **should not** be used by Plutarch users. It is the duty of the user of these unsafe functions to get the type right - 
and it is very easy to get the type wrong. You can easily make the type system believe you're creating a `Term s PInteger`, 
when in reality, you created a function.

Things will go very wrong during script evaluation if you do that kind of thing.

The good thing is that unsafe functions all have explicit indicators through the names, as long as you don't use any `punsafe*` 
functions - you should be fine! 

> Note: unsafe functions are exported by the `Plutarch.Unsafe` module

Of course, these have legitimate use cases. Most often, we use these functions to convert between types that _truly_ have the same 
internal representation in UPLC - but the type system simply isn't expressive enough to infer that.
