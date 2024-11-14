> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [No instance for `PUnsafeLiftDecl a`](#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `getField`, or `pletFields`)](#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-getfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](#lifting-pasdata)
- [Couldn't match type `PLifted (PConstanted Foo)` with `Foo`](#couldnt-match-type-plifted-pconstanted-foo-with-foo)
- [Type match errors when using `pfield`/`getField` (or `OverloadedRecordDot` to access field)](#type-match-errors-when-using-pfieldgetfield-or-overloadedrecorddot-to-access-field)

</details>

# No instance for `PUnsafeLiftDecl a`

You should add `PLift a` to the context! `PLift` is just a synonym to `PUnsafeLiftDecl`.

# Couldn't match representation of type: ... arising from the 'deriving' clause

If you're getting these errors when deriving typeclasses using the machinery provided by Plutarch 
(e.g. generic deriving of `PlutusType`) - it means you're missing a constructor import.

# Infinite loop / Infinite AST

While may not be immediately obvious, things like the following are a no-go in Plutarch:

```haskell
f :: Term s (PInteger :--> PInteger)
f = phoistAcyclic $ plam $ \n ->
  pif (n #== 0)
    0
    (n + f # (n - 1))
```

The issue here is that the AST is infinitely large. Plutarch will try to traverse this AST and will in
the process not terminate, as there is no end to it. In these cases, consider using `pfix`.

Relevant issue: [\#19](https://github.com/Plutonomicon/plutarch/issues/19)

# Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield`, `getField`, 
  `pletFields`, `hrecField` (deprecated)

You might get some weird errors when using `pfield`/`getField`/`pletFields` like the above. Don't be scared! It just 
means that the type application you used is incorrect. Specifically, the type application names a non-existent field. 
Re-check the field name string you used in the type application for typos!

# Expected a type, but "fieldName" has kind `GHC.Types.Symbol`

This just means the argument of a type application wasn't correctly promoted. Most likely arising from a usage of 
`pletFields`. In the case of `pfield` and `getField`, the argument of type application should have kind `Symbol`. 
A simple string literal representing the field name should work in this case. In the case of `pletFields`, the 
argument of type application should have kind `[Symbol]` - a type level list of types with kind `Symbol`. When you use 
a singleton list here, like `["foo"]` - it's actually parsed as something of *kind* `Type` (like `[a]`).

All you need to do, is put a `'` (quote) in front of the list, like so- `@'["foo"]`. This will promote the list 
constructor to the type level.

# Lifting `PAsData`

Don't try to lift a `PAsData` term! It's intentionally blocked and partial. The `PLift` instance for `PAsData` is 
only there to make some important functionality work correctly. But the instance methods will simply `error` if used. 
Instead, you should either use `pforgetData` and `plift` that, or extract the `Term s a` out of `Term s (PAsData a)` 
using `pfromData` and `plift` that instead!

# Couldn't match type `PLifted (PConstanted Foo)` with `Foo`

`PLifted (PConstanted h)` should always just be `h` - right? What's this then?

Orphan instances! Specifically, in order for those type family applications to fully compute (and yield `h`), 
you need the `PConstant` instance for `h` in scope, as well as the `PLift` instance for the corresponding Plutarch type.
Recall that `h` here is a Haskell type - its corresponding `PConstant` instance is _probably_ an orphan instance that 
you haven't imported.

This happens often with Plutarch ledger API types. If you didn't import `Plutarch.Api.V1.Contexts` (or some other
module that imports it), and you're using `pconstant` on a `ScriptContext` - you'll get an error like this. The 
`PConstant` instance for `ScriptContext` hasn't been imported - so GHC has no idea what `PConstanted ScriptContext` is!

Relevant issue: [\#252](https://github.com/Plutonomicon/plutarch/issues/252)

# Type match errors when using `pfield`/`getField` (or `OverloadedRecordDot`, or `hrecField`(deprecated)) to access field

You might get nonsensical "Couldn't match type" errors when extracting fields. This has to do with GHC 
incorrectly inferring the return type. Field extraction is meant to be polymorphic in its return type in the sense that 
it might either return a `Term s (PAsData p)` term, or simply a `Term s p` (automatic `pfromData`). Unfortunately, 
sometimes this polymorphism makes it harder for GHC to infer the types. Also be aware that this automatic `pfromData`
will infer `PAsData`d terms more eagerly.

You can fix this by providing an explicit type annotation on _the result_ of `pfield` or `getField` (or 
`OverloadedRecordDot` for field access). Otherwise, you can also explicitly use `pfromData` on the result.
This will also help to make code more readable and is generally a good idea as plutarch validators and policies
tend to get very long and consequently confusing without explicit type annotations on the bound terms.

Relevant issue: [\#275](https://github.com/Plutonomicon/plutarch/issues/275)
