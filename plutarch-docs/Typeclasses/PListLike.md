<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Plutarch.Docs.PListLike (pfoo, pfoo', x, x') where 
import Plutarch.Prelude
```

</p>
</details>

# `PListLike`

The `PListLike` typeclass bestows beautiful and familiar list utilities to its instances. Plutarch has two list types- [`PBuiltinList`](./../Types/PBuiltinList.md) and 
[`PList`](./../Types/PList.md). Both have `PListLike` instances! However, `PBuiltinList` can only contain builtin types. That means that, for example, it cannot contain 
Plutarch functions. The element type of `PBuiltinList` can be constrained using `PLift a => PBuiltinList a`.

As long as it's a `PLift a => PBuiltinList a` or `PList a` - it has access to all the `PListLike` goodies, out of the box. It helps to look into some of these functions 
at [`Plutarch.List`](https://github.com/Plutonomicon/plutarch/blob/master/Plutarch/List.hs).

Along the way, you might be confronted by 2 big mean baddies ...err, constraints:

```hs
PIsListLike list a
```

This just means that the type `list a`, is _indeed_ a valid `PListLike` containing valid elements! Of course, all `PList a`s are valid `PListLike`, but 
we have to think about `PBuiltinList` since it can only contain `PLift a => a` elements! So, in essence a function declared as:

```haskell
pfoo :: PIsListLike list a => Term s (list a :--> list a)
pfoo = undefined
```

when specialized to `PBuiltinList`, can be simplified as:

```haskell
pfoo' :: PLift a => Term s (PBuiltinList a :--> PBuiltinList a)
pfoo' = undefined
```

That's all it is. Don't be scared of it!

What about this one:

```hs
PElemConstraint list a
```

This one ensures that the element type `a` can indeed be contained within the list type - `list`. For `PList`, this constraint means nothing - it's always true. For `PBuiltinList`, it can be simplified as `PLift a`. Easy!

Here's two of my favorite `PListLike` utilities (not biased):

```hs
-- | Cons an element onto an existing list.
pcons :: PElemConstraint list a => Term s (a :--> list a :--> list a)

-- | The empty list
pnil :: PElemConstraint list a => Term s (list a)
```

What would life be without cons and nil?

Let's build a `PBuiltinList` of `PInteger`s with that:

```haskell
x :: Term s (PBuiltinList PInteger)
x = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```

Wooo! Let's not leave `PList` alone in the corner though:

```haskell
x' :: Term s (PList PInteger)
x' = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```

The code is the same, we just changed the type annotation. Cool!
