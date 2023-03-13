<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Plutarch.Docs.PBuiltinList (validBuiltinList, listOfBs, matchOnList, matchOnList') where 
import Plutarch.Prelude
```

</p>
</details>

# `PBuiltinList`

You'll be using builtin lists quite a lot in Plutarch. `PBuiltinList` has a [`PListLike`](./../Typeclasses/PListLike.md) instance, giving you access to all the goodies from there! 
However, `PBuiltinList` can only contain builtin types. In particular, it cannot contain Plutarch functions (which also implies it cannot contain scott-encoded datatypes).

You can express the constraint of "only builtin types" using `PLift`, exported from `Plutarch.Builtin`-

```haskell
validBuiltinList :: forall a s. PLift a => Term s (PBuiltinList a)
validBuiltinList = undefined
```

As mentioned before, `PBuiltinList` gets access to all the `PListLike` utilities. Other than that, `PLift a => PBuiltinList a` also has a 
[`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md) instance. You can construct a `PBuiltinList` using `pcon` (but you should prefer using `pcons` from `PListLike`):

```haskell
listOfBs :: forall s. Term s (PBuiltinList (PAsData PByteString))
listOfBs = pcon $ PCons (pdata $ phexByteStr "fe") $ pcon PNil
```

would yield a `PBuiltinList (PAsData PByteString)` with one element - `0xfe`. Of course, you could have done that with `pcons # pdata (phexByteStr "fe") # pnil` instead!

You can also use `pmatch` to match on a list:

```haskell
matchOnList :: forall s. Term s PString
matchOnList = pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
```

But you should prefer `pelimList` from `PListLike` instead:

```haskell
matchOnList' :: forall s. Term s PString
matchOnList' = pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PCons (phexByteStr "fe") $ pcon PNil
```

The first argument is a function that is invoked for the `PCons` case, with the head and tail of the list as arguments.

The second argument is the value to return when the list is empty. It's _only evaluated_ **if the list is empty**.

The final argument is, of course, the list itself.

> Aside: Interested in the lower level details of `PBuiltinList` (i.e. Plutus Core builtin lists)? You can find all you need to 
> know about it at [Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md).
