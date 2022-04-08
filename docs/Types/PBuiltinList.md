# `PBuiltinList`

You'll be using builtin lists quite a lot in Plutarch. `PBuiltinList` has a [`PListLike`](./../Typeclasses/PListLike.md) instance, giving you access to all the goodies from there! However, `PBuiltinList` can only contain builtin types. In particular, it cannot contain Plutarch functions.

You can express the constraint of "only builtin types" using `PLift`, exported from `Plutarch.Builtin`-

```hs
validBuiltinList :: PLift a => PBuiltinList a
```

As mentioned before, `PBuiltinList` gets access to all the `PListLike` utilities. Other than that, `PLift a => PBuiltinList a` also has a [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md) instance. You can construct a `PBuiltinList` using `pcon` (but you should prefer using `pcons` from `PListLike`):

```hs
> pcon $ PCons (phexByteStr "fe") $ pcon PNil
```

would yield a `PBuiltinList PByteString` with one element - `0xfe`. Of course, you could have done that with `pcons # phexByteStr "fe" # pnil` instead!

You can also use `pmatch` to match on a list:

```hs
pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
```

But you should prefer `pelimList` from `PListLike` instead:

```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PCons (phexByteStr "fe") $ pcon PNil
```

The first argument is a function that is invoked for the `PCons` case, with the head and tail of the list as arguments.

The second argument is the value to return when the list is empty. It's _only evaluated_ **if the list is empty**.

The final argument is, of course, the list itself.

> Aside: Interested in the lower level details of `PBuiltinList` (i.e. Plutus Core builtin lists)? You can find all you need to know about it at [Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md).
