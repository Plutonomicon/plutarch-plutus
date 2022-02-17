This document describes the fundamental, commonly used Plutarch types.

Also see: [eDSL types in Plutarch](./CONCEPTS.md#edsl-types-in-plutarch)

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [`PInteger`](#pinteger)
- [`PBool`](#pbool)
- [`PString`](#pstring)
- [`PByteString`](#pbytestring)
- [`PUnit`](#punit)
- [`PBuiltinList`](#pbuiltinlist)
- [`PList`](#plist)
- [`PBuiltinPair`](#pbuiltinpair)
- [`PTuple`](#ptuple)
- [`PAsData`](#pasdata)
- [`PDataSum` & `PDataRecord`](#pdatasum--pdatarecord)
- [`PRecord`](#precord)
  - [letrec](#letrec)
  - [Record Data](#record-data)
- [`PData`](#pdata)

</details>

# `PInteger`

`Term s PInteger` has a convenient `Num` instance that allows you to construct Plutarch level integer terms from integer literals. It also means you have all the typical arithmetic operations available to you:

```haskell
1 + 2
```

where `1` and `2` are `Term s PInteger`s.

Alongside `Num`, it also has a `PIntegral` instance, allowing you to use division, modulus etc.

It also has a `PEq` and `POrd` instance, allowing you to do Plutarch level equality and comparison.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin integer](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:Integer).

# `PBool`

Plutarch level boolean terms can be constructed using `pconstant True` and `pconstant False`.

```haskell
pif (pconstant PFalse) 7 42
-- evaluates to 42
```

You can combine Plutarch booleans terms using `#&&` and `#||`, which are synonyms to `&&` and `||`. These are Haskell level operators and therefore have short circuiting. If you don't need short circuiting, you can use the Plutarch level alternatives- `pand'` and `por'` respectively.

This is synonymous to Plutus Core [builtin boolean](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool).

# `PString`

`Term s PString` has a `IsString` instance. This allows you to make Plutarch level string terms from regular string literals, provided you have `OverloadedStrings` turned on.

```haskell
{-# LANGUAGE OverloadedStrings #-}

"foo"
```

where "foo" is actually `Term s PString`.

It also has a `PEq` instance. And its terms have `Semigroup` and `Monoid` instances - which work the way you would expect.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin string](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinString) (actually Text).

# `PByteString`

Plutarch level bytestring terms can be created using `phexByteStr` and `pbyteStr`. `phexByteStr` interprets a hex string literal as a `Term s PByteString` and `pbyteStr` merely converts a `ByteString` into a `Term s PByteString`.

```haskell
import qualified Data.ByteString as BS

phexByteStr "41"
-- yields a `Term s PByteString`, which represents [65]

pbyteStr (BS.pack [91])
-- yields a `Term s PByteString`, which represents [91]
```

Similar to `PString`, it has a `PEq` instance. As well as `Semigroup` and `Monoid` instances for its terms.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin bytestring](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinByteString).

# `PUnit`

The Plutarch level unit term can be constructed using `pconstant ()`.

This is synonymous to Plutus Core [builtin unit](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinUnit).

# `PBuiltinList`

You'll be using builtin lists quite a lot in Plutarch. `PBuiltinList` has a [`PListLike`](./TYPECLASSES.md#plistlike) instance, giving you access to all the goodies from there! However, `PBuiltinList` can only contain builtin types. In particular, it cannot contain Plutarch functions.

You can express the constraint of "only builtin types" using `PLift`, exported from `Plutarch.Builtin`-\`

```hs
validBuiltinList :: PLift a => PBuiltinList a
```

As mentioned before, `PBuiltinList` gets access to all the `PListLike` utilities. Other than that, `PLift a => PBuiltinList a` also has a [`PlutusType`](./TYPECLASSES.md#plutustype-pcon-and-pmatch) instance. You can construct a `PBuiltinList` using `pcon` (but you should prefer using `pcons` from `PListLike`):

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

# `PList`

Here's the [Scott encoded](./CONCEPTS.md#scott-encoding) cousin of `PBuiltinList`. What does that mean? Well, in practice, it just means that `PList` can contain _any arbitrary_ term - not just builtin types. `PList` also has a [`PListLike`](#plistlike) instance - so you won't be missing any of those utilities here!

`PList` also has a [`PlutusType`](./TYPECLASSES.md#plutustype-pcon-and-pmatch) instance. You can construct a `PList` using `pcon` (but you should prefer using `pcons` from `PListLike`):

```hs
> pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```

would yield a `PList PByteString` with one element - `0xfe`. Of course, you could have done that with `pcons # phexByteStr "fe" # pnil` instead!

You can also use `pmatch` to match on a list:

```hs
pmatch (pcon $ PSCons (phexByteStr "fe") $ pcon PSNil) $ \case
  PSNil -> "hey hey there's nothing here!"
  PSCons _ _ -> "oooo fancy!"
```

But you should prefer `pelimList` from `PListLike` instead:

```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```

# `PBuiltinPair`

Much like in the case of builtin lists, you'll just be working with builtin functions (or rather, Plutarch synonyms to builtin functions) here. You can find everything about that in [builtin-pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md). Feel free to only read the `Plutarch` examples.

In particular, you can deconstruct `PBuiltinPair` using `pfstBuiltin` and `psndBuiltin`. You can build `PBuiltinPair (PAsData a) (PAsData b)` terms with `ppairDataBuiltin`:

```hs
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
```

It's also helpful to note that `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` actually have the same representation under the hood. See [`PTuple`](#ptuple)

# `PTuple`

These are [data encoded](./CONCEPTS.md#data-encoding) pairs. You can build `PTuple`s using `ptuple`:

```hs
ptuple :: Term s (PAsData a :--> PAsData b :--> PTuple a b)
```

`PTuple` has a [`PDataFields`](./TYPECLASSES.md#all-about-extracting-fields) instance. As such, you can extract its fields using `pletFields` or `pfield`.

Since `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` have the same representation - you can safely convert between them at no cost:

```hs
ptupleFromBuiltin :: Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b))) -> Term s (PAsData (PTuple a b))

pbuiltinPairFromTuple :: Term s (PAsData (PTuple a b)) -> Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b)))
```

# `PAsData`

This is a typed way of representing [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). It is highly encouraged you use `PAsData` to keep track of what "species" of `Data` value you actually have. `Data` can be a `Constr` (for sum of products - ADTs), `Map` (for wrapping assoc maps of Data to Data), `List` (for wrapping builtin lists of data), `I` (for wrapping builtin integers), and `B` (for wrapping builtin bytestrings).

Consider a function that takes in and returns a `B` data value - aka `ByteString` as a `Data` value. If you use the direct Plutarch synonym to `Data` - `PData`, you'd have:

```hs
foo :: Term s (PData :--> PData)
```

That's not very informative - you have no way to ensure that you're actually working with `B` data values. You could use `PAsData` instead:

```hs
foo :: Term s (PAsData PByteString :--> PAsData PByteString)
```

Now, you have assurance that you're working with a `Data` value that actually represents a builtin bytestring!

Wrapping and unwrapping to and from `PAsData` terms is provided by the [`PIsData`](./TYPECLASSES.md#pisdata) typeclass. Specifically, by the functions- `pfromData` and `pdata`.

Some useful instances of these functions:

```hs
pfromData :: Term s (PAsData PInteger) -> Term s PInteger

pfromData :: Term s (PAsData PByteString) -> Term s PByteString

pfromData :: Term s (PAsData (PBuiltinList (PAsData a))) -> Term s (PBuiltinList (PAsData a))

pdata :: Term s PInteger -> Term s (PAsData PInteger)

pdata :: Term s PByteString -> Term s (PAsData PByteString)

pdata :: Term s (PBuiltinList (PAsData a)) -> Term s (PAsData (PBuiltinList (PAsData a)))
```

# `PDataSum` & `PDataRecord`

Plutarch sum and product types are represented using `PDataSum` and `PDataRecord` respectively. These types are crucial to the [`PIsDataRepr`](./TYPECLASSES.md#pisdatarepr--pdatafields) machinery.

Whenever you need to represent a non-trivial ADT using [`Data` encoding](./CONCEPTS.md#data-encoding), you'll likely be reaching for these.

More often than not, you'll be using `PDataRecord`. This is used to denote all the fields of a constructor:

```hs
import Plutarch.Prelude

newtype Foo (s :: S) = Foo (Term s (PDataRecord '["fooField" ':= PInteger]))
```

`Foo` is a Plutarch type with a single constructor with a single field, named `fooField`, of type `PInteger`. You can [implement `PIsDataRepr`](./TYPECLASSES.md#implementing-pisdatarepr-and-friends) for it so that `PAsData Foo` is represented as a `Constr` encoded data value.

You can build `PDataRecord` terms using `pdcons` and `pdnil`. These are the familiar `cons` and `nil` specialized to `PDataRecord` terms.

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))

pdnil :: Term s (PDataRecord '[])
```

To add an `a` to the `PDataRecord` term, you must have a `PAsData a`. The other type variable of interest, is `label`. This is just the name of the field you're adding. You can either use type application to specify the field, or use a type annotation, or let GHC match up the types.

Here's how you'd build a `PDataRecord` with two integer fields, one is named `foo`, the other is named `bar`:

```hs
test ::
test = pdcons @"foo" @PInteger # 7 #$ pdcons @"bar" @PInteger # 42 # pnil
```

`PDataSum` on the other hand, is more "free-standing". In particular, the following type:

```hs
PDataSum
  [ '[ "_0" ':= PInteger
     , "_1" ':= PByteString
     ]
  , '[ "myField" ':= PBool
     ]
  ]
```

represents a sum type with 2 constructors. The first constructor has two fields- `_0`, and `_1`, with types `PInteger` and `PByteString` respectively. The second constructor has one field- `myField`, with type `PBool`.

> Note: It's convention to give names like `_0`, `_1` etc. to fields that don't have a canonically meaningful name. They are merely the "0th field", "1st field" etc.

# `PRecord`

You can define and use product ADTs, including records with named fields in Plutarch similar to Haskell's records. For a Haskell data type like

```hs
data Circle = Circle{
  x, y :: Integer,
  radius :: Natural
  }
```

the equivalent in Plutarch would be

```hs
data Circle f = Circle{
  x, y :: f PInteger,
  radius :: f PNatural
  }
Plutarch.Rec.TH.deriveAll ''Circle
```

Each field type needs to be wrapped into the type parameter `f` of kind `PType -> Type`. This is a slight modification of a common coding style known as Higher-Kinded Data.

With this definition, `PRecord Circle` will be an instance of [PlutusType](./TYPECLASSES.md#plutustype-pcon-and-pmatch), so you can use the usual `pcon` and `pcon'` to construct its value and `pmatch` and `pmatch'` to de-construct it:

```hs
circle :: Term s (PRecord Circle)
circle = pcon $ PRecord Circle{
  x = 100,
  y = 100,
  radius = 50
  }

distanceFromOrigin :: Term s (PRecord Circle :--> PNatural)
distanceFromOrigin = plam $ flip pmatch $ \(PRecord Circle{x, y})-> sqrt #$ projectAbs #$ x * x + y * y
```

You may also find `rcon` and `rmatch` from `Plutarch.Rec` a bit more convenient because they don't require the `PRecord` wrapper. Alternatively, instead of using `pmatch` or its alternatives you can access individual fields using the `field` accessor from the same module:

```hs
containsOrigin :: Term s (PRecord Circle :--> PBool)
containsOrigin = plam $ \c-> distanceFromOrigin # c #< pto c # field radius
```

## letrec

You can use records to define mutually-recursive functions, or more generally (but less usefully) mutually-recursive values.

```hs
circleFixedPoint :: Term s (PRecord Circle)
circleFixedPoint = punsafeFrom $ letrec $ \Circle{y, radius}-> Circle{
  x = y,
  y = 2 * radius,
  radius = 50
  }
```

## Record Data

You can provide a [`PIsData`](./TYPECLASSES.md#pisdata) instance for `PRecord Circle` using the following definition:

```hs
instance RecordFromData Circle
instance PIsData (PRecord Circle) where
  pfromData = readData $ recordFromFieldReaders Circle{
    x = DataReader pfromData,
    y = DataReader pfromData,
    radius = DataReader pfromData
    }
  pdata = writeData $ recordDataFromFieldWriters Circle{
    x = DataWriter pdata,
    y = DataWriter pdata,
    radius = DataWriter pdata
    }
```

If your record has many fields and you only need to a couple of them from `Data`, it's more efficient to use `pfromData` only on individual fields. You can focus on a single field using the function `fieldFromData`:

```hs
radiusFromCircleData :: Term s (PAsData (PRecord Circle) :--> PAsData PNatural)
radiusFromCircleData = fieldFromData radius
```

# `PData`

This is a direct synonym to [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). As such, it doesn't keep track of what "species" of `Data` it actually is. Is it an `I` data? Is it a `B` data? Nobody can tell for sure!

Consider using [`PAsData`](#pasdata) instead for simple cases, i.e. cases other than `Constr`.

Consider using [`PDataSum`/`PDataList`](#pdatasum--pdatarecord) instead when dealing with ADTs, i.e. `Constr` data values.

You can find more information about `PData` at [Developers' Corner](./DEVGUIDE.md).
