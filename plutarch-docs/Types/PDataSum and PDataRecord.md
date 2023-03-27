<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PDataSumAndRecord (Foo (..), test) where 
import Plutarch.Prelude
```

</p>
</details>

# `PDataSum` & `PDataRecord`

Plutarch sum and product types are represented using `PDataSum` and `PDataRecord` respectively. These types are crucial to the [`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md) machinery.

Whenever you need to represent a non-trivial ADT using [`Data` encoding](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding), you'll likely be reaching for these.

More often than not, you'll be using `PDataRecord`. This is used to denote all the fields of a constructor:

```haskell
newtype Foo (s :: S) = Foo (Term s (PDataRecord '["fooField" ':= PInteger]))
```

`Foo` is a Plutarch type with a single constructor with a single field, named `fooField`, of type `PInteger`. You can 
[implement `PlutusType` via Data](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends) 
for it so that `PAsData Foo` is represented as a `Constr` encoded data value.

You can build `PDataRecord` terms using `pdcons` and `pdnil`. These are the familiar `cons` and `nil` but for `PDataRecord` terms.

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))

pdnil :: forall s. Term s (PDataRecord '[])
```

To add an `a` to the `PDataRecord` term, you must have a `PAsData a`. The other type variable of interest, is `label`. This 
is just the name of the field you're adding. You can either use type application to specify the field, or use a type annotation, 
or let GHC match up the types.

Here's how you'd build a `PDataRecord` with two integer fields, one is named `foo`, the other is named `bar`:

```haskell
test :: Term s (PDataRecord '[ "foo" ':= PInteger, "bar" ':= PInteger])
test = pdcons # pdata 7 #$ pdcons # pdata 42 # pdnil
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

represents a sum type with 2 constructors. The first constructor has two fields- `_0`, and `_1`, with types `PInteger` and `PByteString` respectively. 
The second constructor has one field- `myField`, with type `PBool`.

> Note: It's convention to give names like `_0`, `_1` etc. to fields that don't have a canonically meaningful name. They are merely the "0th field", "1st field" etc.

> Note: The underlying datatype for `PDataRecord`s and `PDataSum`s are builtin list and constr + builtin lists respectively.
