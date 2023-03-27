<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PRecord () where 
import Plutarch.Prelude ()
```

</p>
</details>

# `PRecord`

You can define and use product ADTs, including records with named fields in Plutarch similar to Haskell's records. For a Haskell data type like

```haskell
data Circle 
  = Circle 
  { x, y :: Integer
  , radius :: Natural
  }
```

the equivalent in Plutarch would be

```hs
data PCircle f = PCircle{
  x, y :: f PInteger,
  radius :: f PNatural
  }
Plutarch.Rec.TH.deriveAll ''Circle
```

Each field type needs to be wrapped into the type parameter `f` of kind `PType -> Type`. This is a slight modification of a common coding style known as Higher-Kinded Data.

With this definition, `PRecord Circle` will be an instance of [PlutusType](../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md), so you can use the usual `pcon` and `pcon'` to construct its value and `pmatch` and `pmatch'` to de-construct it:

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

You can provide a [`PIsData`](../Typeclasses/PIsData.md) instance for `PRecord Circle` using the following definition:

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
