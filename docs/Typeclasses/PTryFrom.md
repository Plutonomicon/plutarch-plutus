# `PTryFrom`

```haskell
class PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  ptryFrom :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
```

`PTryFrom` is a typeclass to prove equality between a type that in some way can't be trusted about its representation and another type that we want the untrusted type to be represented as. 
`PTryFrom` proves the structure of the untrusted type and recovers it as the trusted, type which hence also carries more information. 

A good example is getting a `PData` from a redeemer and wanting to prove that it is of a certain kind, e.g. a `PAsData (PBuiltinList (PAsData PInteger))`. We could do this with: 

```haskell
recoverListFromPData opq = unTermCont $ fst <$> ptryFromData @(PAsData (PBuiltinList (PAsData PInteger)) opq
```

As you can see, it uses the utility function `ptryFromData @b`, which does the same as `ptryFrom @PData @b`. 

## Laws

- the operation `ptryFrom` mustn't change the representation of the underlying data
- the operation `ptryFrom` must always prove the integrity of the whole target type
   - example:
       `ptryFrom @PData @(PAsData (PBuiltinList PData))` ssucceeds iff the underlying representation is a `BuiltinList` containing any `PData`
- all conversions are fallible, this happens if the representation doesn't match the expected type.
- the operation `ptryFrom @a @b` proves equality between the less expressive `PType` `a` and the more expressive `PType` `b`, hence the first 
     element of the resulting Tuple must always be wrapped in `PAsData` if the origin type was `PData` (see law 1)
- the result type `b` must always be safer than the origin type `a`, i.e. it must carry more information

## `PTryFromExcess`

An important note is, that `PTryFrom` carries a type `PTryFromExcess` which safes data that arose as "excess" during the act of verifying. For `PData (PAsData PSomething)` instances this most times 
carries a `PSomething`, i.e. the type that has been proven equality for but without `PAsData` wrapper. In cases where this type is not useful, the excess type is just an empty `HRec`.

In case of the recovered type being a record or anything that contains a record, the excess type is more interesting: 
It contains an `HRec`, that has all the fields that have been recoverd and all *their* excess stored. If you recover a `PAsData (PDataRecord xs)` from `PData`, there is another field under the accessor `"unwrapped"` that contains the unwrapped record, which representation wise is just a `PBuiltinList
PData`, of course. 

Generally, when recovering a `PDataRecord`, the procedure is as follows

```haskell
untrustedRecord :: Term s PData
untrustedRecord =
  let rec :: Term s (PAsData (PDataRecord '["_0" ':= (PDataRecord '["_1" ':= PInteger])]))
      rec = pdata $ pdcons # (pdata $ pdcons # pdata (pconstant 42) # pdnil) # pdnil
   in pforgetData rec

-- obviously, `untrustedRecord` would be what we get from our untrusted party

theField :: Term s PInteger
theField = unTermCont $ do
  (_, exc) <- tcont (ptryFromData @(PAsData (PDataRecord '["_0" ':= (PDataRecord '["_1" ':= PInteger])])) untrustedRecord)
  pure $ exc._0._1
```

Because the record excess stores the field already in its unwrapped form, you don't have to `pfromData` it again. 

If you don't use `OverloadedRecordDot`, there is an equivalent function `getExcessField` that does the same and works with type applications. 

## Recovering only partially

In case we don't want to verify the whole structure but rather part of it (this can be a reasonable decision to lower the fees), we can just leave the part of the data that is not to be 
verified a `PData` which serves as the base case: 

```haskell
recoverListPartially = ptryFrom @PData @(PAsData (PBuiltinList PData))
```

This is especially important with something like `PDataSum` which simply cannot store the excess types over the barrier of `pmatch` because obviously,
you don't know the type of the excess before actually matching on it. The solution would be to recover an equivalent `PDataSum` whose constructors
contain `PData` and after having matched on those, recover the underlying record or whatever field you're interested in. If you're not interested 
in the excess, you could of course also just recover the whole Sum without issue, in this case it won't be more expensive.

Please be aware, that nuances can already make a performance difference, e.g.
- recovering `ptryFromData @(PAsData (PBuiltinList PData))` is cheaper than `ptryFromData @(PAsData (PBuiltinList (PAsData PDAta)))` because the latter
  maps over no-ops, whereas the former just asserts that the `PData` indeed contains a `PBuiltinList`. 
- If you only, say, need the head of a list, first recovering a `PAsData (PBuiltinList PData)` (don't forget to use the excess instead of using
  `pfromData`), *then* using head and after that recovering the field in there will be cheaper than recovering the whole list with the target type and
  using head on that.

