# Plutarch Types

When this guide uses the term "Plutarch Type" we explicitly talk about a type of _kind_ `S -> Type`. 

> Note to beginners: Plutarch uses a language extension called `DataKinds`. This means that there are kinds beyond `Type` (aka `*`). We refer the read to \[[3](./../Introduction.md#references)] for an extended beginner-level introduction to these concepts if desired.
```

Types of kind `S -> Type` should be considered as _tags_ on computation. They do not represent types of values in the same way as standard Haskell types.

The kind of basic types such as `Integer` in Haskell has the kind: `Type`; the corresponding "basic" kind in Plutarch is simply `S -> Type`. Higher-kinded types in Haskell, such as `Maybe`, will have kinds such as `Type -> Type`. In Plutarch, the corresponding kind is:

```hs
ghci> :k PMaybe
PMaybe :: (S -> Type) -> S -> Type
```

The kind `S -> Type` is mysterious at first, but we recall that `S -> Type`s are _tags_ on (unexecuted) computations indicating their result type. The `S` kind represents the computational context; thus, a `S -> Type` expects to receive a _computational context_ represented by a value `s` whose type has kind `S` that it will tag to produce a `Type`. Note that end-users never instantiate the value `s` with a concrete value; it is simply a type-level mechanism to maintain functional purity.

The above notion is essential to understanding why not all Plutarch types (`S -> Type`s) have data constructors; the data constructors are irrelevant, except insofar as they enable the implementation to keep track of Haskell-level and UPLC-level representations. `PInteger` is one such case; it is impossible to construct a constant `y` where `y :: PInteger s`. Other Plutarch types, such as `PMaybe`, _do_ have data constructors (specifically `PJust` and `PNothing`), but _still_ do not carry data from the viewpoint of UPLC. A value such as `PNothing` merely facilitates convenient term construction and deconstruction. When `pcon` sees `PNothing`, it knows it should build a UPLC constant that is _morally_ equivalent to the concept of `Nothing :: Maybe a`.

In general, the concrete UPLC representations are connected to Plutarch types through their `PlutusType` implementation.

Also see: [Figuring out the representation of a Plutarch type](./../Tricks/RepresentationOfPlutarchType.md).
