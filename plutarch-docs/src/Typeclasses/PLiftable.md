# `PLiftable`

## Prerequisites

You should be familiar with `PlutusType` and what it does, as well as how to
make instances of it. Furthermore, understanding how associated types and
`via`-deriving works is required. Lastly, familiarity with higher-rank arguments
is helpful, but not essential.

## Introduction

A `PlutusType` instance specifies two capabilities:

* Constructing a value by way of `pcon`; and
* Matching on a value by way of `pmatch`

This is enough as long as we remain entirely in the Plutarch universe. However, 
we often need to interact with Plutus stuff more
directly, and deal with equivalents to the types built-in to the Plutus default
universe (`Integer`, for example), as well as types that operate via a `Data`
encoding (such as most ledger stuff). We need to be able to create a 'bridge'
between the world that Plutus understands (Haskell, essentially) and Plutarch. 

`PLiftable` is designed to act as that bridge. If a type is an instance of
`PLiftable`, we have the following:

* A Haskell-level equivalent of this type
* A way of transforming a value of the Haskell-level equivalent into a Plutarch
  term
* A way of transforming a closed Plutarch term into a Haskell-level equivalent
  value (with the possibility of erroring)

## The type class

`PLiftable` is defined as follows:

```haskell
class PlutusType a => PLiftable (a :: S -> Type) where
    type AsHaskell a :: Type
    type PlutusRepr a :: Type
    toPlutarch :: forall (s :: S) . AsHaskell a -> PLifted a s
    toPlutarchRepr :: AsHaskell a -> PlutusRepr a
    fromPlutarch :: (forall (s :: S) . PLifted a s) -> Either LiftError (AsHaskell a)
    fromPlutarchRepr :: PlutusRepr a -> Maybe (AsHaskell a)
```

Even though we rarely need to interact with `PLiftable` and its methods
directly, it is worth understanding what exactly this type class requires from
its instances.

Firstly, we define an associated type `AsHaskell`, which determines the 'Haskell
equivalent' of the Plutarch type `a`. We can see which is which by looking at
the kinds involved: `a` has the kind `S -> Type` (meaning, 'Plutarch type'),
while `AsHaskell a` has the kind `Type` (meaning, 'Haskell type'). We also note
that any type with a `PLiftable` instance must also have a `PlutusType`
instance; this is not surprising, as we need some way of operating on whatever
we bring into Plutarch. 

In general, `AsHaskell a` must either be a type directly in the Plutus default
universe, or else a type which has a `Data` encoding. Nearly every case you are
likely to see, or define, will be one of these two. As a result, we provide two
helpers to derive such instances with less effort (further description of these
will come later).

We also define two methods: one for moving from the 'Haskell world' into the
'Plutarch world', and another for the opposite direction. We note that the
direction specified by `toPlutarch` is unconditional (cannot fail), whereas the
direction specified by `fromPlutarch` is conditional (can fail). This is because
a Plutarch term may represent a computation (valid or not), and to determine its
Haskell-equivalent value, we must compile and evaluate the term, which could
fail.

To fully grasp how these methods work, we need to examine two more types: the
`PLifted` wrapper, and the `LiftError` type:

```haskell
newtype PLifted (a :: S -> Type) (s :: S) = PLifted (Term s POpaque)

data LiftError
  = CouldNotEvaluate (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)
  | TypeError BuiltinError
  | CouldNotCompile Text
  | CouldNotDecodeData
```

`PLifted a s` is an implementation detail needed to drive `via`-derivation.
Whenever you see it, mentally substitute it for `Term s a`. Unless you plan to
write `PLiftable` instances by hand, you will never need to interact with this
type directly, or know anything about the distinction between `PLifted a s` and
`Term s a`. `LiftError` on the other hand designates all the ways in which
transforming a closed Plutarch term into its Haskell-level equivalent can fail:

* The term does not compile
* The evaluation of the term errors instead of giving a value
* We attempt to transform into a type not part of the Plutus default universe
* We evaluate to an invalid `Data` encoding

Once again, you almost never have to interact with this type directly unless
manually specifying an instance of this type class. The main advantage of having
a 'structured error type' here is debugging.

## Defining an instance

Aside from the manual method, we provide two `via`-deriving helpers, for the two
most common cases. We explain how to use each of them below. In almost all
situations, one of the two `via`-deriving helpers is what you want to use.

### Via `DeriveBuiltinPLiftable`

This helper is designed for types which have direct representations in Haskell
using a type that's part of the default Plutus universe. An example of such a
type is `PInteger`: `Term s PInteger` is meant to represent computations that
result in an `Integer` (or an error), and `Integer`s are part of the Plutus
default universe. Indeed, if you attempt to use this helper with a Haskell type
that _isn't_ part of the default Plutus universe, you will get a compile error.

The type is defined as follows: its implementation is not important.

```haskell
newtype DeriveBuiltinPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveBuiltinPLiftable (a s)
```

We can see that this `newtype` has two type arguments (besides the `s` to make
it a Plutarch type): one for the Plutarch type for which we want to derive the
instance, and one for a Haskell type that is meant to be its `AsHaskell`
equivalent. As an example of how to use this helper, consider the following:

```haskell
deriving via (DeriveBuiltinPLiftable PInteger Integer) 
    instance PLiftable PInteger
```

This specifies that `PInteger`'s Haskell-level equivalent is `Integer` by way of
its presence in the default Plutarch universe. 

### Via `DeriveDataPLiftable`

This helper is designed for types which are represented onchain by way of their
`Data` encoding, rather than being part of the Plutus universe directly. An
example of such a type is `PScriptContext`: its equivalent in Haskell is
`ScriptContext` from `plutus-ledger-api`, which is essentially a 'skin' over
`Data`.

This type is defined as follows: its implementation is not important.

```haskell
newtype DeriveDataPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveDataPLiftable (a s)
```

Similarly to `DeriveBuiltinPLiftable`, we have two relevant type arguments: 
one for the Plutarch type for which we want to derive an instance, and 
another for its Haskell-level equivalent. As an example of how to use this
helper, consider the following:

```haskell
deriving via (DeriveDataPLiftable PScriptContext ScriptContext) 
    instance PLiftable PScriptContext
```

This declares that `PScriptContext`'s Haskell-level equivalent is
`ScriptContext` (from `plutus-ledger-api`), by way of its `Data` encoding. 

There are additional requirements for using this helper with Plutarch type `a`
and Haskell-level equivalent `h`. Aside from `a` being an instance of
`PlutusType`, we also must have the following:

* `PInner a` is `PData` (namely, construction and matching is actually carried
  out in a computation involving `Data`)
* `h` is an instance of both `ToData` and `FromData` (namely, it has a `Data`
  encoding that we can decode from and encode into)

### Via `DeriveNewtypePLiftable`

This helper is for types that have the same representation as some other
type that already defined `PLiftable`. Instance defined that way will have
the same `PlutusRepr`.

```haskell
newtype PPositive s = PPositive (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

deriving via
  DeriveNewtypePLiftable PPositive PInteger Positive
  instance PLiftable PPositive
```

This defines that `PPositive`'s Haskell-level equivalent is `Positive` and `PPositive` has the same representation as `PInteger`.

Implementation is not important but is useful to talk about its type parameters

```haskell
newtype DeriveNewtypePLiftable (wrapper :: S -> Type) (inner :: S -> Type) (h :: Type) (s :: S)
  = DeriveNewtypePLiftable (wrapper s)
```

To use `DeriveNewtypePLiftable` the following must hold:

* `inner` has `PLiftable` instance
* `AsHaskell inner` is coercible to `h`

### Manual derivation

In the (unlikely) case that your type fits neither of the above, you will have
to write the instance manually. Given how unusual this situation is, we can't
really give any general guidance as to how this should be done. Instead, we
recommend examining the definitions of `PLiftable`, as well as the two
derivation helpers described above, in the source code. Alternatively, reach out
to us: we might be able to advise better.

### Ensuring your instance is correct

Any instance of `PLiftable` must obey the following laws:

```
1. fromPlutarch . toPlutarch = Right
2. fmap toPlutarch . fromPlutarch = Right
```

If you use either of `DeriveBuiltinPLiftable` or `DeriveDataPLiftable` with
`via` derivation, these laws will automatically be satisfied. If you write an
instance manually, you will have to ensure this yourself. We provide a helper
for testing that these laws hold in `plutarch-testlib`, in the
`Plutarch.Test.Laws` module, called `checkPLiftableLaws`, which uses QuickCheck
to verify that the laws are maintained. For example, to check that the instances
for `PLiftable PInteger` and `PLiftable PScriptContext` are correct, we would 
write:

```haskell
main :: IO ()
main = defaultMain . testGroup "Laws" $ [
    checkPLiftableLaws @PInteger,
    checkPLiftableLaws @PScriptContext
    ]
```

`checkPLiftableLaws` requires the use of a type argument, as it is otherwise
ambiguous. It also works largely by way of `AsHaskell`, which means that the
Haskell-level equivalent of the type being tested must be an instance of
`Arbitrary`, `Eq` and `Show`, as well as `Pretty`. The name of the type being
tested will automatically be added to the output of the test suite.

One important caveat to any definition of `PLiftable` instances, manual or not:
ensure that the Haskell-level equivalent that you declare is genuine. While
there is nothing stopping you from defining something like `PLiftable PNatural`
with `AsHaskell PNatural = Text`, this is clearly not sensible, and we cannot
check this. Your intent is taken at its word: the only checks are that what you
want is not literally impossible. Keep this in mind when defining your
instances.

## Using an instance

To simplify the use of `PLiftable`, we provide two functions:

```haskell
pconstant :: forall (a :: S -> Type) (s :: S) .
    PLiftable a =>
    AsHaskell a -> 
    Term s a

plift :: forall (a :: S -> Type) .
    PLiftable a => 
    (forall (s :: S) . Term s a) -> 
    AsHaskell a
```

The type signatures more-or-less speak for themselves: `pconstant` is the
Haskell-to-Plutarch direction, while `plift` is the Plutarch-to-Haskell
direction. There are three minor caveats to their use:

* `pconstant` is technically ambiguous, as many different Plutarch types can
  share the same choice for `AsHaskell`. A good example is that `PAsData
  PInteger` and `PInteger` would have the same `AsHaskell` (namely, `Integer`).
  Thus, you may need to use a type argument to avoid ambiguity errors from the
  compiler.
* `plift` transforms any `LiftError` into a call to `error`.
* `plift` requires a rank-2 argument for the Plutarch term (to ensure it's
  closed). This can occasionally confuse the compiler's inference when combined
  with `.` or similar operators: consider either manually bracketing, or using
  `ImpredicativeTypes`.

These functions are the main interface enabled by `PLiftable` that you should
use in your own code. `plift` is something that should be used fairly rarely
outside of testing, however: as it requires compilation and evaluation, it will
never be efficient.
