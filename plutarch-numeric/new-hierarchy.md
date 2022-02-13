# New Plutarch numeric hierarchy

## Types

### `Integer`

Built-in (the _only_ one)! Essentially Z.

### `Natural`

Essentially N.

### `NZNatural`

Essentially N, but without 0.

### `NZInteger`

Essentially Z, but without 0.

### `Ratio`

Higher-kinded type representing every manner of ratio. The type parameter is
meant to be 'filled-in' with one of `Integer`, `Natural`, `NZInteger` or
`NZNatural`, depending on what kind of ratio we want. This would be effectively
`data Ratio a = Ratio a NZNatural`, with the added guarantee that `a` and
`NZNatural` are in lowest common terms.

## Type classes

### `Fractionable`

class (Eq a) => Fractionable a where
  scale :: NZNatural -> a -> a
  unscale :: NZNatural -> a -> a
  gcd :: a -> NZNatural -> NZNatural
```

The following laws must hold:

1. `scale one = unscale one = id`
1. `scale (n * n') = scale n . scale n'`
1. `unscale n . scale n = id`
1. `gcd x one = one`
1. `gcd (scale x m) (m * n) = m * gcd x n`
1. if `m = gcd x n`, then `gcd (unscale x m) m = 1`

Instances:

* `Fractionable Integer`
* `Fractionable Natural`
* `Fractionable NZInteger`
* `Fractionable NZNatural`

**Koz note:** This is what allows a given type to act as the numerator in a
`Ratio`.

### `AdditiveSemigroup`

```haskell
class (Eq a) => AdditiveSemigroup a where
  {-# MINIMAL (+) #-}
  (+) :: a -> a -> a
  scaleNZNatural :: a -> NZNatural -> a
```

`(+)` must commute and associate. Furthermore, `scaleNZNatural` must obey the
following recurrence:

```
scaleNZNatural x one = x
scaleNZNatural x n = x + scaleNZNatural x (n ^- one)
```

Lastly, if `a` is also `Fractionable`, `scale n x = scaleNZNatural n x`.

Instances:

* `AdditiveSemigroup Natural`
* `AdditiveSemigroup Integer`
* `AdditiveSemigroup NZNatural`
* `(AdditiveSemigroup a, Fractionable a) => AdditiveSemigroup (Ratio a)`

**Koz note:** `NZInteger` being excluded from this is a real pain, because it
means that both it, and `Ratio NZInteger` cannot participate in most of the
hierarchy. We can't avoid this though, as there's no way to combine closure of
`(+)`, additive inverses _and_ not having `zero`.

### `MultiplicativeSemigroup`

```haskell
class (Eq a) => MultiplicativeSemigroup a where
  {-# MINIMAL (*) #-}
  (*) :: a -> a -> a
  powNZNatural :: a -> NZNatural -> a
```

`(*)` must associate (but not necessarily commute). Furthermore, `powNZNatural`
must obey the following recurrence:

```
powNZNatural x one = x
powNZNatural x n = x * powNZNatural x (n ^- one)
```

Instances:

* `MultiplicativeSemigroup Natural`
* `MultiplicativeSemigroup Integer`
* `MultiplicativeSemigroup NZNatural`
* `MultiplicativeSemigroup NZInteger`
* `(MultiplicativeSemigroup a, Fractionable a) => MultiplicativeSemigroup (Ratio a)`

### `NonZero`

```haskell
class (Eq a, Eq b) => NonZero a b | a -> b where
  toNonZero :: a -> Maybe b
  fromNonZero :: b -> a
```

`toNonZero` and `fromNonZero` must form a partial isomorphism; specifically,
`toNonZero . fromNonZero = Just`. 

Instances:

* `NonZero Integer NZInteger`
* `NonZero Natural NZNatural`
* `NonZero NZInteger NZInteger`
* `NonZero NZNatural NZNatural`
* `(NonZero a b) => NonZero (Ratio a) (Ratio b)`

**Koz note:** Does this require supplemental laws?

### `AdditiveMonoid`

```haskell
class (AdditiveSemigroup a) => AdditiveMonoid a where
  {-# MINIMAL zero #-}
  zero :: a
  abs :: a -> a
  scaleNatural :: a -> Natural -> a
```

`zero` must be a left and right identity of `(+)`; specifically, for any `x`, `x
+ zero = zero + x = x`. Furthermore, `scaleNatural` must obey the following
recurrence: 

```
scaleNatural x zero = zero
scaleNatural x n = x + scaleNatural x (n ^- one)
```

Instances:

* `AdditiveMonoid Natural`
* `AdditiveMonoid Integer`
* `(AdditiveMonoid a, Fractionable a) => AdditiveMonoid (Ratio a)`

### `MultiplicativeMonoid`

```haskell
class (MultiplicativeSemigroup a) => MultiplicativeMonoid a where
  {-# MINIMAL one, abs, signum #-}
  one :: a
  abs :: a -> a
  signum :: a -> a
  powNatural :: a -> Natural -> a
```

`one` must be a left and right identity of `(*)`; specifically, for any `x`, 
`x * one = one * x = x`. Furthermore, `powNatural` must obey the following recurrence:

```
powNatural x zero = one
powNatural x n = x * powNatural x (n ^- one)
```

Lastly, `abs` and `signum` must follow the following laws:

* `abs x * signum x = x`
* `abs one = signum one = one`
* `abs x * abs y = abs (x * y)`

Instances:

* `MultiplicativeMonoid Natural`
* `MultiplicativeMonoid Integer`
* `MultiplicativeMonoid NZNatural`
* `MultiplicativeMonoid NZInteger`
* `(MultiplicativeMonoid a, Fractionable a) => MultiplicativeMonoid (Ratio a)`

**Koz note:** This definition allows us to define `isPositive :: forall (a ::
Type) . (MultiplicativeMonoid a) => a -> Bool`, for example.

### `AdditiveGroup`

```haskell
class (AdditiveMonoid a) => AdditiveGroup a where
  {-# MINIMAL negate #-}
  (-) :: a -> a -> a
  negate :: a -> a
  scaleInteger :: a -> Integer -> a 
```

`negate` must construct the additive inverse of its argument: thus, `x + negate
x = zero`. If we define `(-)`, it must be the case that, for all `x` and `y`, `x
- y = x + negate y`. Lastly, `scaleInteger` must obey the following laws:

* `scaleInteger x zero = zero`
* `scaleInteger x one = x`
* If `n` is positive, `scaleInteger x n = x + scaleInteger x (n - one)`
* If `n` is negative, `scaleInteger x n = negate . scaleInteger x . negate $ n`

Instances:

* `AdditiveGroup Integer`
* `(AdditiveGroup a, Fractionable a) => AdditiveGroup (Ratio a)`

### `AdditiveCMM`

**Koz note:** 'CMM' is short for 'commutative monoid with monus'.

```haskell
class (AdditiveMonoid a) => AdditiveCMM a where
  (^-) :: a -> a -> a
```

**Koz note:** `^-` is pronounced 'monus'.

The following must all hold for all `x`, `y` and `z`:

* `x + (y ^- x) = y + (x ^- y)`
* `(x ^- y) ^- z = x ^- (y + z)
* `x ^- x = zero`
* `zero ^- x = zero`

Instances:

* `AdditiveCMM Natural`
* `(AdditiveCMM a, Fractionable a) => AdditiveCMM (Ratio a)`

**Koz note:** You can't be both an `AdditiveGroup` and an `AdditiveCMM`: a CMM
must be canonically-ordered, and if you're canonically-ordered, you can't have
inverses (Gondran and Minoux).

### `MultiplicativeGroup`

```haskell
class (MultiplicativeMonoid a, MultiplicativeMonoid b, NonZero a b) => 
  MultiplicativeGroup a b | a -> b where
    {-# MINIMAL reciprocal #-}
    (/) :: a -> b -> a
    reciprocal :: b -> b -> b
    powInteger :: a -> Integer -> a
```

For any `x`, `reciprocal x * x = one`. Furthermore, for any `x` and `y`, it must
be the case that `x / y = x * (fromNonZero . reciprocal $ y)`. Lastly,
`powInteger` must obey the following laws:

* `powInteger x zero = one`
* `powInteger x one = x`
* If `toNonZero x = Nothing`, then `powInteger x n = x` (unless `n = zero`, in
  which case it's `one`).
* If `toNonZero x = Just y` and `n` is positive, then `powInteger x n = x *
  powInteger x (n - one)`.
* If `toNonZero x = Just y` and `n` is negative, then `powInteger x n =
  fromNonZero . powNatural (reciprocal y) . abs' $ n`

**Koz note:** This is why we want `NonZero` and its attendant complexities: it
makes these operations total with reduced friction to, for example, always being
in `Maybe` or having your code blow up periodically. The laws are a bit awkward
though.

Instances:

* `(NonZero a b, Fractionable a) => MultiplicativeGroup (Ratio a) (Ratio b)`

### `Semirig`

**Koz note:** This is a cutesy way of saying 'semiring without a neutral element
for addition'.

```haskell
class (AdditiveSemigroup a, MultiplicativeMonoid a) => Semirig a where
  fromNZNatural :: NZNatural -> a
```

Multiplication left and right must distribute over addition; specifically, for
all `x`, `y` and `z`:

* `x * (y + z) = (x * y) + (x * z)`
* `(x + y) * z) = (x * z) + (y * z)`

Furthermore, `fromNZNatural` must be the unique semirig homomorphism from
`NZNatural` to `a`. Thus, the following must hold for all `x` and `y`:

* `fromNZNatural one = one`
* `fromNZNatural (x + y) = fromNZNatural x + fromNZNatural y`

**Koz note:** It is exactly this homomorphism that allows us to have constants
of various types all use the same syntax. If you can write it as an `NZNatural`,
we can get the equivalent value in `a`, and this _must_ be unique.

Furthermore, we can support decimal notation as well: any decimal, such as
`0.045` can be thought of as a literal (with possibly extra zeroes) of the
'numerator type' with a denominator of 10 to some non-negative power.

Instances:

* `Semirig Integer`
* `Semirig Natural`
* `Semirig NZNatural` (this is the 'canonical semirig')
* `(Semirig a, Fractionable a) => Semirig (Ratio a)`

### `Semiring`

```haskell
class (AdditiveMonoid a, Semirig a) => Semiring a where
  fromNatural :: Natural -> a
```

`zero` must annihilate multiplication left and right; thus, for all `x`, `x *
zero = zero * x = zero`. Additionally, `fromNatural` must be the unique semiring
homomorphism from `Natural` to `a`. Thus, the following must hold for all `x`
and `y`:

* `fromNatural zero = zero`
* `fromNatural one = one`
* `fromNatural (x + y) = fromNatural x + fromNatural y`

**Koz note:** Similar to the above, but because we have zero, we can do more.
This allows us to eliminate `fromNatural`-type functions from `NatRatio`, since
this can just come from syntax. The same applies to decimal notation too.

Instances:

* `Semiring Integer`
* `Semiring Natural` (this is the 'canonical semiring')
* `(Semiring a, Fractionable a) => Semiring (Ratio a)`

### `Ring`

```haskell
class (AdditiveGroup a, Semiring a) => Ring a where
  fromInteger :: Integer -> a
```

`fromInteger` must be the unique ring homomorphism from `Integer` to `a`. Thus,
the following must hold for all `x` and `y`:

* `fromInteger zero = zero`
* `fromInteger one = one`
* `fromInteger (negate x) = negate (fromInteger x)`
* `fromInteger (x + y) = fromInteger x + fromInteger y`

**Koz note:** This is the basis of `Num`'s syntax, except with actual rules.
Also `Fractional`'s syntax.

Instances:

* `Ring Integer`
* `(Ring a, Fractionable a) => Ring (Ratio a)`

### `Euclidean`

**Koz note:** This is a generalization of Euclidean domains, but without
requiring a ring.

```haskell
class (Semiring a, MultiplicativeMonoid b, Ord b, NonZero a b) => Euclidean a b | a -> b where
  divMod :: a -> b -> (a, a)
```

For all `x` and `y`, if `divMod x y = (q, r)`, then `(q * fromNonZero y) + r =
x`. Additionally, if `toNonZero r = Just r'`, then `abs r' <= abs y`.

Instances:

* `Euclidean Integer NZInteger`
* `Euclidean Natural NZNatural`

**Koz note:** Again, for obnoxious reasons, we can't include `NZInteger` and
`NZNatural` themselves in this, even though it totally makes sense to. 

This also possibly relates to `Fractionable` somehow?
