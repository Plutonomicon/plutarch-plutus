# Strictness and Laziness; Delayed Terms and Forcing

Plutarch, like UPLC, is strict by default; this is in contrast to Haskell, which is nonstrict. In practice, this means that calling a function in Plutarch evaluates _all_ arguments to the Plutarch lambda `Term` beforehand.

> Note: the below example does not correspond precisely to the implementation of `pif` or `pif'`; it is for didactic purposes only

This behavior may be undesirable, for example, when one of two `Term`s are branched upon within an `if` statement. The Plutarch level function `pif'` is naturally strict in its arguments - and therefore evaluate both branches before even entering the function body.

```hs
pif' :: Term s (PBool :--> b :--> b :--> b)
pif' = plam hif
```

A strict `if` is undesirable for the obvious reason: we only follow one branch at runtime, so it doesn't make sense to evaluate both before examining the `PBool` value to which we apply `pif`.

To avoid this, we use `pdelay` to create a "delayed `Term`." `pdelay` wraps the `PType` tag of a term, overriding the default strict behavior and indicating that the term should _not_ be evaluated immediately. `pdelay` has the following type:

```hs
pdelay :: Term s a -> Term s (PDelayed a)
```

A delayed term evaluates when it is _forced_ using the `pforce` function. Forcing a term strips the `PDelayed` wrapper:

```hs
pforce :: Term s (PDelayed a) -> Term s a
```

Thus, if we wanted a lazy `pif`, we could do the following:

```hs
-- | Utilizing Haskell level functions with `pdelay` and `pforce` to have lazy wrapper around `pif`.
hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse
```

A note of caution: calling `pforce` on the same delayed term twice will execute the computation each time. Users familiar with Haskell's handling of laziness -- where forcing a thunk twice never duplicates computation -- should note that UPLC behaves differently.

Finally, readers should note that `pdelay` and `pforce` are extremely powerful tools when writing Plutarch scripts and are encouraged to familiarize themselves accordingly.
