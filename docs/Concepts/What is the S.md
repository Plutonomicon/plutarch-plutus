# What is the `s`?

The `s` essentially represents the context, and is like the `s` of `ST`.

It's used to distinguish between closed and open terms:

- Closed term: `type ClosedTerm = forall s. Term s a`
- Arbitrary term: `exists s. Term s a`
- NB: `(exists s. Term s a) -> b` is isomorphic to
- `forall s. Term s a -> b`
