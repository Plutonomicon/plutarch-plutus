# Untyped Plutus Core (UPLC)

Plutarch compiles to UPLC. Most Plutarch end-users will not need to concern themselves with the details of UPLC, but a brief overview will aid in building a mental model of how Plutarch works.

Unlike Haskell, UPLC is a low-level and untyped language implementing a basic lambda calculus. Consequently, it supports only a handful of built-in values and functions which may be strung together in lambda applications. The built-in types provided by UPLC include the usual primitive types -- integers, byte strings and strings, booleans, and so forth -- and a special `Data` value that can encode representations of arbitrary sum-of-products Haskell types.

While the _semantic_ meaning of a Haskell type such as `Maybe Integer` is missing in UPLC, it still can be _represented_ in UPLC through certain [encodings](./../Concepts/Data%20and%20Scott%20encoding.md). The aforementioned `Data` encoding can be used to represent arbitrary types in on-chain components such as Datum and Redeemers. On the other hand Scott Encoding can additionally encode function types but cannot be used in Datums or Redeemers. The key idea is that UPLC doesn't track what differentiates semantically distinct values, regardless of their encoding, and will not prevent a programmer from operating on the underlying representation in non-sensical ways.

Plutarch's solution is to _tag_ scripts that compile to UPLC (i.e., Plutarch `Term`s) with types. Doing so allows the Plutarch compiler to track and type check operations on semantically distinct UPLC values. These tags are provided by "Plutarch Types", or "types of kind `PType`".

For the Plutarch compiler to bridge between arbitrary, semantically-rich Haskell types and the untyped values of UPLC, it is necessary to associate various bits of information with `PType`s. On the one hand, each `PType` should have some semantic, type-level richness such as typeclass instances (otherwise, there would be little point in programming in Haskell!). On the other hand, each `PType` needs to have a UPLC representation, either as a built-in primitive value,`Data`, or as a Scott-encoded lambda, in order to compile to UPLC.
