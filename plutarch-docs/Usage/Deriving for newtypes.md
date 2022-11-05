# Deriving typeclasses for `newtype`s

If you're defining a `newtype` to an existing Plutarch type, like so:

```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
```

You ideally want to just have this `newtype` be represented as a `PByteString` under the hood. Therefore, all the typeclass instances of `PByteString` make sense for `PPubKeyHash` as well. In this case, you can simply derive all those typeclasses for your `PPubKeyHash` type as well! Via `DerivePNewtype`:

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Prelude

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)
```

(yes, you need `UndecidableInstances` to derive `PlutusType`)

`DerivePNewtype` takes two type parameters. Both of them are Plutarch types (i.e. types with kind `PType`). The first one is the type you're deriving the instances for, while the second one is the _inner_ type (whatever `PPubKeyHash` is a newtype to).

> Note: It's important to note that the content of a `newtype` _that aims to be a Plutarch type_ (i.e. can be represented as a Plutarch term), must also be a Plutarch term. The type `PByteString s` simply doesn't exist in the Plutus Core world after compilation. It's all just `Term`s. So, when you say `Term s PPubKeyHash`, you're really just describing a `Term s PByteString` under the hood - since that's what it _is_ during runtime.

> Aside: You can access the inner type using `pto` (assuming it's a `PlutusType` instance). For example, `pto x`, where `x :: Term s PPubKeyHash`, would give you `Term s PByteString`. `pto` converts a [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md) term to its inner type. This is very useful, for example, when you need to use a function that operates on bytestring terms, but all you have is a `Term s PPubKeyHash`. You _know_ it's literally a bytestring under the hood anyway - but how do you obtain that? Using `pto`!

Currently, `DerivePNewtype` lets you derive the following typeclasses for your Plutarch _types_:

- `PlutusType`
- `PIsData`
- `PEq`
- `POrd`
- `PIntegral`

You can also derive the following typeclasses for Plutarch _terms_:

- `Num`
- `Semigroup`
- `Monoid`

What does this mean? Well, `Num` would actually be implemented for `Term s a`, where `a` is a Plutarch type. For example, if you wanted to implement `Semigroup` for `Term s PPubKeyHash` (`Term s PByteString` already has a `Semigroup` instance), you can write:

```hs
{-# LANGUAGE StandaloneDeriving #-}

deriving via (Term s (DerivePNewtype PPubKeyHash PByteString)) instance Semigroup (Term s PPubKeyHash)
```
