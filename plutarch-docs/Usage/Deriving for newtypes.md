<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Plutarch.Docs.DerivingForNewtype (PPubKeyHash'(..), PPubKeyHash(..)) where 
import Plutarch.Prelude
```

</p>
</details>

# Deriving typeclasses for `newtype`s

If you're defining a `newtype` to an existing Plutarch type, like so:

```haskell
newtype PPubKeyHash' (s :: S) = PPubKeyHash' (Term s PByteString)
```

You ideally want to just have this `newtype` be represented as a `PByteString` under the hood. Therefore, all the typeclass instances of `PByteString` make sense for 
`PPubKeyHash` as well. In this case, you can simply derive all those typeclasses for your `PPubKeyHash` type as well:

```haskell
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType PPubKeyHash where type DPTStrat _ = PlutusTypeNewtype

```

> Note: It's important to note that the content of a `newtype` _that aims to be a Plutarch type_ (i.e. can be represented as a Plutarch term), must also be a Plutarch term. 
> The type `PByteString s` simply doesn't exist in the Plutus Core world after compilation. It's all just `Term`s. So, when you say `Term s PPubKeyHash`, you're really just 
> describing a `Term s PByteString` under the hood - since that's what it _is_ during runtime.

> Aside: You can access the inner type using `pto` (assuming it's a `PlutusType` instance). For example, `pto x`, where `x :: Term s PPubKeyHash`, would give you 
> `Term s PByteString`. `pto` converts a [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md) term to its inner type. This is very useful, for 
> example, when you need to use a function that operates on bytestring terms, but all you have is a `Term s PPubKeyHash`. You _know_ it's literally a bytestring 
> under the hood anyway - but how do you obtain that? Using `pto`!

Currently, `DerivePNewtype` lets you derive the following typeclasses for your Plutarch _types_:

- `PEq`
- `PIntegral`
- `PIsData`
- `PNum`
- `POrd`
- `PPartialOrd`
- `PShow`
- `PlutusType`

> Note: You cannot derive instances for `Term`s anymore because of coherence issues with the previous solutions. All derivations have to 
> be done for the PlutusType (e.g. you cannot newtype derive `Semigroup` for `PPubKeyHash` anymore)
