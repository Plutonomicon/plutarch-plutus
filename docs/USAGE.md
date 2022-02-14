This document describes various core Plutarch usage concepts.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

-   [Applying functions](#applying-functions)
-   [Conditionals](#conditionals)
-   [Recursion](#recursion)
-   [Do syntax with `TermCont`](#do-syntax-with-termcont)
-   [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](#do-syntax-with-qualifieddo-and-plutarchmonadic)
    -   [Translating `do` syntax with `QualifiedDo` to GHC 8](#translating-do-syntax-with-qualifieddo-to-ghc-8)
-   [Deriving typeclasses for `newtype`s](#deriving-typeclasses-for-newtypes)
-   [Deriving typeclasses with generics](#deriving-typeclasses-with-generics)

</details>

# Applying functions

You can apply Plutarch level functions using `#` and `#$` (or `papp`). Notice the associativity and precedence of those operators-

> Jack: operators:

```haskell
infixl 8 #

infixr 0 #$
```

`#$` is pretty much just `$` for Plutarch functions. But `#` is left associative and has a high precedence. This essentially means that the following-

> Jack: following:

```haskell
f # 1 # 2

-- f :: Term s (PInteger :--> PInteger :--> PUnit)
```

applies `f` to 1 and 2. i.e, it is parsed as - `((f 1) 2)`

Whereas, the following-

```haskell
f #$ foo # 1

-- f :: Term s (PBool :--> PUnit)
-- foo :: Term s (PInteger :--> PBool)
```

parses as - `f (foo 1)`

(don't take the parens literally - after all `f` and `foo` are not Haskell level functions)

> Jack: consider:
>
> This means that `f # 1 # 2`\* applies `f` to `1` and then `2` i.e. `(f 1) 2`. In contrast, `g #$ foo # 1`\*\* parses as `g (foo 1)`.
>
> > \* `f :: Term s (PInteger :--> PInteger :--> PUnit)`
>
> > \*\* `g :: Term s (PBool :--> PUnit)` `foo :: Term s (PInteger :--> PBool)`

> Aside: Remember that function application here is **strict**. The arguments _will be evaluated_ and then passed in.
>
> Rule of thumb: If you see `#` - you can quickly infer that a Plutarch level function is being applied and the arguments will be evaluated. Haskell level functions still have their usual semantics, which is why `pif` doesn't evaluate both branches. (if you want, you can use `pif'` - which is a Plutarch level function and therefore strict)

# Conditionals

You can simulate `if/then/else` at the Plutarch level using `pif`-

> Jack: `pif`: . N.B. I am no longer going to highlight instances where '-' needs to be replaced. Essentially everywhere I have seen '-' used thus far should be replaced with ':' or ','. See [here](https://www.lexico.com/grammar/colon), [here](https://www.lexico.com/grammar/comma) and [here](https://www.lexico.com/grammar/hyphen).

```haskell
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
```

This has similar semantics to Haskell's `if/then/else`. That is, only the branch for which the predicate holds - is evaluated.

```haskell
pif (pconstant True) 1 2
```

The above evaluates to `1`, which has type `Term s PInteger`

> Jack: `Term s PInteger`. \[full stop]

Of course, the predicate can be an arbitrary `Term s PBool` producing computation.

> Jack: I find 'producing computation' abrupt and unclear.

# Recursion

To emulate recursion in UPLC (Untyped Plutus Core), you need to use the Y combinator. Plutarch provides the Y combinator with the name `pfix`-

> Jack: Consider linking to the Plutonomicon UPLC docs here, as you did in guide.

> Jack: Consider linking to a simple resource on the Y combinator.

```haskell
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
```

It works as you would expect, though the type is scary. Think of it as the Haskell type-

```haskell
fix :: ((a -> b) -> (a -> b)) -> a -> b
```

The first argument is "self", or the function you want to recurse with.

```haskell
import Plutarch.Prelude

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

There's a Plutarch level factorial function! Note how `f` takes in a `self` and just recurses on it. All you have to do, is create a Plutarch level function by using `plam` on `f` and `pfix` the result - and that `self` argument will be taken care of for you.

# Do syntax with `TermCont`

You can mostly replicate the `do` syntax from `Plutarch.Monadic` using `TermCont`. In particular, the continuation accepting functions like `plet`, `pletFields`, `pmatch` and so on can utilize regular `do` syntax with `TermCont` as the underlying monad.

> Jack: consider emphasising _mostly_.

`TermCont @b s a` essentially represents `(a -> Term s b) -> Term s b`. `a` being the input to the continuation, and `Term s b` being the output. Notice the type application - `b` must have been brought into scope through another binding first.

TODO: REWORK (not above example, example has been moved below)
Here's how you'd write the [above example](#do-syntax-with-qualifieddo-and-plutarchmonadic) with `TermCont` instead.

```hs
import Plutarch.Api.Contexts
import Plutarch.Prelude

f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> unTermCont $ do
  PSpending _ <- tcont $ pmatch x
  pure $ ptrace "matched spending script purpose" $ pconstant ()
```

The best part is that this doesn't require `QualifiedDo`! So you don't need GHC 9.

> Jack: The best part is: as you don't require `QualifiedDo`, you don't need GHC 9!

Furthermore, this is very similar to the `Cont` monad - it just operates on Plutarch level terms. This means you can draw parallels to utilities and patterns one would use when utilizing the `Cont` monad. Here's an example-

```hs
import Plutarch.Prelude

-- | Terminate with given value on empty list, otherwise continue with head and tail.
nonEmpty :: Term s r -> PList a s -> TermCont @r s (Term s a, Term s (PList a))
nonEmpty x0 list = TermCont $ \k ->
  case list of
    PSCons x xs -> k (x, xs)
    PSNil -> x0

foo :: Term s (PList PInteger :--> PInteger)
foo = plam $ \l -> unTermCont $ do
  (x, xs) <- nonEmpty 0 =<< tcont (pmatch l)
  pure $ x + plength # xs
```

`foo` adds up the first element of the given list with the length of its tail. Unless the list was empty, in which case, it just returns 0. It uses continuations with the `do` syntax to elegantly utilize short circuiting!

# Do syntax with `QualifiedDo` and `Plutarch.Monadic`

The `Plutarch.Monadic` module provides convenient do syntax on common usage scenarios. It requires the `QualifiedDo` extension, which is only available in GHC 9.

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> P.do
  PSpending _ <- pmatch x
  ptrace "matched spending script purpose"
  pconstant ()
```

In essence, `P.do { x; y }` simply translates to `x y`; where `x :: a -> Term s b` and `y :: a`.

Similarly, `P.do { y <- x; z }` translates to `x $ \case { y -> z; _ -> ptraceError <msg> }`; where `x :: (a -> Term s b) -> Term s b`, `y :: a`, and `z :: Term s b`. Of course, if `y` is a fully exhaustive pattern match (e.g, singular constructor), the extra `_ -> ptraceError <msg>` case will not be generated at all and you'd simply get `x $ \y -> z`.

> Jack: e.g. not e.g,

Finally, `P.do { x }` is just `x`.

These semantics make it _extremely_ convenient for usage with [`pmatch`](./TYPECLASSES.md#plutustype-pcon-and-pmatch), [`plet`](./CONCEPTS.md#plet-to-avoid-work-duplication), [`pletFields`](./TYPECLASSES.md#all-about-extracting-fields), and [`ptrace`](./CONCEPTS.md#tracing) etc.

> Jack: consider 'for use with'.

```hs
pmatch :: Term s a -> (a s -> Term s b) -> Term s b

ptrace :: Term s PString -> Term s a -> Term s a
```

Of course, as long as the semantics of the `do` notation allows it, you can make your own utility functions that take in continuations - and they can utilize `do` syntax just the same.

## Translating `do` syntax with `QualifiedDo` to GHC 8

For convenience, most examples in this guide will be utilizing this `do` syntax. However, since `QualifiedDo` is available pre GHC 9 - we'll discuss how to translate those examples to GHC 8.

> Jack: pre-GHC 9

There are several ways to do this-

-   Use [`TermCont`](#do-syntax-with-termcont).
-   Don't use do syntax at all. You can easily translate the `do` syntax to regular continuation chains.

> Jack: use 'do syntax' or use do-syntax.

    Here's how you'd translate the above `f` function-

    ```hs
    f :: Term s (PScriptPurpose :--> PUnit)
    f = plam $ \x -> pmatch x $ \case
      PSpending _ -> ptrace "matched spending script purpose" $ pconstant ()
      _ -> ptraceError "incorrect script purpose"
    ```

    Simply put, functions like `pmatch`, `pletFields` take in a continuation. The `do` syntax enables you to bind the argument of the continuation using `<-`, and simply use flat code, rather than nested function calls.

-   Use the `Cont` monad. You can utilize this to also use regular `do` syntax by simply applying `cont` over functions such as `pmatch`, `pletFields` and similar utilities that take in a continuation function. There is an example of this [here](https://github.com/Plutonomicon/plutarch/blob/6b7dd254e4aaf366eb716dd3e18788426b3d1e2a/examples/Examples/Api.hs#L175-L189). Notice how `checkSignatory` has been translated to `Cont` monad usage in `checkSignatoryCont`.

    Here's how you'd translate the above `f` function-

    ```hs
    import Control.Monad.Trans.Cont (cont, runCont)

    import Plutarch.Prelude
    import Plutarch.Api.Contexts

    f :: Term s (PScriptPurpose :--> PUnit)
    f = plam $ \x -> (`runCont` id) $ do
      purpose <- cont $ pmatch x
      pure $ case purpose of
        PSpending _ -> ptrace "matched spending script purpose" $ pconstant ()
        _ -> ptraceError "invalid script purpose"
    ```

    Note that you have to translate the pattern matching manually, as `Cont` doesn't have a `MonadFail` instance.
-   Use [`RebindableSyntax`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html). You can replace the `>>=`, `>>`, and `fail` functions in your scope with the ones from `Plutarch.Monadic` using `RebindableSyntax`. This is arguably a bad practice but the choice is there. This will let you use the `do` syntax word for word. Although you wouldn't be qualifying your `do` keyword (like `P.do`), you'd just be using `do`.

    Here's how you'd translate the above `f` function-

    ```hs
    {-# LANGUAGE RebindableSyntax #-}

    import Prelude hiding ((>>=), (>>), fail)

    import Plutarch.Prelude
    import Plutarch.Monadic ((>>=), (>>), fail)
    import Plutarch.Api.Contexts

    f :: Term s (PScriptPurpose :--> PUnit)
    f = plam $ \x -> do
      PSpending _ <- pmatch x
      ptrace "matched spending script purpose"
      pconstant ()
    ```

# Deriving typeclasses for `newtype`s

If you're defining a `newtype` to an existing Plutarch type, like so-

```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
```

You ideally want to just have this `newtype` be represetned as a `PByteString` under the hood. Therefore, all the typeclass instances of `PByteString` make sense for `PPubKeyHash` as well. In this case, you can simply derive all those typeclasses for your `PPubKeyHash` type as well! Via `DerivePNewtype`-

> Jack: represented

```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)
```

`DerivePNewtype` takes two type parameters. Both of them are Plutarch types (i.e types with kind `PType`). The first one is the type you're deriving the instances for, while the second one is the _inner_ type (whatever `PPubKeyHash` is a newtype to).

> Note: It's important to note that the contents of a `newtype` _that aims to be a Plutarch type_ (i.e can be represented as a Plutarch term), must also be Plutarch terms. The type `PByteString s` simply doesn't exist in the Plutus Core world after compilation. It's all just `Term`s. So, when you say `Term s PPubKeyHash`, you're really just describing a `Term s PByteString` under the hood - since that's what it _is_ during runtime.

> Aside: You can access the inner type using `pto` (assuming it's a `PlutusType` instance). For example, `pto x`, where `x :: Term s PPubKeyHash`, would give you `Term s PByteString`. `pto` converts a [`PlutusType`](./TYPECLASSES.md#plutustype-pcon-and-pmatch) term to its inner type. This is very useful, for example, when you need to use a function that operates on bytestring terms, but all you have is a `Term s PPubKeyHash`. You _know_ it's literally a bytestring under the hood anyway - but how do you obtain that? Using `pto`!

Currently, `DerivePNewtype` lets you derive the following typeclasses for your Plutarch _types_:-

-   `PlutusType`
-   `PIsData`
-   `PEq`
-   `POrd`
-   `PIntegral`

You can also derive the following typeclasses for Plutarch _terms_:-

-   `Num`
-   `Semigroup`
-   `Monoid`

What does this mean? Well, `Num` would actually be implemented for `Term s a`, where `a` is a Plutarch type. For example, if you wanted to implement `Semigroup` for `Term s PPubKeyHash` (`Term s PByteString` already has a `Semigroup` instance), you can write-

```hs
{-# LANGUAGE StandaloneDeriving #-}

deriving via (Term s (DerivePNewtype PPubKeyHash PByteString)) instance Semigroup (Term s PPubKeyHash)
```

# Deriving typeclasses with generics

Plutarch also provides sophisticated generic deriving support for completely custom types. In particular, you can easily derive `PlutusType` for your own type-

```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s a)
  | Two (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
```

> Note: This requires the `generics-sop` package.

This will use a [scott encoding representation](./CONCEPTS.md#data-encoding-and-scott-encoding) for `MyType`, which is typically what you want. However, this will forbid you from representing your type as a `Data` value and as a result - you cannot implement `PIsData` for it. (Well, you can if you try hard enough - but you _really really really_ shouldn't)

> Jack: Scott is a proper noun and must always be capitalized.

Currently, generic deriving supports the following typeclasses:-

-   [`PlutusType`](./TYPECLASSES.md#implementing-plutustype-for-your-own-types) (scott encoding only)
-   [`PIsDataRepr`](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)
