{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.PlutusType (
  PlutusType (..),
  PCon (..),
  PMatch (..),
) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (Text), Nat, TypeError, type (+))
import Generics.SOP (
  All,
  Generic (Code),
  NP (Nil, (:*)),
  NS (S, Z),
  SOP (SOP),
  Top,
 )
import Plutarch.DataRepr.Internal.HList.Utils (IndexList)
import Plutarch.Internal (PType, S, Term, pforce, plam', punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutarch.Internal.Generic (MkNS, PCode, PGeneric, gpfrom, gpto, mkSum)
import Plutarch.Internal.PLam ((#))
import Plutarch.Internal.TypeFamily (ToPType, ToPType2)

{- |

  The 'PlutusType' class allows encoding Haskell data-types as plutus terms
  via constructors and destructors.

  Typically, you want to use scott encoding to represent the data type, which
  can be automatically derived as follows:

  > import qualified GHC.Generics as GHC
  > import Generics.SOP
  >
  > data MyType (a :: PType) (b :: PType) (s :: S)
  >   = One (Term s a)
  >   | Two (Term s b)
  >   deriving stock (GHC.Generic)
  >   deriving anyclass (Generic, PlutusType)

  If you instead want to use data encoding, you should first implement "Plutarch.PDataRepr.PIsDataRepr", and then
  derive 'PlutusType' via "Plutarch.PDataRepr.PIsDataReprInstances":

  > import qualified GHC.Generics as GHC
  > import Generics.SOP
  > import Plutarch.DataRepr
  >
  > data MyType (a :: PType) (b :: PType) (s :: S)
  >   = One (Term s (PDataRecord '[ "_0" ':= a ]))
  >   | Two (Term s (PDataRecord '[ "_0" ':= b ]))
  >   deriving stock (GHC.Generic)
  >   deriving anyclass (Generic, PIsDataRepr)
  >   deriving (PlutusType, PIsData) via PIsDataReprInstances (MyType a b)

  Alternatively, you may derive 'PlutusType' by hand as well. A simple example, encoding a
  Sum type as an Enum via PInteger:

  > data AB (s :: S) = A | B
  >
  > instance PlutusType AB where
  >   type PInner AB _ = PInteger
  >
  >   pcon' A = 0
  >   pcon' B = 1
  >
  >   pmatch' x f =
  >     pif (x #== 0) (f A) (f B)
  >

  instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type:

  > swap :: Term s AB -> Term s AB
  > swap x = pmatch x $ \case
  >  A -> pcon B
  >  B -> pcon A

  Further examples can be found in examples/PlutusType.hs
-}
class (PCon a, PMatch a) => PlutusType (a :: PType) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: PType) :: PType
  type PInner a (b' :: PType) = ScottFn (ScottList 'PI.SI (ToPType2 (Code (a 'PI.SI))) b') b'
  pcon' :: forall s b. a s -> Term s (PInner a b)
  default pcon' ::
    forall s b code pcode.
    ( code ~ Code (a s)
    , pcode ~ PCode s a
    , PGeneric s a
    , GPCon pcode b s
    , PLamL (ScottList' s pcode b) b s
    , ScottFn' (ScottList s pcode b) b ~ PInner a b
    , ScottFn (ScottList' s pcode b) b ~ PInner a b
    , All Top pcode
    ) =>
    a s ->
    Term s (PInner a b)
  pcon' x = gpcon @a @b $ gpfrom x

  pmatch' :: forall s b. (Term s (PInner a b)) -> (a s -> Term s b) -> Term s b
  default pmatch' ::
    forall s b code pcode.
    ( code ~ Code (a s)
    , pcode ~ PCode s a
    , PGeneric s a
    , AppL b (ScottList' s pcode b)
    , GPMatch a 0 code b s
    , PInner a b ~ ScottFn (ScottList' s pcode b) b
    ) =>
    (Term s (PInner a b)) ->
    (a s -> Term s b) ->
    Term s b
  pmatch' x f = gpmatch @a x (f . gpto)

instance {-# OVERLAPPABLE #-} PlutusType a => PMatch a where
  pmatch x f = pmatch' (punsafeCoerce x) f

instance PlutusType a => PCon a where
  pcon x = punsafeCoerce (pcon' x)

class PCon a where
  -- | Construct a Plutarch Term via a Haskell datatype
  pcon :: a s -> Term s a

class PMatch a where
  -- | Pattern match over Plutarch Terms via a Haskell datatype
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b

-- | Generic version of `pcon'`
gpcon ::
  forall a c s code pcode.
  ( PlutusType a
  , PGeneric s a
  , code ~ Code (a s)
  , pcode ~ PCode s a
  , GPCon pcode c s
  , PLamL (ScottList' s pcode c) c s
  , ScottFn (ScottList' s pcode c) c ~ ScottFn' (ScottList s pcode c) c
  ) =>
  SOP (Term s) pcode ->
  Term s (ScottFn' (ScottList s pcode c) c)
gpcon (SOP val) =
  plamL @(ScottList' s pcode c) @c $ \(f :: NP (Term s) (ScottList' s pcode c)) ->
    gpcon' @pcode @c @s f val

{- |
  `gpcon'`, given a *partial* scott encoding (as a `PLamL`) and a sum choice, applies
  that encoding to the sum choice.

  The partial encoding is any tail of the full scott encoded function, such that
  one of its elements corresponds to the sum choice.
-}
class GPCon (xss :: [[PType]]) (c :: PType) (s :: S) where
  gpcon' :: NP (Term s) (ScottList' s xss c) -> NS (NP (Term s)) xss -> Term s c

instance GPCon '[] c s where
  gpcon' Nil = \case {}

instance (GPCon xs c s, AppL c x) => GPCon (x ': xs) c s where
  gpcon' (f :* fs) = \case
    Z x -> f `appL` x
    S xs -> gpcon' fs xs

{- |
  Generic version of `pmatch'`
-}
gpmatch ::
  forall a s c code pcode.
  ( PGeneric s a
  , code ~ Code (a s)
  , pcode ~ PCode s a
  , AppL c (ScottList' s pcode c)
  , GPMatch a 0 code c s
  ) =>
  Term s (ScottFn (ScottList' s pcode c) c) ->
  (SOP (Term s) pcode -> Term s c) ->
  Term s c
gpmatch x f =
  x `appL` gpmatch' @a @0 @code @c @s f

{- |
  `gpmatch'` returns a hlist of lambdas (or delayed terms) to be applied on the
  scott encoding function.
-}
class GPMatch (a :: PType) (n :: Nat) (xss :: [[Type]]) (c :: PType) (s :: S) where
  gpmatch' :: (SOP (Term s) (PCode s a) -> Term s c) -> NP (Term s) (ScottList' s (ToPType2 xss) c)

instance GPMatch a n '[] c s where
  gpmatch' _ = Nil

instance
  ( code ~ Code (a s)
  , ToPType xs ~ IndexList n (PCode s a)
  , GPMatch a (n + 1) xss c s
  , PLamL (ToPType xs) c s
  , MkNS n (PCode s a) (NP (Term s))
  , All Top (ToPType xs)
  , All Top xs
  ) =>
  GPMatch a n (xs : xss) c s
  where
  gpmatch' f =
    plamL @(ToPType xs) @c (f . SOP . mkSum @n @(PCode s a) @(Term s))
      :* gpmatch' @a @(n + 1) @xss @c @s f

{- |
  `plamL` is like `plamL'`, but pdelays the 0-arity case.

  ```
  plamL $ \Nil -> pcon 42 -- Equivalent to: `pdelay (pcon 42)`.
-}
class PLamL (as :: [PType]) (b :: PType) (s :: S) where
  plamL :: (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)

instance PLamL '[] b s where
  plamL f = PI.pdelay $ f Nil

instance PLamL' as b s => PLamL (a ': as) b s where
  plamL f = plam' $ \a -> plamL' $ \as -> f (a :* as)

{- |
  `plamL'` produces a multi-arity plam, but taking a HList of Plutarch terms as
  arguments.

  ```
  plamL $ \(x :* y :* Nil) ->
    x + y
  ```

  - `NP (Term s) '[x, y]` corresponds to `x :* y :* Nil`.
  - `ScottFn' '[x, y] b` corresponds to `x :--> y :--> b`.
-}
class PLamL' (as :: [PType]) (b :: PType) (s :: S) where
  plamL' :: (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)

instance PLamL' '[] b s where
  plamL' f = f Nil

instance PLamL' as b s => PLamL' (a ': as) b s where
  plamL' f = plam' $ \a -> plamL' $ \as -> f (a :* as)

{- |
  `appL` is like `appL'`, but pforce's the 0-arity case.

  ```
  f = plamL $ \Nil -> pdelay $ pcon 42
  g = f `appL` Ni
  ```
-}
class AppL (c :: PType) (xs :: [PType]) where
  appL :: Term s (ScottFn xs c) -> NP (Term s) xs -> Term s c

instance AppL c '[] where
  appL f Nil = pforce f

instance (AppL' c xs, AppL c xs) => AppL c (x ': xs) where
  appL f (x :* xs) = (f # x) `appL'` xs

{- |
  `appL'` takes a multi-argument lambda (produced by `plamL`) and applies it to
  the associated list of values.

  ```
  f = plamL $ \(x :* y :* z :* Nil) -> x + y + z
  g = f `appL'` (1 :* 2 :* 3 :* Nil)
  ```
-}
class AppL' (c :: PType) (xs :: [PType]) where
  appL' :: Term s (ScottFn' xs c) -> NP (Term s) xs -> Term s c

instance AppL' c '[] where
  appL' f Nil = f

instance AppL' c xs => AppL' c (x ': xs) where
  appL' f (x :* xs) = (f # x) `appL'` xs

{- |
  List of scott-encoded constructors of a Plutarch type (represented by `Code`)

  ScottList s (Code (PEither a b s)) c = '[a :--> c, b :--> c]
-}
type ScottList :: S -> [[PType]] -> PType -> [PType]
type family ScottList s code c where
-- We disallow certain shapes because Scott encoding is not appropriate for them.
  ScottList _ '[] c = TypeError ( 'Text "PlutusType(scott encoding): Data type without constructors not accepted")
  ScottList _ '[ '[]] c = TypeError ( 'Text "PlutusType(scott encoding): Data type with single nullary constructor not accepted")
  ScottList _ '[ '[_]] c = TypeError ( 'Text "PlutusType(scott encoding): Data type with single unary constructor not accepted; use newtype!")
  ScottList s (xs ': xss) c = ScottFn xs c ': ScottList' s xss c

type ScottList' :: S -> [[PType]] -> PType -> [PType]
type family ScottList' s code c where
  ScottList' _ '[] c = '[]
  ScottList' s (xs ': xss) c = ScottFn xs c ': ScottList' s xss c

{- |
  An individual constructor function of a Scott encoding.

   ScottFn '[a, b] c = (a :--> b :--> c)
   ScottFn '[] c = PDelayed c
-}
type ScottFn :: [PType] -> PType -> PType
type family ScottFn xs b where
  ScottFn '[] b = PI.PDelayed b
  ScottFn (x ': xs) b = x :--> ScottFn' xs b

{- |
  Like `ScottFn`, but without the PDelayed case.
-}
type ScottFn' :: [PType] -> PType -> PType
type family ScottFn' xs b where
  ScottFn' '[] b = b
  ScottFn' (x ': xs) b = x :--> ScottFn' xs b
