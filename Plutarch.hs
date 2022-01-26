{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch (
  (PI.:-->),
  PI.ClosedTerm,
  PI.compile,
  PI.Dig,
  PI.hashOpenTerm,
  PI.hashTerm,
  PI.papp,
  PI.pdelay,
  PI.PDelayed,
  PI.perror,
  PI.pforce,
  PI.phoistAcyclic,
  PI.plam',
  PI.plet,
  PI.Term,
  PI.TermCont (..),
  PI.S,
  PI.PType,
  PlutusType (..),
  printTerm,
  printScript,
  (#$),
  (#),
  pinl,
  PCon (..),
  PMatch (..),
  pto,
  pfix,
  POpaque (..),
  popaque,
  plam,
  DerivePNewtype (DerivePNewtype),
  gpcon,
  gpmatch,
) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.SOP.Constraint
import GHC.TypeLits
import Generics.SOP
import Plutarch.DataRepr.Internal.Generic
import Plutarch.DataRepr.Internal.HList (IndexList)
import Plutarch.Internal (ClosedTerm, PType, S, Term, compile, papp, pforce, phoistAcyclic, plam', punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Pretty (prettyPlcReadableDebug)

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: ClosedTerm a -> String
printTerm term = printScript $ compile term

{- |
  High precedence infixl synonym of 'papp', to be used like
  function juxtaposition. e.g.:

  >>> f # x # y
  f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
(#) = papp

infixl 8 #

{- |
  Low precedence infixr synonym of 'papp', to be used like
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
(#$) = papp

infixr 0 #$

{- $plam
 Lambda abstraction.

 The 'PLamN' constraint allows
 currying to work as expected for any number of arguments.

 > id :: Term s (a :--> a)
 > id = plam (\x -> x)

 > const :: Term s (a :--> b :-> a)
 > const = plam (\x y -> x)
-}

class PLamN (a :: Type) (b :: PType) (s :: S) | a -> b, s b -> a where
  plam :: forall c. (Term s c -> a) -> Term s (c :--> b)

instance (a' ~ Term s a) => PLamN a' a s where
  plam = plam'

instance {-# OVERLAPPING #-} (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s where
  plam f = plam' $ \x -> plam (f x)

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v

{- |

  The 'PlutusType' class allows encoding Haskell data-types as plutus terms
  via constructors and destructors.

  A simple example, encoding a Sum type as an Enum via PInteger:

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
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)

  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

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

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> (forall b. Term s (PInner a b))
pto x = punsafeCoerce x

-- | An Arbitrary Term with an unknown type
data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | Erase the type of a Term
popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam $ \f ->
      (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))

{- | Facilitates deriving 'PlutusType' and 'PIsData' for newtypes.

For any newtype represented as-
> newtype PFoo (s :: S) = PFoo (Term s PBar)

where 'PBar' has a 'PIsData' instance, you can derive 'PlutusType' and 'PIsData' using-
> deriving (PlutusType, PIsData) via (DerivePNewtype PFoo PBar)

This will make 'PFoo' simply be represnted as 'PBar' under the hood.
-}
newtype DerivePNewtype (a :: PType) (b :: PType) (s :: PI.S) = DerivePNewtype (a s)

instance (forall (s :: PI.S). Coercible (a s) (Term s b)) => PlutusType (DerivePNewtype a b) where
  type PInner (DerivePNewtype a b) _ = b
  pcon' (DerivePNewtype t) = ptypeInner t
  pmatch' x f = f . DerivePNewtype $ ptypeOuter x

instance Semigroup (Term s b) => Semigroup (Term s (DerivePNewtype a b)) where
  x <> y = punsafeFrom $ pto x <> pto y

instance Monoid (Term s b) => Monoid (Term s (DerivePNewtype a b)) where
  mempty = punsafeFrom $ mempty @(Term s b)

instance Num (Term s b) => Num (Term s (DerivePNewtype a b)) where
  x + y = punsafeFrom $ pto x + pto y
  x - y = punsafeFrom $ pto x - pto y
  x * y = punsafeFrom $ pto x * pto y
  abs x = punsafeFrom $ abs $ pto x
  negate x = punsafeFrom $ negate $ pto x
  signum x = punsafeFrom $ signum $ pto x
  fromInteger x = punsafeFrom $ fromInteger @(Term s b) x

ptypeInner :: forall (x :: PType) y s. Coercible (x s) (Term s y) => x s -> Term s y
ptypeInner = coerce

ptypeOuter :: forall (x :: PType) y s. Coercible (x s) (Term s y) => Term s y -> x s
ptypeOuter = coerce

punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom x = PI.punsafeCoerce x

-- PlutusType deriving machinary

{- |
  Generic version of `pmatch'`
-}
gpmatch ::
  forall a s c code pcode.
  ( Generic (a s)
  , code ~ Code (a s)
  , pcode ~ ToPType2 code
  , AppL c (ScottList 'PI.SI pcode c)
  , MkMatchList a 0 code c s
  ) =>
  Term s (ScottFn (ScottList 'PI.SI (ToPType2 (Code (a s))) c) c) ->
  (SOP I (Code (a s)) -> Term s c) ->
  Term s c
gpmatch scott f =
  scott `appL` mkMatchList @a @0 @code @c @s f

class MkMatchList (a :: PType) (n :: Nat) (xss :: [[Type]]) (c :: PType) (s :: S) where
  mkMatchList :: (SOP I (Code (a s)) -> Term s c) -> NP (Term s) (ScottList 'PI.SI (ToPType2 xss) c)

instance MkMatchList a n '[] c s where
  mkMatchList _ = Nil

instance
  ( ToPType xs ~ ToPType (IndexList n (Code (a s)))
  , MkMatchList a (n + 1) xss c s
  , ps ~ IndexList n (Code (a s))
  , PLamL (ToPType ps) c s
  , MkSum n (Code (a s))
  , AllZipF (LiftedCoercible (Term s) I) (ToPType ps) ps
  , SameShapeAs ps (ToPType ps)
  , SameShapeAs (ToPType ps) ps
  , All Top (ToPType ps)
  , All Top ps
  ) =>
  MkMatchList a n (xs : xss) c s
  where
  mkMatchList f =
    plamL @(ToPType xs) @c (\xargs -> f $ SOP $ mkSum @n @(Code (a s)) $ unPsop xargs)
      :* mkMatchList @a @(n + 1) @xss @c @s f

-- | Generic version of `pcon'`
gpcon ::
  forall a s c code pcode.
  ( PlutusType a
  , Generic (a s)
  , code ~ Code (a s)
  , pcode ~ ToPType2 code
  , GPCon pcode c s
  , PLamL (ScottList s pcode c) c s
  , AllZipN (Prod SOP) (LiftedCoercible I (Term s)) code pcode
  ) =>
  SOP I (Code (a s)) ->
  Term s (ScottFn (ScottList s pcode c) c)
gpcon val =
  plamL @(ScottList s pcode c) @c $ \(f :: NP (Term s) (ScottList s pcode c)) ->
    gpcon' @pcode @c @s f $
      unSOP $ pSop val

pSop :: AllZipN (Prod SOP) (LiftedCoercible I (Term s)) xss (ToPType2 xss) => SOP I xss -> SOP (Term s) (ToPType2 xss)
pSop = hcoerce

unPsop ::
  ( AllZipF (LiftedCoercible (Term s) I) (ToPType xs) xs
  , SameShapeAs xs (ToPType xs)
  , SameShapeAs (ToPType xs) xs
  , All Top (ToPType xs)
  , All Top xs
  ) =>
  NP (Term s) (ToPType xs) ->
  NP I xs
unPsop = hcoerce
class GPCon (xss :: [[PType]]) (c :: PType) (s :: S) where
  gpcon' :: NP (Term s) (ScottList s xss c) -> NS (NP (Term s)) xss -> Term s c

instance {-# OVERLAPPABLE #-} (AppL c x) => GPCon (x ': '[]) c s where
  gpcon' (f :* Nil) = \case
    Z x -> appL f x
    S _sum' -> undefined -- This sucks, can we avoid it at type-level?

instance {-# OVERLAPPING #-} (GPCon (x1 ': xs) c s, AppL c x) => GPCon (x ': x1 ': xs) c s where
  gpcon' (f :* fs) = \case
    Z x -> appL f x
    S sum' -> gpcon' fs sum'

{- |
  `appL` is like `appL'`, but pforce's the 0-arity case.

  ```
  f = plamL $ \Nil -> pdelay $ pcon 42
  g = f `appL` Ni
  ```

  TODO: Write a test to test laziness case: https://github.com/Plutonomicon/plutarch/issues/193
-}
class AppL (c :: PType) (xs :: [PType]) where
  appL :: Term s (ScottFn xs c) -> NP (Term s) xs -> Term s c

instance AppL c '[] where
  appL f Nil = pforce f

instance (AppL' c xs, AppL c xs) => AppL c (x ': xs) where
  appL f (x :* xs) = (f # x) `appL'` xs

{- |
  `appL'` takes a multi-argument lambda (usually created by `plamL`) and applies
  it to the associated list of values.

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
  `plamL` is like `plam`, but takes a HList of Plutarch terms as arguments.

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
  Scott-encoding of a Plutarch type, given its ScottList.

   ScottFn '[a, b] c = (a :--> b :--> c)
   ScottFn '[] c = PDelayed c
-}
type ScottFn :: [PType] -> PType -> PType
type family ScottFn xs b where
  ScottFn '[] b = PI.PDelayed b
  ScottFn (x ': xs) b = x :--> ScottFn' xs b

type ScottFn' :: [PType] -> PType -> PType
type family ScottFn' xs b where
  ScottFn' '[] b = b
  ScottFn' (x ': xs) b = x :--> ScottFn' xs b

{- |
  List of scott-encoded constructors of a Plutarch type, represented by `Code`

  ScottList s (Code (PEither a b s)) c = '[a :--> c, b :--> c]
-}
type ScottList :: S -> [[PType]] -> PType -> [PType]
type family ScottList s code c where
  ScottList _ '[] c = '[]
  ScottList s (xs ': xss) c = ScottFn xs c ': ScottList s xss c

-- TODO: clean these up
type UnTerm :: Type -> PType
type family UnTerm x where
  UnTerm (Term s a) = a

-- Unfortunately we can't write a generic FMap due to ghc's arity limitations.
type ToPType :: [Type] -> [PType]
type family ToPType as where
  ToPType '[] = '[]
  ToPType (a ': as) = UnTerm a ': ToPType as

type ToPType2 :: [[Type]] -> [[PType]]
type family ToPType2 as where
  ToPType2 '[] = '[]
  ToPType2 (a ': as) = ToPType a ': ToPType2 as
