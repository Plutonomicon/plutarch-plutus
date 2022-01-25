{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Either (PEither (..)) where

import Data.Kind (Type)
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch
import Plutarch.Internal

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PEither a b) where
  -- type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pcon' :: forall s. PEither a b s -> forall c. Term s (PInner (PEither a b) c)
  pcon' x =
    -- plam' $ \c1 -> plam' $ \c2 -> f (c1 :* c2 :* Nil) val
    -- plamL $ \cs -> pconEither cs x
    gpcon @(PEither a b) @s @_ @(Code (PEither a b s)) $ from x

  -- pcon' (PLeft x) = plam' $ \c1 -> plam' $ \_ -> c1 # x
  -- pcon' (PRight y) = plam' $ \_ -> plam' $ \c2 -> c2 # y
  pmatch' p f = p # plam (f . PLeft) # plam (f . PRight)

-- | Specialized
_pconEither :: forall s a b c. NP (Term s) '[a :--> c, b :--> c] -> PEither a b s -> Term s c
_pconEither (c1 :* c2 :* Nil) = \case
  PLeft x -> c1 # x
  PRight y -> c2 # y

-- | Generic version of `pcon'`
gpcon ::
  forall a s c code pcode.
  ( PlutusType a
  , Generic (a s)
  , code ~ Code (a s)
  , pcode ~ ToPType2 code
  , GPCon pcode c s
  , PLamL (ScottList s pcode c) c s
  , Fn (ScottList s pcode c) c ~ ScottEncoding (Code (a 'SI)) c
  , AllZipN (Prod SOP) (LiftedCoercible I (Term s)) code pcode
  ) =>
  SOP I (Code (a s)) ->
  Term s (ScottEncoding (Code (a 'SI)) c)
gpcon val =
  plamL @(ScottList s pcode c) @c $ \(f :: NP (Term s) (ScottList s pcode c)) ->
    gpcon' @pcode @c @s f $
      unSOP $ pSop val
  where
    pSop :: AllZipN (Prod SOP) (LiftedCoercible I (Term s)) xss (ToPType2 xss) => SOP I xss -> SOP (Term s) (ToPType2 xss)
    pSop = hcoerce

-- | '[a :--> c, b :--> c]
type ScottList :: S -> [[PType]] -> PType -> [PType]
type family ScottList s code c where
  ScottList _ '[] c = '[]
  ScottList s (xs ': xss) c = Fn xs c ': ScottList s xss c

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

class AppL (c :: PType) (xs :: [PType]) where
  appL :: Term s (Fn xs c) -> NP (Term s) xs -> Term s c

instance AppL c '[] where
  appL f Nil = f

instance AppL c xs => AppL c (x ': xs) where
  appL f (x :* xs) = (f # x) `appL` xs

class PLamL (as :: [PType]) (b :: PType) (s :: S) where
  plamL :: (NP (Term s) as -> Term s b) -> Term s (Fn as b)

instance PLamL '[] b s where
  plamL f = f Nil

instance PLamL as b s => PLamL (a ': as) b s where
  plamL f = plam' $ \a -> plamL $ \as -> f (a :* as)

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
