{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Plutarch.Field 
  (HList (..), indexHList, Elem (..), NatElem (..), getField, BindFields (..), xssum) where

--import Data.Kind (Type)
--import Data.Proxy (Proxy (..))
import GHC.TypeLits (type (-), type (+), Nat)
import Plutarch.DataRepr 
  (PDataList, pdhead, pdtail)
import Plutarch.Internal (TermCont (..), punsafeCoerce)
--import Plutarch (PType)
import Plutarch.Builtin (PAsData, PData, PBuiltinList, PIsData (..))
import Plutarch.Prelude

import qualified PlutusTx
import Plutarch.Integer
import Plutarch.Lift


--------------------------------------------------------------------------------

-- | Usual GADT Heterogenous List encoding
data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | GADT proof-witness of HList membership, usable as an index
data Elem (x :: k) (xs :: [k]) where
  Here :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

-- | Index HList using Elem
indexHList :: HList xs -> (forall x. Elem x xs -> x)
indexHList (HCons x _) Here = x
indexHList (HCons _ xs) (There i) = indexHList xs i
indexHList HNil impossible = case impossible of {}

-- | Indexing type-level lists
type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

{- | 
  Construct an `Elem` via Nat.

  This class could instead be a more direct version of 'indexHList',
  but perhaps the `Elem` encoding will be useful.

-}
class 
  (IndexList n xs ~ x) => 
  NatElem (n :: Nat) (x :: k) (xs :: [k]) | xs n -> x where
  {- | Construct the `Elem` corresponding to a Nat index.

    Example:

    >>> natElem @_ @0 
    Here

    >>> natElem @_ @3
    There (There (There Here))
  -}

  natElem :: Elem x xs

instance {-# OVERLAPS #-} NatElem 0 x (x ': xs) where
  natElem :: Elem x (x ': xs)
  natElem = Here

instance {-# OVERLAPPABLE #-}
  ( IndexList n (y ': xs) ~ x
  , NatElem (n - 1) x xs 
  ) =>
  NatElem n x (y ': xs) where
  natElem :: Elem x (y ': xs)
  natElem = There (natElem @_ @(n-1) @x @xs)

-- | Get the Index of a type in a list
type family IndexOf (x :: k) (xs :: [k]) :: Nat where
  IndexOf x (x ': _) = 0
  IndexOf x (y ': xs) = (IndexOf x xs + 1)

{- | 
  Construct an Elem via the position of a given type
  in a type-level list.

  The intended use is to 'lookup' the index of a 
  field name:

  >>> fieldElem @"z" @["x", "y", "z", "w"]
  >>> There (There Here))

-}
fieldElem :: 
  forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  Elem x xs
fieldElem = natElem @_ @n

{- | 
  Index a HList with a field in a provided list of fields.

  >>> xs = HCons 1 (HCons 2 (HCons 3 HNil))
  >>> getField @"y" @["x", "y", "z"] xs
  >>> 2

-}
getField 
  :: forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  HList xs -> x
getField xs = indexHList xs $ fieldElem @f @fs

--------------------------------------------------------------------------------

type family TermsOf (s :: S) (as :: [PType]) :: [Type] where
  TermsOf _ '[] = '[]
  TermsOf s (x ': xs) = Term s (PAsData x) ': TermsOf s xs

class BindFields as where
  {- | 
    Bind all the fields in a 'PDataList' term to a corresponding
    HList of Terms.

    A continuation is returned to enable sharing of
    the generated bound-variables.

  -}
  bindFields' :: Term s (PDataList as) -> TermCont s (HList (TermsOf s as))

instance  {-# OVERLAPS #-}
  BindFields (a ': '[]) where
  bindFields' t =
    pure $ HCons (pdhead # t) HNil

instance {-# OVERLAPPABLE #-}
  (BindFields as) => BindFields (a ': as) where
  bindFields' t = do
    t' <- TermCont $ plet t
    --tail <- TermCont $ plet $ pdtail # t 
    xs <- bindFields @as (pdtail # t')
    pure $ HCons (pdhead # t') xs

xs :: Term s (PDataList '[PInteger, PInteger, PInteger])
xs = 
  punsafeCoerce $ 
    pconstant @(PBuiltinList PData) $ fmap (PlutusTx.toData @Integer) [1,2,3]

xssum :: Term s PInteger
xssum = 
  runTermCont (bindFields' xs) $ \case
    (HCons x (HCons y (HCons z HNil))) -> 
      pfromData x + pfromData y + pfromData z

--------------------------------------------------------------------------------

{- | 
  Heterogenous Record, using a list of symbols as a
  set of corresponding indices by position.

-}
newtype HRec (fs :: [Symbol]) (as :: [Type]) = 
  HRec { hrecList :: HList as }

class PDataFields (a :: PType) where
  type PFieldNames a :: [Symbol]
  type PFieldTypes a :: [PType]
  ptoFields :: Term s a -> Term s (PDataList (PFieldTypes a))

letFields :: 
  ( PDataFields a
  , BindFields (PFieldTypes a)
  ) =>
  Term s a -> (HRec (PFieldNames a) (PFieldTypes a) -> Term s b) -> Term s b
letFields t =
  bindFields @as

