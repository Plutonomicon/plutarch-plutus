{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.HList (
  -- * HRec and Label types
  HRec (HNil, HCons),
  Labeled (Labeled, unLabeled),

  -- * Field indexing functions
  hrecField,

  -- * Type families
  type IndexList,
  type IndexLabel,
  type SingleItem,
  type Drop,

  -- * Internal utils
  Elem (..),
  ElemOf (..),
) where

import Data.Kind (Type)
import GHC.Records (HasField, getField)
import GHC.TypeLits (Symbol)
import Plutarch (Term)
import Plutarch.Builtin
import Plutarch.DataRepr.Internal.FromData (PFromDataable, pmaybeFromAsData)
import Plutarch.DataRepr.Internal.HList.Utils (
  Drop,
  Elem (Here, There),
  IndexLabel,
  IndexList,
  Labeled (Labeled, unLabeled),
  SingleItem,
 )

--------------------------------------------------------------------------------
---------- HList and HRec types

data HRec (as :: [Type]) where
  HNil :: HRec '[]
  HCons :: (Labeled name a) -> HRec as -> HRec ((Labeled name a) ': as)

---------- Field indexing functions

-- | Index HRec using Elem
indexHRec :: HRec as -> (forall a. Elem a as -> a)
indexHRec (HCons x _) Here = x
indexHRec (HCons _ xs) (There i) = indexHRec xs i
indexHRec HNil impossible = case impossible of {}

{- |
  Index a HList with a field in a provided list of fields.

  >>> xs = HRec @["x", "y", "z"] (HCons 1 (HCons 2 (HCons 3 HNil)))
  >>> hrecField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hrecField' ::
  forall name a as.
  ( (IndexLabel name as ~ a)
  , ElemOf name a as
  ) =>
  HRec as ->
  a
hrecField' xs = unLabeled $ indexHRec xs $ elemOf @name @a @as

---------- Internal utils

{- |
  Construct an `Elem` via Nat.

  This class could instead be a more direct version of 'indexHList',
  but perhaps the `Elem` encoding will be useful.
-}
class
  (IndexLabel name as ~ a) =>
  ElemOf (name :: Symbol) (a :: Type) (as :: [Type])
    | as name -> a
  where
  -- | Construct the `Elem` corresponding to a Nat index.
  --
  --    Example:
  --
  --    >>> natElem @_ @0
  --    Here
  --
  --    >>> natElem @_ @3
  --    There (There (There Here))
  elemOf :: Elem (Labeled name a) as

instance {-# OVERLAPPING #-} ElemOf name a ((Labeled name a) ': as) where
  elemOf :: Elem (Labeled name a) ((Labeled name a) ': as)
  elemOf = Here

instance
  {-# OVERLAPPABLE #-}
  ( IndexLabel name (b ': as) ~ a
  , ElemOf name a as
  ) =>
  ElemOf name a (b ': as)
  where
  elemOf :: Elem (Labeled name a) (b ': as)
  elemOf = There (elemOf @name @a @as)

{- |
  Index a `HRec` with a field in a provided list of data fields.
  Implicitly unwraps `PAsData a` to `a` when necessary.

  >>> xs = HRec @["x", "y", "z"] (HCons 1 (HCons 2 (HCons 3 HNil)))
  >>> hrecField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hrecField ::
  forall name a as b c s.
  ( IndexLabel name as ~ a
  , ElemOf name a as
  , Term s (PAsData b) ~ a
  , PFromDataable b c
  ) =>
  HRec as ->
  Term s c
hrecField xs = pmaybeFromAsData $ hrecField' @name xs

---------- HasField instances
instance
  forall name a as b c s.
  ( IndexLabel name as ~ a
  , ElemOf name a as
  , Term s (PAsData b) ~ a
  , PFromDataable b c
  ) =>
  HasField name (HRec as) (Term s c)
  where
  getField = hrecField @name
