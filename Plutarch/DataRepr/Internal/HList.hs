{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.HList (
  -- * HRec and Label types
  HRec (HNil, HCons),
  HRecGeneric (HRecGeneric),
  Labeled (Labeled, unLabeled),

  -- * Field indexing functions
  hrecField,
  hrecField',

  -- * Type families
  type IndexList,
  type IndexLabel,
  type SingleItem,
  type Drop,

  -- * Internal utils
  Elem (..),
  ElemOf (..),
) where

import Data.Kind (Constraint, Type)
import GHC.Records (HasField, getField)
import GHC.TypeLits (Symbol)
import Plutarch.Builtin (PAsData)
import Plutarch.DataRepr.Internal.FromData (PFromDataable, pmaybeFromAsData)
import Plutarch.DataRepr.Internal.HList.Utils (
  Drop,
  Elem (Here, There),
  IndexLabel,
  IndexList,
  Labeled (Labeled, unLabeled),
  SingleItem,
 )
import Plutarch.Internal (Term)
import Plutarch.Internal.TypeFamily (Snd)

--------------------------------------------------------------------------------
---------- HList and HRec types

type HRec :: [(Symbol, Type)] -> Type
data HRec as where
  HNil :: HRec '[]
  HCons :: Labeled name a -> HRec as -> HRec ('(name, a) ': as)

---------- Field indexing functions

-- | Index HRec using Elem
indexHRec :: HRec as -> (forall a. Elem a as -> Snd a)
indexHRec (HCons x _) Here = unLabeled x
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
  ElemOf name a as =>
  HRec as ->
  a
hrecField' xs = indexHRec xs $ elemOf @name @a @as

---------- Internal utils

{- |
  Construct an `Elem` via Nat.

  This class could instead be a more direct version of 'indexHList',
  but perhaps the `Elem` encoding will be useful.
-}
type ElemOf :: Symbol -> Type -> [(Symbol, Type)] -> Constraint
class IndexLabel name as ~ a => ElemOf name a as | as name -> a where
  -- | Construct the `Elem` corresponding to a Nat index.
  --
  --    Example:
  --
  --    >>> natElem @_ @0
  --    Here
  --
  --    >>> natElem @_ @3
  --    There (There (There Here))
  elemOf :: Elem '(name, a) as

instance ElemOf name a ('(name, a) ': as) where
  elemOf :: Elem '(name, a) ('(name, a) ': as)
  elemOf = Here

instance
  {-# OVERLAPPABLE #-}
  ( IndexLabel name (b ': as) ~ a
  , ElemOf name a as
  ) =>
  ElemOf name a (b ': as)
  where
  elemOf :: Elem '(name, a) (b ': as)
  elemOf = There (elemOf @name @a @as)

{- |
  Index a `HRec` with a field in a provided list of data fields.
  Implicitly unwraps `PAsData a` to `a` when necessary.

  >>> xs = HRec @["x", "y", "z"] (HCons 1 (HCons 2 (HCons 3 HNil)))
  >>> hrecField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hrecField ::
  forall name c as a b s.
  ( ElemOf name a as
  , Term s (PAsData b) ~ a
  , PFromDataable b c
  ) =>
  HRec as ->
  Term s c
hrecField xs =
  pmaybeFromAsData @b @c $ hrecField' @name xs
{-# DEPRECATED hrecField "please use getField from GHC.Records" #-}

---------- HasField instances
instance
  forall name c as a b s.
  ( IndexLabel name as ~ a
  , ElemOf name a as
  , Term s (PAsData b) ~ a
  , PFromDataable b c
  ) =>
  HasField name (HRec as) (Term s c)
  where
  getField = hrecField @name

-- Generic HRec

newtype HRecGeneric as = HRecGeneric (HRec as)

instance
  forall name a as.
  ( IndexLabel name as ~ a
  , ElemOf name a as
  ) =>
  HasField name (HRecGeneric as) a
  where
  getField (HRecGeneric x) = hrecField' @name x
