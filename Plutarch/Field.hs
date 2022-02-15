{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Field (PField (..), FieldName (..), pfield) where

import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol)

--------------------------------------------------------------------------------

import Plutarch.Builtin (PAsData)
import Plutarch.DataRepr (IndexList, PDataList, pindexDataList)
import Plutarch.Prelude

--------------------------------------------------------------------------------

class PField name rec a | name rec -> a where
  pfield' :: (Term s (rec :--> a))

data FieldName name = FieldName

instance (KnownSymbol name) => IsLabel (name :: Symbol) (FieldName name) where
  fromLabel = FieldName

pfield ::
  forall name rec a s.
  (PField name rec a) =>
  (FieldName name) ->
  (Term s (rec :--> a))
pfield _ = pfield' @name @rec @a

--------------------------------------------------------------------------------

{- |
  Convert a type-level symbol to a type-level Natural.

  With base 4.16.0.0 we could have `NatToChar` ...
-}
type family SymbolToNat (sym :: Symbol) :: Nat where
  SymbolToNat "_0" = 0
  SymbolToNat "_1" = 1
  SymbolToNat "_2" = 2
  SymbolToNat "_3" = 3
  SymbolToNat "_4" = 4
  SymbolToNat "_5" = 5
  SymbolToNat "_6" = 6
  SymbolToNat "_7" = 7
  SymbolToNat "_8" = 8
  SymbolToNat "_9" = 9
  SymbolToNat "_10" = 10
  SymbolToNat "_11" = 11
  SymbolToNat "_12" = 12
  SymbolToNat "_13" = 13
  SymbolToNat "_14" = 14
  SymbolToNat "_15" = 15
  SymbolToNat "_16" = 16
  SymbolToNat "_17" = 17
  SymbolToNat "_18" = 18
  SymbolToNat "_19" = 19
  SymbolToNat "_20" = 20

instance
  ( (SymbolToNat name) ~ n
  , KnownNat n
  , (IndexList n as) ~ a
  ) =>
  PField name (PDataList as) (PAsData a)
  where
  pfield' = pindexDataList (Proxy @(SymbolToNat name))

--------------------------------------------------------------------------------

data HList (as :: [Type]) :: Type where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 4 :::

class IndexHList (n :: Nat) (xs :: [Type]) a | xs n -> a where
  hInd :: HList xs -> a
