{-# LANGUAGE UndecidableInstances #-}

{- NOTE: Plutarch's Generic infrastructure is in its fancy, and more
functionality may be added latter.
-}
module Plutarch.Generic (
  GetPDataRecordArgs,
) where

import GHC.TypeLits
import Plutarch.DataRepr (PDataRecord, PLabeled)
import Plutarch.Prelude

-- TODO: Default type instance in PIsDataRepr
type family GetPDataRecordArgs (a :: [[Type]]) :: [[PLabeled]] where
  GetPDataRecordArgs xs = ToPLabeled2 xs

type ToPLabeled :: [Type] -> [PLabeled]
type family ToPLabeled as where
  ToPLabeled '[] = '[]
  ToPLabeled '[Term s (PDataRecord fs)] = fs
  ToPLabeled '[_] = TypeError ( 'Text "Expected PDataRecord")
  ToPLabeled _ = TypeError ( 'Text "Must have 0 or 1 argument in sum constructor")

-- Unfortunately we can't write a generic FMap due to ghc's arity limitations.
type ToPLabeled2 :: [[Type]] -> [[PLabeled]]
type family ToPLabeled2 as where
  ToPLabeled2 '[] = '[]
  ToPLabeled2 (a ': as) = ToPLabeled a ': ToPLabeled2 as
