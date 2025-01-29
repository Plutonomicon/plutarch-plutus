{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Newtype (PlutusTypeNewtype) where

import Generics.SOP qualified as SOP
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
import Plutarch.Internal.PlutusType (
  DerivedPInner,
  PlutusTypeStrat,
  PlutusTypeStratConstraint,
  derivedPCon,
  derivedPMatch,
 )
import Plutarch.Internal.Term (PType)

data PlutusTypeNewtype

class (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: PType)
instance (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: PType)

instance PlutusTypeStrat PlutusTypeNewtype where
  type PlutusTypeStratConstraint PlutusTypeNewtype = Helper
  type DerivedPInner PlutusTypeNewtype a = GetPNewtype a
  derivedPCon x = case gpfrom x of
    SOP.SOP (SOP.Z (x SOP.:* SOP.Nil)) -> x
    SOP.SOP (SOP.S x) -> case x of {}
  derivedPMatch x f = f (gpto $ SOP.SOP $ SOP.Z $ x SOP.:* SOP.Nil)

type family GetPNewtype' (a :: [[PType]]) :: PType where
  GetPNewtype' '[ '[a]] = a

type family GetPNewtype (a :: PType) :: PType where
  GetPNewtype a = GetPNewtype' (PCode a)
