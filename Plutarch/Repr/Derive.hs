{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Repr.Derive (DerivePLiftableAsRepr (DerivePLiftableAsRepr)) where

import Data.Kind (Type)
import GHC.Exts (Any)
import GHC.Generics (Generic)
import Generics.SOP (Code, SOP)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (
  AsHaskell,
  LiftError,
  PLiftable,
  PlutusRepr,
  haskToRepr,
  plutToRepr,
  punsafeCoercePLifted,
  reprToHask,
  reprToPlut,
 )
import Plutarch.Internal.PlutusType (DeriveFakePlutusType (DeriveFakePlutusType), PInner, PlutusType)
import Plutarch.Internal.Term (S)
import Plutarch.Repr.Internal (StructAsHaskell, UnTermStruct')

{- |
This is @PLiftable@ derivation helper for user-defined datatypes like Data/SOP encoded types.
Please consult example below.

@@
data PBobData (a :: S -> Type) (s :: S)
  = PBobData (Term s (PAsData a)) (Term s (PAsData PBool))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving PlutusType via (DeriveAsDataRec (PBobData a)) -- SOP encoding works as well.

deriving via
  DerivePLiftableAsRepr (PBobData a) (Bob (AsHaskell a))
  instance
    PLiftable (PAsData a) => PLiftable (PBobData a)
@@

 @since WIP
-}
newtype DerivePLiftableAsRepr (wrapper :: S -> Type) (h :: Type) (s :: S)
  = DerivePLiftableAsRepr (wrapper s)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveFakePlutusType (DerivePLiftableAsRepr wrapper h))

-- | @since WIP
instance
  forall wrapper h (struct' :: [[Type]]) (struct :: [[S -> Type]]) (hstruct :: [[Type]]).
  ( PLiftable (PInner wrapper)
  , SOP.Generic (wrapper Any)
  , SOP.Generic h
  , hstruct ~ Code h
  , struct' ~ Code (wrapper Any)
  , struct ~ UnTermStruct' struct'
  , hstruct ~ StructAsHaskell struct
  , AsHaskell (PInner wrapper) ~ SOP SOP.I hstruct
  ) =>
  PLiftable (DerivePLiftableAsRepr wrapper h)
  where
  type AsHaskell (DerivePLiftableAsRepr wrapper h) = h
  type PlutusRepr (DerivePLiftableAsRepr wrapper h) = PlutusRepr (PInner wrapper)
  haskToRepr :: h -> PlutusRepr (PInner wrapper)
  haskToRepr x = haskToRepr @(PInner wrapper) $ SOP.from x
  reprToHask :: PlutusRepr (PInner wrapper) -> Either LiftError h
  reprToHask x = SOP.to <$> reprToHask @(PInner wrapper) x
  reprToPlut x = punsafeCoercePLifted $ reprToPlut @(PInner wrapper) x
  plutToRepr x = plutToRepr @(PInner wrapper) $ punsafeCoercePLifted x
