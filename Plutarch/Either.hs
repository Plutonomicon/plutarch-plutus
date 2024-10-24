{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Either (
  -- * Types
  PEither (PLeft, PRight),
  PEitherData (PDLeft, PDRight),

  -- * Functions

  -- ** PEitherData

  -- *** Construction
  pdleft,
  pdright,

  -- *** Elimination
  peitherData,
  pdisLeft,
  pdisRight,
  pdfromLeft,
  pdfromRight,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch (
  DPTStrat,
  DerivePlutusType,
  PType,
  PlutusType,
  PlutusTypeScott,
  S,
  Term,
  pcon,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, PPartialOrd)
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.DataRepr.Internal (
  DerivePConstantViaData (DerivePConstantViaData),
  PConstantData,
  PDataRecord,
  PLabeledType ((:=)),
  PLiftData,
  PlutusTypeData,
  pdcons,
  pdnil,
 )
import Plutarch.DataRepr.Internal.Field (pfield)
import Plutarch.Lift (PConstantDecl (PConstanted), PUnsafeLiftDecl (PLifted))
import Plutarch.Show (PShow)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.TryFrom (PTryFrom)

-- | Scott-encoded 'Either'.
data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PEither a b) where
  type DPTStrat _ = PlutusTypeScott

{- | @Data@-encoded 'Either'.

@since WIP
-}
data PEitherData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PDLeft (Term s (PDataRecord '["_0" ':= a]))
  | PDRight (Term s (PDataRecord '["_0" ':= b]))
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    , -- | @since WIP
      PIsData
    , -- | @since WIP
      PEq
    , -- | @since WIP
      PShow
    , -- | @since WIP
      PPartialOrd
    , -- | @since WIP
      POrd
    )

-- | @since WIP
instance DerivePlutusType (PEitherData a b) where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PEitherData a b)

-- | @since WIP
instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PAsData (PEitherData a b))

-- | @since WIP
instance (PLiftData a, PLiftData b) => PUnsafeLiftDecl (PEitherData a b) where
  type PLifted (PEitherData a b) = Either (PLifted a) (PLifted b)

-- | @since WIP
deriving via
  (DerivePConstantViaData (Either a b) (PEitherData (PConstanted a) (PConstanted b)))
  instance
    (PConstantData a, PConstantData b) => PConstantDecl (Either a b)

{- | Make a @Data@-encoded @Left@.

@since WIP
-}
pdleft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData a =>
  Term s (a :--> PEitherData a b)
pdleft = phoistAcyclic $ plam $ \x ->
  pcon $ PDLeft $ pdcons @"_0" # pdata x #$ pdnil

{- | Make a @Data@-encoded @Right@.

@since WIP
-}
pdright ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData b =>
  Term s (b :--> PEitherData a b)
pdright = phoistAcyclic $ plam $ \x ->
  pcon $ PDRight $ pdcons @"_0" # pdata x #$ pdnil

{- | Eliminator for 'PEitherData'.

@since WIP
-}
peitherData ::
  forall (a :: S -> Type) (b :: S -> Type) (r :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> r) :--> (b :--> r) :--> PEitherData a b :--> r)
peitherData = phoistAcyclic $ plam $ \whenLeft whenRight t ->
  pmatch t $ \case
    PDLeft x -> whenLeft #$ pfield @"_0" # x
    PDRight x -> whenRight #$ pfield @"_0" # x

{- | Verifies if a 'PEitherData' is a 'PDLeft'. Less code than using
'peitherData', as it doesn't need to inspect the contents.

@since WIP
-}
pdisLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEitherData a b :--> PBool)
pdisLeft = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDLeft _ -> pcon PTrue
    PDRight _ -> pcon PFalse

{- | As 'pdisLeft', except verifies whether we have a 'PDRight'.

@since WIP
-}
pdisRight ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEitherData a b :--> PBool)
pdisRight = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDRight _ -> pcon PTrue
    PDLeft _ -> pcon PFalse

{- | Return the value inside a 'PDEither' if it's a 'PDLeft', error otherwise.

@since WIP
-}
pdfromLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PEitherData a b :--> a)
pdfromLeft = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDLeft x -> pfield @"_0" # x
    PDRight _ -> ptraceInfoError "pdfromLeft: unexpected PDRight"

{- | As 'pdfromLeft', but yields a value if given a 'PDRight' instead.

@since WIP
-}
pdfromRight ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData b =>
  Term s (PEitherData a b :--> b)
pdfromRight = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDRight x -> pfield @"_0" # x
    PDLeft _ -> ptraceInfoError "pdfromRight: unexpected PDLeft"
