{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Either (
  -- * Types
  PEither (PLeft, PRight),
  PEitherData (PDLeft, PDRight),

  -- * Functions

  -- ** PEither

  -- *** Elimination
  pisLeft,
  pfromLeft,
  pfromRight,

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
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (
  PBool (PFalse, PTrue),
  pif,
 )
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinPair (PBuiltinPair),
  PData,
  pasConstr,
  pconstrBuiltin,
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.IsData (PIsData (pdataImpl, pfromDataImpl), pdata, pforgetData, pfromData)
import Plutarch.Internal.Lift (
  DeriveDataPLiftable,
  PLiftable (
    AsHaskell,
    PlutusRepr,
    haskToRepr,
    plutToRepr,
    reprToHask,
    reprToPlut
  ),
  PLifted (PLifted),
  PLiftedClosed,
  getPLiftedClosed,
  mkPLifted,
  mkPLiftedClosed,
  pconstant,
  pliftedFromClosed,
  pliftedToClosed,
 )
import Plutarch.Internal.ListLike (pcons, phead, pnil)
import Plutarch.Internal.Ord (POrd (pmax, pmin, (#<), (#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType (PInner, pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Show (PShow)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TryFrom (PTryFrom)
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))
import Plutarch.Trace (ptraceInfoError)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 qualified as Plutus

{- | SOP-encoded 'Either'.

@since 1.10.0
-}
data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock
    ( -- | @since 1.10.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.10.0
      SOP.Generic
    , -- | @since 1.10.0
      PEq
    , -- | @since 1.10.0
      PShow
    )

-- | @since 1.10.0
deriving via
  DeriveAsSOPStruct (PEither a b)
  instance
    PlutusType (PEither a b)

-- | @since 1.10.0
instance (PLiftable a, PLiftable b) => PLiftable (PEither a b) where
  type AsHaskell (PEither a b) = Either (AsHaskell a) (AsHaskell b)
  type PlutusRepr (PEither a b) = PLiftedClosed (PEither a b)
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = \case
    Left x -> mkPLiftedClosed $ pcon $ PLeft (pconstant @a x)
    Right x -> mkPLiftedClosed $ pcon $ PRight (pconstant @b x)
  {-# INLINEABLE reprToHask #-}
  reprToHask x = do
    isLeft :: Bool <- plutToRepr $ mkPLifted (pisLeft # getPLiftedClosed x)
    if isLeft
      then do
        lr :: PlutusRepr a <- plutToRepr $ mkPLifted (pfromLeft # getPLiftedClosed x)
        lh :: AsHaskell a <- reprToHask @a lr
        pure $ Left lh
      else do
        rr :: PlutusRepr b <- plutToRepr $ mkPLifted (pfromRight # getPLiftedClosed x)
        rh :: AsHaskell b <- reprToHask @b rr
        pure $ Right rh
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = pliftedFromClosed
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = Right . pliftedToClosed

-- | @since 1.10.0
pisLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEither a b :--> PBool)
pisLeft = phoistAcyclic $ plam $ \t -> pmatch t $ \case
  PLeft _ -> pcon PTrue
  PRight _ -> pcon PFalse

-- | @since 1.10.0
pfromLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEither a b :--> a)
pfromLeft = phoistAcyclic $ plam $ \t -> pmatch t $ \case
  PLeft x -> x
  PRight _ -> ptraceInfoError "pfromLeft: used on a PRight"

-- | @since 1.10.0
pfromRight ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEither a b :--> b)
pfromRight = phoistAcyclic $ plam $ \t -> pmatch t $ \case
  PLeft _ -> ptraceInfoError "pfromRight: used on a PLeft"
  PRight x -> x

{- | @Data@-encoded 'Either'.

@since 1.10.0
-}
data PEitherData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PDLeft (Term s (PAsData a))
  | PDRight (Term s (PAsData b))
  deriving stock
    ( -- | @since 1.10.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.10.0
      PEq
    , -- | @since 1.10.0
      PShow
    )

-- | @since 1.10.0
instance
  (POrd a, POrd b, PIsData a, PIsData b) =>
  POrd (PEitherData a b)
  where
  {-# INLINEABLE (#<=) #-}
  t1 #<= t2 = pmatch t1 $ \case
    PDLeft t1' -> pmatch t2 $ \case
      PDLeft t2' -> pfromData t1' #<= pfromData t2'
      PDRight _ -> pcon PTrue
    PDRight t1' -> pmatch t2 $ \case
      PDLeft _ -> pcon PFalse
      PDRight t2' -> pfromData t1' #<= pfromData t2'
  {-# INLINEABLE (#<) #-}
  t1 #< t2 = pmatch t1 $ \case
    PDLeft t1' -> pmatch t2 $ \case
      PDLeft t2' -> pfromData t1' #< pfromData t2'
      PDRight _ -> pcon PTrue
    PDRight t1' -> pmatch t2 $ \case
      PDLeft _ -> pcon PFalse
      PDRight t2' -> pfromData t1' #< pfromData t2'
  {-# INLINEABLE pmax #-}
  pmax t1 t2 = pmatch t1 $ \case
    PDLeft t1' -> pmatch t2 $ \case
      PDLeft t2' -> pif (pfromData t1' #< pfromData t2') t2 t1
      PDRight _ -> t2
    PDRight t1' -> pmatch t2 $ \case
      PDLeft _ -> t1
      PDRight t2' -> pif (pfromData t1' #< pfromData t2') t2 t1
  {-# INLINEABLE pmin #-}
  pmin t1 t2 = pmatch t1 $ \case
    PDLeft t1' -> pmatch t2 $ \case
      PDLeft t2' -> pif (pfromData t1' #< pfromData t2') t1 t2
      PDRight _ -> t1
    PDRight t1' -> pmatch t2 $ \case
      PDLeft _ -> t2
      PDRight t2' -> pif (pfromData t1' #< pfromData t2') t1 t2

-- | @since 1.10.0
instance PlutusType (PEitherData a b) where
  type PInner (PEitherData a b) = PData
  {-# INLINEABLE pcon' #-}
  pcon' = \case
    PDLeft t ->
      pforgetData $ pconstrBuiltin # 0 #$ pcons # pforgetData t # pnil
    PDRight t ->
      pforgetData $ pconstrBuiltin # 1 #$ pcons # pforgetData t # pnil
  {-# INLINEABLE pmatch' #-}
  pmatch' t f = plet (pasConstr # t) $ \asConstr ->
    pmatch asConstr $ \(PBuiltinPair tag dat) ->
      plet (phead # dat) $ \arg ->
        pif
          (tag #== 0)
          (f . PDLeft . punsafeCoerce $ arg)
          (f . PDRight . punsafeCoerce $ arg)

-- | @since 1.10.0
deriving via
  DeriveDataPLiftable (PEitherData a b) (Either (AsHaskell a) (AsHaskell b))
  instance
    ( Plutus.ToData (AsHaskell a)
    , Plutus.FromData (AsHaskell a)
    , Plutus.ToData (AsHaskell b)
    , Plutus.FromData (AsHaskell b)
    ) =>
    PLiftable (PEitherData a b)

-- | @since 1.10.0
instance PIsData (PEitherData a b) where
  {-# INLINEABLE pdataImpl #-}
  pdataImpl = pto
  {-# INLINEABLE pfromDataImpl #-}
  pfromDataImpl = punsafeCoerce

-- | @since 1.10.0
instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PEitherData a b)

-- | @since 1.10.0
instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PAsData (PEitherData a b))

{- | Make a @Data@-encoded @Left@.

@since 1.10.0
-}
pdleft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData a =>
  Term s (a :--> PEitherData a b)
pdleft = phoistAcyclic $ plam $ \x ->
  pcon . PDLeft . pdata $ x

{- | Make a @Data@-encoded @Right@.

@since 1.10.0
-}
pdright ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData b =>
  Term s (b :--> PEitherData a b)
pdright = phoistAcyclic $ plam $ \x ->
  pcon . PDRight . pdata $ x

{- | Eliminator for 'PEitherData'.

@since 1.10.0
-}
peitherData ::
  forall (a :: S -> Type) (b :: S -> Type) (r :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> r) :--> (b :--> r) :--> PEitherData a b :--> r)
peitherData = phoistAcyclic $ plam $ \whenLeft whenRight t ->
  pmatch t $ \case
    PDLeft x -> whenLeft # pfromData x
    PDRight x -> whenRight # pfromData x

{- | Verifies if a 'PEitherData' is a 'PDLeft'. Less code than using
'peitherData', as it doesn't need to inspect the contents.

@since 1.10.0
-}
pdisLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEitherData a b :--> PBool)
pdisLeft = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDLeft _ -> pcon PTrue
    PDRight _ -> pcon PFalse

{- | As 'pdisLeft', except verifies whether we have a 'PDRight'.

@since 1.10.0
-}
pdisRight ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PEitherData a b :--> PBool)
pdisRight = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDRight _ -> pcon PTrue
    PDLeft _ -> pcon PFalse

{- | Return the value inside a 'PDEither' if it's a 'PDLeft', error otherwise.

@since 1.10.0
-}
pdfromLeft ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PEitherData a b :--> a)
pdfromLeft = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDLeft x -> pfromData x
    PDRight _ -> ptraceInfoError "pdfromLeft: unexpected PDRight"

{- | As 'pdfromLeft', but yields a value if given a 'PDRight' instead.

@since 1.10.0
-}
pdfromRight ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData b =>
  Term s (PEitherData a b :--> b)
pdfromRight = phoistAcyclic $ plam $ \t ->
  pmatch t $ \case
    PDRight x -> pfromData x
    PDLeft _ -> ptraceInfoError "pdfromRight: unexpected PDLeft"
