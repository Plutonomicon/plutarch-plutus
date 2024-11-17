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
import Plutarch.Builtin (
  PAsData,
  PData,
  PIsData (pdataImpl, pfromDataImpl),
  pasConstr,
  pconstrBuiltin,
  pdata,
  pforgetData,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue))
import Plutarch.Builtin.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.DataRepr.Internal (
  DerivePConstantViaData (DerivePConstantViaData),
  PConstantData,
  PLiftData,
 )
import Plutarch.Internal.Builtin (pif, plam, pto)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Ord (POrd, PPartialOrd ((#<), (#<=)))
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType (PInner, pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Show (PShow)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.List (pcons, phead, pnil)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.TryFrom (PTryFrom)

-- | Scott-encoded 'Either'.
data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
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
  = PDLeft (Term s (PAsData a))
  | PDRight (Term s (PAsData b))
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PEq
    , -- | @since WIP
      PShow
    , -- | @since WIP
      POrd
    )

-- | @since WIP
instance
  (PPartialOrd a, PPartialOrd b, PIsData a, PIsData b) =>
  PPartialOrd (PEitherData a b)
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

-- | @since WIP
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
    plet (phead #$ psndBuiltin # asConstr) $ \arg ->
      pif
        ((pfstBuiltin # asConstr) #== 0)
        (f . PDLeft . punsafeCoerce $ arg)
        (f . PDRight . punsafeCoerce $ arg)

-- | @since WIP
instance PIsData (PEitherData a b) where
  {-# INLINEABLE pdataImpl #-}
  pdataImpl = pto
  {-# INLINEABLE pfromDataImpl #-}
  pfromDataImpl = punsafeCoerce

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
  pcon . PDLeft . pdata $ x

{- | Make a @Data@-encoded @Right@.

@since WIP
-}
pdright ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData b =>
  Term s (b :--> PEitherData a b)
pdright = phoistAcyclic $ plam $ \x ->
  pcon . PDRight . pdata $ x

{- | Eliminator for 'PEitherData'.

@since WIP
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
    PDLeft x -> pfromData x
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
    PDRight x -> pfromData x
    PDLeft _ -> ptraceInfoError "pdfromRight: unexpected PDLeft"
