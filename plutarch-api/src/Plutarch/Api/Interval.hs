{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is meant to be imported qualified, as some of its identifiers
clash with the Plutarch prelude, as well as other parts of the Plutarch API.
-}
module Plutarch.Api.Interval (
  -- * Types
  PInterval (..),
  PLowerBound (..),
  PUpperBound (..),
  PExtended (..),

  -- * Functions

  -- ** Creation
  psingleton,

  -- ** Queries
  pmember,
  pcontains,
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude hiding (psingleton)
import PlutusLedgerApi.V2 qualified as Plutus

-- | @since 2.0.0
newtype PInterval (a :: S -> Type) (s :: S)
  = PInterval
      ( Term
          s
          ( PDataRecord
              '[ "from" ':= PLowerBound a
               , "to" ':= PUpperBound a
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PInterval a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PInterval a)
  where
  type PLifted (PInterval a) = (Plutus.Interval (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.Interval a) (PInterval (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.Interval a)

-- | @since 2.0.0
newtype PLowerBound (a :: S -> Type) (s :: S)
  = PLowerBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PBool
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PLowerBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PLowerBound a)
  where
  type PLifted (PLowerBound a) = (Plutus.LowerBound (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.LowerBound a) (PLowerBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.LowerBound a)

-- | @since 2.0.0
newtype PUpperBound (a :: S -> Type) (s :: S)
  = PUpperBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PBool
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PUpperBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PUpperBound a)
  where
  type PLifted (PUpperBound a) = (Plutus.UpperBound (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.UpperBound a) (PUpperBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.UpperBound a)

-- | @since 2.0.0
data PExtended (a :: S -> Type) (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PExtended a) where
  type DPTStrat _ = PlutusTypeData

{- | Check if a value is inside the given interval.

@since 2.1.1
-}
pmember ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PAsData a
        :--> PInterval a
        :--> PBool
    )
pmember = phoistAcyclic $ plam $ \x i -> pcontains # i # (psingleton # x)

{- | @'pcontains' # x # y@ is true if the interval @y@ is entirely contained in @a@.

@since 2.1.1
-}
pcontains ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PBool
    )
pcontains = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    lowerX <- tcont . plet $ pfield @"from" # x
    upperX <- tcont . plet $ pfield @"to" # x
    lowerY <- tcont . plet $ pfield @"from" # y
    upperY <- tcont . plet $ pfield @"to" # y
    pure $ leqP # (lToE # lowerX) # (lToE # lowerY) #&& leqP # (uToE # upperY) # (uToE # upperX)

{-
  x <- pletFields @'["from", "to"] x'
  y <- pletFields @'["from", "to"] y'
  let lowerX = pfield @"from" x
      upperX = pfield @"to" x
      lowerY = pfield @"from" y
      upperY = pfield @"to" y
  leqP # (lToE # lowerX) # (lToE # lowerY) #&& leqP # (uToE # upperY) # (uToE # upperX)
-}

{- | Given @x@, create the interval @[x, x]@.

@since 2.1.1
-}
psingleton ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PAsData a :--> PInterval a)
psingleton = phoistAcyclic $
  plam $ \x ->
    plet (pcon $ PFinite $ pdcons @"_0" # x # pdnil) $ \start ->
      pclosedInterval # start # start

-- Helpers

-- closed interval from PExtended
pclosedInterval ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PInterval a
    )
pclosedInterval = phoistAcyclic $
  plam $ \start end ->
    let closure = pconstantData True
        upper =
          pcon $
            PUpperBound $
              pdcons @"_0"
                # pdata end
                #$ pdcons @"_1"
                # closure
                # pdnil
        lower =
          pcon $
            PLowerBound $
              pdcons @"_0"
                # pdata start
                #$ pdcons @"_1"
                # closure
                # pdnil
     in pinterval' # pdata lower # pdata upper

--  interval from upper and lower
pinterval' ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term
    s
    ( PAsData (PLowerBound a)
        :--> PAsData (PUpperBound a)
        :--> PInterval a
    )
pinterval' = phoistAcyclic $
  plam $ \lower upper ->
    pcon $
      PInterval $
        pdcons @"from"
          # lower
          #$ pdcons @"to"
          # upper
          # pdnil

leqP ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term
    s
    ( PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
leqP = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- pletFields @'["_0", "_1"] x'
    y <- pletFields @'["_0", "_1"] y'
    xt <- plet $ pfield @"_0" # x
    yt <- plet $ pfield @"_0" # y
    xc <- plet $ pfield @"_1" # x
    yc <- plet $ pfield @"_1" # y
    pif
      (xc #&& yc #|| (pnot # xc) #&& (pnot # yc))
      (leqE # xt # yt)
      (ltE # xt # yt)

leqE ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PBool
    )
leqE = phoistAcyclic $ plam $ \x y -> ltE # x # y #|| eqE # x # y

ltE ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PBool
    )
ltE = phoistAcyclic $ plam $ \x y -> pmatch x (cont y)
  where
    cont y' x' = case x' of
      PNegInf _ -> pconstant True
      PPosInf _ -> pmatch y' isPosInf
      PFinite l -> pmatch y' (ltE' $ pfield @"_0" # l)

isPosInf ::
  forall (a :: S -> Type) (s :: S).
  PExtended a s ->
  Term s PBool
isPosInf = \case
  PPosInf _ -> pconstant True
  _ -> pconstant False

ltE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
ltE' x = \case
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> x #< pfield @"_0" # r

lToE ::
  forall (a :: S -> Type) (s :: S).
  Term s (PLowerBound a :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool])
lToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PLowerBound a) -> a)

uToE ::
  forall (a :: S -> Type) (s :: S).
  Term s (PUpperBound a :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool])
uToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PUpperBound a) -> a)
