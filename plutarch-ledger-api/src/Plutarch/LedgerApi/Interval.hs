{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is meant to be imported qualified, as some of its identifiers
clash with the Plutarch prelude, as well as other parts of the Plutarch API.
-}
module Plutarch.LedgerApi.Interval (
  -- * Types
  PInterval (..),
  PLowerBound (..),
  PUpperBound (..),
  PExtended (..),

  -- * Functions

  -- ** Creation
  psingleton,
  pfrom,
  pto,
  palways,
  pinterval,
  pinclusiveLowerBound,
  pinclusiveUpperBound,

  -- ** Queries
  pmember,
  pcontains,
  pbefore,
  pafter,
  pisEmpty,

  -- ** Transformation
  phull,
  pintersection,
) where

import Plutarch.Bool (pif')
import Plutarch.DataRepr (PDataFields)
import Plutarch.Enum (
  PCountable (psuccessor),
  PEnumerable (ppredecessor),
 )
import Plutarch.Internal.Lift (DeriveDataPLiftable, PLifted' (PLifted'))
import Plutarch.Prelude hiding (psingleton, pto)
import PlutusLedgerApi.V3 qualified as Plutus

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

-- | @since WIP
deriving via
  DeriveDataPLiftable (PInterval a) (Plutus.Interval (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PInterval a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PInterval a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PInterval a))

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
      POrd
    , -- | @since 2.0.0
      PShow
    )

deriving via
  DeriveDataPLiftable (PLowerBound a) (Plutus.LowerBound (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PLowerBound a)

-- | @since WIP
instance (PIsData a, PCountable a) => PEq (PLowerBound a) where
  {-# INLINEABLE (#==) #-}
  lb1 #== lb2 = (pinclusiveLowerBound # lb1) #== (pinclusiveLowerBound # lb2)

-- | @since WIP
instance (PIsData a, PCountable a) => PPartialOrd (PLowerBound a) where
  {-# INLINEABLE (#<=) #-}
  lb1 #<= lb2 = (pinclusiveLowerBound # lb1) #<= (pinclusiveLowerBound # lb2)
  {-# INLINEABLE (#<) #-}
  lb1 #< lb2 = (pinclusiveLowerBound # lb1) #< (pinclusiveLowerBound # lb2)

-- | @since 2.0.0
instance DerivePlutusType (PLowerBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PLowerBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PLowerBound a))

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
      POrd
    , -- | @since 2.0.0
      PShow
    )

deriving via
  DeriveDataPLiftable (PUpperBound a) (Plutus.UpperBound (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PUpperBound a)

-- | @since WIP
instance (PIsData a, PEnumerable a) => PEq (PUpperBound a) where
  {-# INLINEABLE (#==) #-}
  ub1 #== ub2 = (pinclusiveUpperBound # ub1) #== (pinclusiveUpperBound # ub2)

-- | @since WIP
instance (PIsData a, PEnumerable a) => PPartialOrd (PUpperBound a) where
  {-# INLINEABLE (#<=) #-}
  ub1 #<= ub2 = (pinclusiveUpperBound # ub1) #<= (pinclusiveUpperBound # ub2)
  {-# INLINEABLE (#<) #-}
  ub1 #< ub2 = (pinclusiveUpperBound # ub1) #< (pinclusiveUpperBound # ub2)

-- | @since 2.0.0
instance DerivePlutusType (PUpperBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PUpperBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PUpperBound a))

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

deriving via
  DeriveDataPLiftable (PExtended a) (Plutus.Extended (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PExtended a)

-- | @since 2.0.0
instance DerivePlutusType (PExtended a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PExtended a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PExtended a))

{- | Check if a value is inside the given interval.

@since WIP
-}
pmember ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term
    s
    ( PAsData a
        :--> PInterval a
        :--> PBool
    )
pmember = phoistAcyclic $ plam $ \x i -> pcontains # i # (psingleton # x)

{- | Check if a 'PInterval' is empty.

@since WIP
-}
pisEmpty ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term s (PInterval a :--> PBool)
pisEmpty = phoistAcyclic $ plam $ \i -> unTermCont $ do
  unpacked <- tcont $ pletFields @'["from", "to"] i
  let lowerBound = getField @"from" unpacked
  let upperBound = getField @"to" unpacked
  let inclusiveLowerBound = pinclusiveLowerBound # lowerBound
  let inclusiveUpperBound = pinclusiveUpperBound # upperBound
  pure $ inclusiveLowerBound #> inclusiveUpperBound

{- | Turn a 'PLowerBound' into a single inclusive bounding value.

@since WIP
-}
pinclusiveLowerBound ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PCountable a) =>
  Term s (PLowerBound a :--> PExtended a)
pinclusiveLowerBound = phoistAcyclic $ plam $ \lb -> unTermCont $ do
  unpacked <- tcont $ pletFields @'["_0", "_1"] lb
  let extended = getField @"_0" unpacked
  let closure = getField @"_1" unpacked
  pure $
    pif
      closure
      -- We are already closed
      extended
      ( pmatch extended $ \case
          -- Open at a finite value, get its successor
          PFinite t -> pletFields @'["_0"] t $ \unpackedT ->
            pcon . PFinite $ pdcons # pdata (psuccessor # getField @"_0" unpackedT) # pdnil
          -- We have an infinity, who cares
          _ -> extended
      )

{- | Turn a 'PUpperBound' into a single inclusive bounding value.

@since WIP
-}
pinclusiveUpperBound ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term s (PUpperBound a :--> PExtended a)
pinclusiveUpperBound = phoistAcyclic $ plam $ \ub -> unTermCont $ do
  unpacked <- tcont $ pletFields @'["_0", "_1"] ub
  let extended = getField @"_0" unpacked
  let closure = getField @"_1" unpacked
  pure $
    pif
      closure
      -- We are already closed
      extended
      ( pmatch extended $ \case
          -- Open at a finite value, get its predecessor
          PFinite t -> pletFields @'["_0"] t $ \unpackedT ->
            pcon . PFinite $ pdcons # pdata (ppredecessor # getField @"_0" unpackedT) # pdnil
          -- We have an infinity, who cares
          _ -> extended
      )

{- | @'pcontains' # i1 # i2@ is true if @i2@ is entirely contained in @i1@: more
specifically, if for any @s@, if @'pmember' # s # i2@, then @'pmember' # s #
i1@.

@since WIP
-}
pcontains ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PBool
    )
pcontains = phoistAcyclic $ plam $ \i1 i2 -> unTermCont $ do
  unpackedI1 <- tcont $ pletFields @'["from", "to"] i1
  unpackedI2 <- tcont $ pletFields @'["from", "to"] i2
  let l1 = pfromData $ getField @"from" unpackedI1
  let l2 = getField @"from" unpackedI2
  let u1 = pfromData $ getField @"to" unpackedI1
  let u2 = getField @"to" unpackedI2
  -- Note: This manually inlines `pisEmpty` to avoid redundant unpacking.
  let ilb2 = pinclusiveLowerBound # l2
  let iub2 = pinclusiveUpperBound # u2
  pure $
    pif
      (ilb2 #> iub2)
      -- i2 is empty, so therefore it must be contained in i1
      (pcon PTrue)
      ( unTermCont $ do
          let ilb1 = pinclusiveLowerBound # l1
          let iub1 = pinclusiveUpperBound # u1
          pure $
            pif
              (ilb1 #> iub1)
              -- i1 is empty, so therefore it cannot contain
              -- anything
              (pcon PFalse)
              -- Neither interval is empty, so compare bounds
              ((l1 #<= l2) #&& (u2 #<= u1))
      )

{- | Given @x@, create the interval @[x, x]@.

@since 2.1.1
-}
psingleton ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
psingleton = phoistAcyclic $
  plam $ \x ->
    plet (pcon $ PFinite $ pdcons @"_0" # x # pdnil) $ \start ->
      pclosedInterval # start # start

{- | Given @x@, create the interval @[x, +infty)@

@since 2.1.1
-}
pfrom ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pfrom = phoistAcyclic $
  plam $ \a ->
    let start = pcon $ PFinite $ pdcons @"_0" # a # pdnil
        end = pcon $ PPosInf pdnil
     in pclosedInterval # start # end

{- | Given @x@, create the interval @(-infty, x]@.

@since 2.1.1
-}
pto ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pto = phoistAcyclic $
  plam $ \a ->
    let start = pcon $ PNegInf pdnil
        end = pcon $ PFinite $ pdcons @"_0" # a # pdnil
     in pclosedInterval # start # end

-- TODO: Rename this, as this name is too specific to slots.

{- | Create the interval @(-infty, +infty)@.

@since 2.1.1
-}
palways ::
  forall (a :: S -> Type) (s :: S).
  (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) =>
  Term s (PInterval a)
palways = pconstant Plutus.always

{- | @'phull' i1 i2@ gives the smallest interval that contains both @i1@ and
@i2@.

@since 2.1.1
-}
phull ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PInterval a
    )
phull = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    let lowerX = getField @"from" x
    let upperX = getField @"to" x
    let lowerY = getField @"from" y
    let upperY = getField @"to" y
    let lower = minLower # lowerX # lowerY
    let upper = maxUpper # upperX # upperY
    pure $ pinterval' # pdata lower # pdata upper

{- | @'pintersection' i1 i2@ gives the largest interval that is contained in
both @i1@ and @i2@.

@since 2.1.1
-}
pintersection ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PInterval a
    )
pintersection = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    let lowerX = getField @"from" x
    let upperX = getField @"to" x
    let lowerY = getField @"from" y
    let upperY = getField @"to" y
    let lower = maxLower # lowerX # lowerY
    let upper = minUpper # upperX # upperY
    pure $ pinterval' # pdata lower # pdata upper

{- | @'before' x i@ is true if @x@ is earlier than the start of @i@.

@since 2.1.1
-}
pbefore ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( a
        :--> PInterval a
        :--> PBool
    )
pbefore = phoistAcyclic $
  plam $ \a y ->
    let lower = pfield @"from" # y
     in pbefore' # a # (lToE # lower)

{- | @'after' x u@ is true if @x@ is later than the end of @i@.

@since 2.1.1
-}
pafter ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( a
        :--> PInterval a
        :--> PBool
    )
pafter = phoistAcyclic $
  plam $ \a y ->
    let upper = pfield @"to" # y
     in pafter' # a # (uToE # upper)

{- | @'pinterval' x y@ creates the interval @[x, y]@.

@since 2.1.1
-}
pinterval ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PAsData a
        :--> PAsData a
        :--> PInterval a
    )
pinterval = phoistAcyclic $
  plam $ \x y ->
    let start = pcon $ PFinite $ pdcons @"_0" # x # pdnil
        end = pcon $ PFinite $ pdcons @"_0" # y # pdnil
     in pclosedInterval # start # end

-- Helpers

-- closed interval from PExtended
pclosedInterval ::
  forall (a :: S -> Type) (s :: S).
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

eqE' ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
eqE' a y' = case y' of
  PFinite r -> unTermCont $ do
    b <- tcont $ plet $ pfield @"_0" # r
    pure $ a #== b
  _ -> pconstant False

minLower ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PLowerBound a :--> PLowerBound a :--> PLowerBound a)
minLower = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["_0", "_1"] (lToE # x')
    y <- tcont $ pletFields @'["_0", "_1"] (lToE # y')
    let xt = getField @"_0" x
    let yt = getField @"_0" y
    let xc = getField @"_1" x
    pure $
      pif
        (pfromData xt #< pfromData yt)
        x'
        (pif (pfromData yt #< pfromData xt) y' (pif' # xc # x' # y'))

maxLower ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PLowerBound a :--> PLowerBound a :--> PLowerBound a)
maxLower = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["_0", "_1"] (lToE # x')
    y <- tcont $ pletFields @'["_0", "_1"] (lToE # y')
    let xt = getField @"_0" x
    let yt = getField @"_0" y
    let xc = getField @"_1" x
    pure $
      pif
        (pfromData xt #< pfromData yt)
        y'
        (pif (pfromData yt #< pfromData xt) x' (pif' # xc # y' # x'))

minUpper ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PUpperBound a :--> PUpperBound a :--> PUpperBound a)
minUpper = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["_0", "_1"] (uToE # x')
    y <- tcont $ pletFields @'["_0", "_1"] (uToE # y')
    let xt = getField @"_0" x
    let yt = getField @"_0" y
    let xc = getField @"_1" x
    pure $
      pif
        (pfromData xt #< pfromData yt)
        x'
        (pif (pfromData yt #< pfromData xt) y' (pif' # xc # y' # x'))

maxUpper ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PUpperBound a :--> PUpperBound a :--> PUpperBound a)
maxUpper = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["_0", "_1"] (uToE # x')
    y <- tcont $ pletFields @'["_0", "_1"] (uToE # y')
    let xt = getField @"_0" x
    let yt = getField @"_0" y
    let xc = getField @"_1" x
    pure $
      pif
        (pfromData xt #< pfromData yt)
        y'
        (pif (pfromData yt #< pfromData xt) x' (pif' # xc # x' # y'))

-- value < endpoint
pbefore' ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( a
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
pbefore' = phoistAcyclic $
  plam $ \a y' -> unTermCont $ do
    y <- tcont $ pletFields @'["_0", "_1"] y'
    let yt = getField @"_0" y
    let yc = getField @"_1" y
    pure $
      pif
        yc
        (pmatch yt (ltE' a))
        (pmatch yt (leqE' a))

-- value > endpoint
pafter' ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( a
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
pafter' = phoistAcyclic $
  plam $ \a y' -> unTermCont $ do
    y <- tcont $ pletFields @'["_0", "_1"] y'
    let yt = getField @"_0" y
    let yc = getField @"_1" y
    pure $
      pif
        yc
        (pmatch yt (gtE' a))
        (pmatch yt (geqE' a))

-- value <= PExtended
leqE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
leqE' a y = ltE' a y #|| eqE' a y

-- value >= PExtended
geqE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
geqE' a y = gtE' a y #|| eqE' a y

-- value > PExtended
gtE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
gtE' x = \case
  PNegInf _ -> pconstant True
  PPosInf _ -> pconstant False
  PFinite r ->
    let y = pfield @"_0" # r
     in y #< x
