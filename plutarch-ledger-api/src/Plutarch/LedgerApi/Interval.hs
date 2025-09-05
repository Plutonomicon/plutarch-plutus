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

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude hiding (psingleton, pto)
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.0.0
data PInterval (a :: S -> Type) (s :: S) = PInterval
  { pinteral'from :: Term s (PLowerBound a)
  , pinteral'to :: Term s (PUpperBound a)
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct (PInterval a))

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable (PInterval a) (Plutus.Interval (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PInterval a)

-- | @since 3.4.0
instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PInterval a))

-- | @since 2.0.0
data PLowerBound (a :: S -> Type) (s :: S)
  = PLowerBound (Term s (PExtended a)) (Term s (PAsData PBool))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct (PLowerBound a))

deriving via
  DeriveDataPLiftable (PLowerBound a) (Plutus.LowerBound (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PLowerBound a)

-- | @since 3.3.0
instance (PIsData a, PCountable a) => PEq (PLowerBound a) where
  {-# INLINEABLE (#==) #-}
  lb1 #== lb2 = (pinclusiveLowerBound # lb1) #== (pinclusiveLowerBound # lb2)

-- | @since 3.3.0
instance (PIsData a, PCountable a) => POrd (PLowerBound a) where
  {-# INLINEABLE (#<=) #-}
  lb1 #<= lb2 = (pinclusiveLowerBound # lb1) #<= (pinclusiveLowerBound # lb2)
  {-# INLINEABLE (#<) #-}
  lb1 #< lb2 = (pinclusiveLowerBound # lb1) #< (pinclusiveLowerBound # lb2)

-- | @since 3.4.0
instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PLowerBound a))

-- | @since 2.0.0
data PUpperBound (a :: S -> Type) (s :: S)
  = PUpperBound (Term s (PExtended a)) (Term s (PAsData PBool))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct (PUpperBound a))

deriving via
  DeriveDataPLiftable (PUpperBound a) (Plutus.UpperBound (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PUpperBound a)

-- | @since 3.4.0
instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PUpperBound a))

-- | @since 3.3.0
instance (PIsData a, PEnumerable a) => PEq (PUpperBound a) where
  {-# INLINEABLE (#==) #-}
  ub1 #== ub2 = (pinclusiveUpperBound # ub1) #== (pinclusiveUpperBound # ub2)

-- | @since 3.3.0
instance (PIsData a, PEnumerable a) => POrd (PUpperBound a) where
  {-# INLINEABLE (#<=) #-}
  ub1 #<= ub2 = (pinclusiveUpperBound # ub1) #<= (pinclusiveUpperBound # ub2)
  {-# INLINEABLE (#<) #-}
  ub1 #< ub2 = (pinclusiveUpperBound # ub1) #< (pinclusiveUpperBound # ub2)

-- | @since 2.0.0
data PExtended (a :: S -> Type) (s :: S)
  = PNegInf
  | PFinite (Term s (PAsData a))
  | PPosInf
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct (PExtended a))

deriving via
  DeriveDataPLiftable (PExtended a) (Plutus.Extended (AsHaskell a))
  instance
    (Plutus.FromData (AsHaskell a), Plutus.ToData (AsHaskell a)) => PLiftable (PExtended a)

-- phoistAcyclic is used because comparators generates very large code.
instance (POrd a, PIsData a) => POrd (PExtended a) where
  x #<= y = f # x # y
    where
      f = phoistAcyclic $
        plam $ \a b ->
          pmatch a $ \a' ->
            pmatch b $ \b' ->
              case (a', b') of
                (PNegInf, PNegInf) -> pconstant True
                (PNegInf, _) -> pconstant True
                (_, PNegInf) -> pconstant False
                (PPosInf, PPosInf) -> pconstant True
                (_, PPosInf) -> pconstant True
                (PPosInf, _) -> pconstant False
                (PFinite l, PFinite r) -> pfromData l #<= pfromData r

  x #< y = f # x # y
    where
      f = phoistAcyclic $
        plam $ \a b ->
          pmatch a $ \a' ->
            pmatch b $ \b' ->
              case (a', b') of
                (PNegInf, PNegInf) -> pconstant False
                (PNegInf, _) -> pconstant True
                (_, PNegInf) -> pconstant False
                (PPosInf, PPosInf) -> pconstant False
                (_, PPosInf) -> pconstant True
                (PPosInf, _) -> pconstant False
                (PFinite l, PFinite r) -> pfromData l #< pfromData r

-- | @since 3.4.0
instance PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PExtended a))

{- | Check if a value is inside the given interval.

@since 3.3.0
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

@since 3.3.0
-}
pisEmpty ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term s (PInterval a :--> PBool)
pisEmpty = phoistAcyclic $ plam $ \i -> unTermCont $ do
  PInterval lowerBound upperBound <- pmatchC i
  let inclusiveLowerBound = pinclusiveLowerBound # lowerBound
  let inclusiveUpperBound = pinclusiveUpperBound # upperBound
  pure $ inclusiveLowerBound #> inclusiveUpperBound

{- | Turn a 'PLowerBound' into a single inclusive bounding value.

@since 3.3.0
-}
pinclusiveLowerBound ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PCountable a) =>
  Term s (PLowerBound a :--> PExtended a)
pinclusiveLowerBound = phoistAcyclic $ plam $ \lb -> unTermCont $ do
  PLowerBound extended closure <- pmatchC lb
  pure $
    pif
      (pfromData closure)
      -- We are already closed
      extended
      ( pmatch extended $ \case
          -- Open at a finite value, get its successor
          PFinite t -> pcon $ PFinite (pdata $ psuccessor # pfromData t)
          -- We have an infinity, who cares
          _ -> extended
      )

{- | Turn a 'PUpperBound' into a single inclusive bounding value.

@since 3.3.0
-}
pinclusiveUpperBound ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PEnumerable a) =>
  Term s (PUpperBound a :--> PExtended a)
pinclusiveUpperBound = phoistAcyclic $ plam $ \ub -> unTermCont $ do
  PUpperBound extended closure <- pmatchC ub
  pure $
    pif
      (pfromData closure)
      -- We are already closed
      extended
      ( pmatch extended $ \case
          -- Open at a finite value, get its predecessor
          PFinite t ->
            pcon $ PFinite (pdata $ ppredecessor # pfromData t)
          -- We have an infinity, who cares
          _ -> extended
      )

{- | @'pcontains' # i1 # i2@ is true if @i2@ is entirely contained in @i1@: more
specifically, if for any @s@, if @'pmember' # s # i2@, then @'pmember' # s #
i1@.

@since 3.3.0
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
  PInterval l1' u1' <- pmatchC i1
  PInterval l2' u2' <- pmatchC i2
  l1 <- pletC l1'
  u1 <- pletC u1'
  l2 <- pletC l2'
  u2 <- pletC u2'
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
    plet (pcon $ PFinite x) $ \start ->
      pclosedInterval # start # start

{- | Given @x@, create the interval @[x, +infty)@

@since 2.1.1
-}
pfrom ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pfrom = phoistAcyclic $
  plam $ \a ->
    let start = pcon $ PFinite a
        end = pcon PPosInf
     in pclosedInterval # start # end

{- | Given @x@, create the interval @(-infty, x]@.

@since 2.1.1
-}
pto ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pto = phoistAcyclic $
  plam $ \a ->
    let start = pcon PNegInf
        end = pcon $ PFinite a
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
    PInterval lowerX upperX <- pmatchC x'
    PInterval lowerY upperY <- pmatchC y'
    let lower = minLower # lowerX # lowerY
    let upper = maxUpper # upperX # upperY
    pure $ pinterval' # lower # upper

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
    PInterval lowerX upperX <- pmatchC x'
    PInterval lowerY upperY <- pmatchC y'
    let lower = maxLower # lowerX # lowerY
    let upper = minUpper # upperX # upperY
    pure $ pinterval' # lower # upper

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
    pmatch y $ \(PInterval lower _) -> unTermCont $ do
      PLowerBound t c <- pmatchC lower
      pure $
        pif
          (pfromData c)
          (pmatch t (ltE' a))
          (pmatch t (leqE' a))

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
    pmatch y $ \(PInterval _ upper) -> unTermCont $ do
      PUpperBound t c <- pmatchC upper
      pure $
        pif
          (pfromData c)
          (pmatch t (gtE' a))
          (pmatch t (geqE' a))

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
    let start = pcon $ PFinite x
        end = pcon $ PFinite y
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
    let closure = pconstant @(PAsData PBool) True
        upper = pcon $ PUpperBound end closure
        lower = pcon $ PLowerBound start closure
     in pinterval' # lower # upper

--  interval from upper and lower
pinterval' ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PLowerBound a
        :--> PUpperBound a
        :--> PInterval a
    )
pinterval' = phoistAcyclic $
  plam $ \lower upper ->
    pcon $
      PInterval lower upper

ltE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
ltE' x = \case
  PNegInf -> pconstant False
  PPosInf -> pconstant True
  PFinite r -> x #< pfromData r

eqE' ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
eqE' a y' = case y' of
  PFinite r -> unTermCont $ do
    pure $ a #== pfromData r
  _ -> pconstant False

minLower ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PLowerBound a :--> PLowerBound a :--> PLowerBound a)
minLower = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    PLowerBound xt' xc <- pmatchC x'
    PLowerBound yt' _ <- pmatchC y'
    xt <- pletC xt'
    yt <- pletC yt'

    pure $
      pif
        (xt #< yt)
        x'
        (pif (yt #< xt) y' (pif' # pfromData xc # x' # y'))

maxLower ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PLowerBound a :--> PLowerBound a :--> PLowerBound a)
maxLower = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    PLowerBound xt' xc <- pmatchC x'
    PLowerBound yt' _ <- pmatchC y'
    xt <- pletC xt'
    yt <- pletC yt'

    pure $
      pif
        (xt #< yt)
        y'
        (pif (yt #< xt) x' (pif' # pfromData xc # y' # x'))

minUpper ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PUpperBound a :--> PUpperBound a :--> PUpperBound a)
minUpper = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    PUpperBound xt' xc <- pmatchC x'
    PUpperBound yt' _ <- pmatchC y'
    xt <- pletC xt'
    yt <- pletC yt'

    pure $
      pif
        (xt #< yt)
        x'
        (pif (yt #< xt) y' (pif' # pfromData xc # y' # x'))

maxUpper ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    (PUpperBound a :--> PUpperBound a :--> PUpperBound a)
maxUpper = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    PUpperBound xt' xc <- pmatchC x'
    PUpperBound yt' _ <- pmatchC y'
    xt <- pletC xt'
    yt <- pletC yt'

    pure $
      pif
        (xt #< yt)
        y'
        (pif (yt #< xt) x' (pif' # pfromData xc # x' # y'))

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
  PNegInf -> pconstant True
  PPosInf -> pconstant False
  PFinite r -> pfromData r #< x
