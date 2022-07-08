{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Plutarch.Extra.Interval (
  member,
  interval,
  from,
  to,
  always,
  never,
  singleton,
  hull,
  intersection,
  contains,
  before,
  after,
) where

import Plutarch.Api.V1.Interval (
  PClosure,
  PExtended (PFinite, PNegInf, PPosInf),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PUpperBound (PUpperBound),
 )
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

-- check if `a` belongs to interval `i`
member ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s a ->
  Term s (PInterval a) ->
  Term s PBool
member a i = i `contains` (singleton a)

{- | create an interval that includes all values that are greater than or equal
 - to a and smaller than or equal to b
-}
interval ::
  forall a (s :: S).
  PIsData a =>
  Term s a ->
  Term s a ->
  Term s (PInterval a)
interval a b = closedInterval start end
  where
    start :: Term s (PExtended a)
    start = pcon $ PFinite $ pdcons @"_0" # (pdata a) # pdnil

    end :: Term s (PExtended a)
    end = pcon $ PFinite $ pdcons @"_0" # (pdata b) # pdnil

{- | create an interval that includes all values that are greater than or equal
 - to a
-}
from :: forall a s. PIsData a => Term s a -> Term s (PInterval a)
from a = closedInterval start end
  where
    start :: Term s (PExtended a)
    start = pcon $ PFinite $ pdcons @"_0" # (pdata a) # pdnil

    end :: Term s (PExtended a)
    end = pcon $ PPosInf pdnil

{- | create an interval that includes all values that are smaller than or equal
 - to a
-}
to :: forall a (s :: S). PIsData a => Term s a -> Term s (PInterval a)
to a = closedInterval start end
  where
    start :: Term s (PExtended a)
    start = pcon $ PNegInf pdnil

    end :: Term s (PExtended a)
    end = pcon $ PFinite $ pdcons @"_0" # (pdata a) # pdnil

-- | create an interval that covers every slot
always :: forall a (s :: S). PIsData a => Term s (PInterval a)
always = closedInterval start end
  where
    start :: Term s (PExtended a)
    start = pcon $ PNegInf pdnil

    end :: Term s (PExtended a)
    end = pcon $ PPosInf pdnil

-- | create an interval that is empty
never :: forall a (s :: S). PIsData a => Term s (PInterval a)
never = closedInterval start end
  where
    start :: Term s (PExtended a)
    start = pcon $ PPosInf pdnil

    end :: Term s (PExtended a)
    end = pcon $ PNegInf pdnil

-- | create and interval [a, a]
singleton :: forall a (s :: S). PIsData a => Term s a -> Term s (PInterval a)
singleton a = closedInterval start start
  where
    start :: Term s (PExtended a)
    start = pcon $ PFinite $ pdcons @"_0" # (pdata a) # pdnil

-- | `hull i1 i2` is the smallest interval containing `i1` and `i2`
hull ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s (PInterval a) ->
  Term s (PInterval a) ->
  Term s (PInterval a)
hull x' y' = P.do
  x <- pletFields @'["from", "to"] x'
  y <- pletFields @'["from", "to"] y'

  lowerX <- plet x.from
  upperX <- plet x.to

  lowerY <- plet y.from
  upperY <- plet y.to

  let lower = pcon $ PLowerBound $ minP (lToE lowerX) (lToE lowerY)
      upper = pcon $ PUpperBound $ maxP (uToE upperX) (uToE upperY)

  interval' lower upper

-- | `intersecion i1 i2` is the largest interval contained in `i1` and `i2`
intersection ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s (PInterval a) ->
  Term s (PInterval a) ->
  Term s (PInterval a)
intersection x' y' = P.do
  x <- pletFields @'["from", "to"] x'
  y <- pletFields @'["from", "to"] y'

  lowerX <- plet x.from
  upperX <- plet x.to

  lowerY <- plet y.from
  upperY <- plet y.to

  let lower = pcon $ PLowerBound $ maxP (lToE lowerX) (lToE lowerY)
      upper = pcon $ PUpperBound $ minP (uToE upperX) (uToE upperY)

  interval' lower upper

-- | `a` contains `b` is true if the interval `b` is entirely contained in `a`
contains ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s (PInterval a) ->
  Term s (PInterval a) ->
  Term s PBool
contains x' y' = P.do
  x <- pletFields @'["from", "to"] x'
  y <- pletFields @'["from", "to"] y'

  lowerX <- plet x.from
  upperX <- plet x.to

  lowerY <- plet y.from
  upperY <- plet y.to

  (leqP (lToE lowerX) (lToE lowerY)) #&& (leqP (uToE upperY) (uToE upperX))

-- | `a` before interval `i` is true if `a` is earlier than the start of `i`
before ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s a ->
  Term s (PInterval a) ->
  Term s PBool
before a y = P.do
  lower <- plet $ pfield @"from" # y
  before' a (lToE lower)

-- | `a` after interval `i` is true if `a` is later than the end of `i`
after ::
  forall a s.
  (PEq a, POrd a, PIsData a) =>
  Term s a ->
  Term s (PInterval a) ->
  Term s PBool
after a y = P.do
  upper <- plet $ pfield @"to" # y
  after' a (uToE upper)

-- | interval from upper and lower
interval' ::
  forall a (s :: S).
  PIsData a =>
  Term s (PLowerBound a) ->
  Term s (PUpperBound a) ->
  Term s (PInterval a)
interval' lower upper =
  pcon $
    PInterval $
      pdcons @"from" # pdata lower
        #$ pdcons @"to" # pdata upper # pdnil

-- | closed interval from PExtended
closedInterval ::
  forall a (s :: S).
  PIsData a =>
  Term s (PExtended a) ->
  Term s (PExtended a) ->
  Term s (PInterval a)
closedInterval start end = interval' lower upper
  where
    closure :: Term s PClosure
    closure = pconstant True

    upper :: Term s (PUpperBound a)
    upper =
      pcon $
        PUpperBound $
          pdcons @"_0" # pdata end #$ pdcons @"_1"
            # pdata closure
            # pdnil

    lower :: Term s (PLowerBound a)
    lower =
      pcon $
        PLowerBound $
          pdcons @"_0" # pdata start #$ pdcons @"_1"
            # pdata closure
            # pdnil

-- | value < endpoint
before' ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term s a ->
  Term s (EndPoint a) ->
  Term s PBool
before' a y' = P.do
  y <- pletFields @'["_0", "_1"] y'
  yt <- plet $ y._0
  yc <- plet $ y._1

  pif
    yc
    (pmatch yt (ltE' a))
    (pmatch yt (leqE' a))

-- | value > endpoint
after' ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term s a ->
  Term s (EndPoint a) ->
  Term s PBool
after' a y' = P.do
  y <- pletFields @'["_0", "_1"] y'
  yt <- plet $ y._0
  yc <- plet $ y._1

  pif
    yc
    (pmatch yt (gtE' a))
    (pmatch yt (geqE' a))

-- | (x :: Term s (EndPoint a)) <= (y :: Term s (EndPoint a))
leqP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term s (EndPoint a) ->
  Term s (EndPoint a) ->
  Term s PBool
leqP x' y' = P.do
  x <- pletFields @'["_0", "_1"] x'
  y <- pletFields @'["_0", "_1"] y'

  xt <- plet $ x._0
  yt <- plet $ y._0

  xc <- plet $ x._1
  yc <- plet $ y._1

  pif
    (xc #&& yc #|| (pnot # xc) #&& (pnot # yc))
    (leqE xt yt)
    (ltE xt yt)

minP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term s (EndPoint a) ->
  Term s (EndPoint a) ->
  Term s (EndPoint a)
minP x y = pif (leqP x y) x y

maxP ::
  forall a (s :: S).
  (PIsData a, POrd a, PEq a) =>
  Term s (EndPoint a) ->
  Term s (EndPoint a) ->
  Term s (EndPoint a)
maxP x y = pif (leqP x y) y x

-- | (x :: Term s (PExtended a)) < (y :: Term s (PExtended b))
ltE ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term s (PExtended a) ->
  Term s (PExtended a) ->
  Term s PBool
ltE x y = pmatch x (cont y)
  where
    cont :: Term s (PExtended a) -> PExtended a s -> Term s PBool
    cont y' x' = case x' of
      PNegInf _ -> pconstant True
      PPosInf _ -> pmatch y' isPosInf
      PFinite l -> P.do
        a <- plet $ pfield @"_0" # l
        pmatch y' (ltE' a)

-- | (x :: Term s (PExtended a)) = (y :: Term s (PExtended b))
eqE ::
  forall a (s :: S).
  (PEq a, PIsData a) =>
  Term s (PExtended a) ->
  Term s (PExtended a) ->
  Term s PBool
eqE x y = pmatch x cont
  where
    cont :: PExtended a s -> Term s PBool
    cont x' = case x' of
      PNegInf _ -> pmatch y isNegInf
      PPosInf _ -> pmatch y isPosInf
      PFinite l -> P.do
        a <- plet $ pfield @"_0" # l
        pmatch y (eqE' a)

-- | value < PExtended
ltE' ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
ltE' a y' = case y' of
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> P.do
    b <- plet $ pfield @"_0" # r
    a #< b

-- | value > PExtended
gtE' ::
  forall a (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
gtE' a y' = case y' of
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> P.do
    b <- plet $ pfield @"_0" # r
    b #< a

-- | value = PExtended
eqE' ::
  forall a (s :: S).
  (PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
eqE' a y' = case y' of
  PFinite r -> P.do
    b <- plet $ pfield @"_0" # r
    a #== b
  _ -> pconstant False

-- | value <= PExtended
leqE' ::
  forall a (s :: S).
  (POrd a, PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
leqE' a y = (ltE' a y) #|| (eqE' a y)

-- | value >= PExtended
geqE' ::
  forall a (s :: S).
  (POrd a, PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
geqE' a y = (gtE' a y) #|| (eqE' a y)

-- | (x :: Term s (PExtended a)) <= (y :: Term s (PExtended b))
leqE ::
  forall a (s :: S).
  (PEq a, POrd a, PIsData a) =>
  Term s (PExtended a) ->
  Term s (PExtended a) ->
  Term s PBool
leqE x y = (ltE x y) #|| (eqE x y)

isNegInf :: PExtended a s -> Term s PBool
isNegInf x = case x of
  PNegInf _ -> pconstant True
  _ -> pconstant False

isPosInf :: PExtended a s -> Term s PBool
isPosInf x = case x of
  PPosInf _ -> pconstant True
  _ -> pconstant False

type EndPoint a =
  PDataRecord
    '[ "_0" ':= PExtended a
     , "_1" ':= PClosure
     ]

uToE :: Term s (PUpperBound a) -> Term s (EndPoint a)
uToE x = pmatch x (\(PUpperBound a) -> a)

lToE :: Term s (PLowerBound a) -> Term s (EndPoint a)
lToE x = pmatch x (\(PLowerBound a) -> a)
