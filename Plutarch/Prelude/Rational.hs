{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Prelude.Rational (
  -- * Type
  PRatio (PRatio),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, plam', punsafeCoerce)
import Plutarch.Numeric.Additive (
  PAdditiveGroup (pminus, pnegate),
  PAdditiveMonoid (pzero),
  PAdditiveSemigroup (padd, pscalePositive),
  (#+),
  (#-),
 )
import Plutarch.Numeric.Euclidean (PEuclidean (pdiv, pgcd, pmod))
import Plutarch.Numeric.Field (
  PDistributive (pfromPositive),
  PField (precip),
  PRing (pfromInteger),
  PSemiring (pfromNatural),
 )
import Plutarch.Numeric.Multiplicative (
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup (pmultiply, ppowPositive),
  (#*),
 )
import Plutarch.Numeric.Zeroable (
  PAbs (pabs),
  PNZInteger,
  PZeroable (PNonZero, ptoNonZero),
 )
import Plutarch.Primitive.Apply (pcoerce, pgeneralize, punsafeSpecialize, (#))
import Plutarch.Primitive.Bool (pand, pif)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Numeric (PInteger, PNatural, PPositive)
import Plutarch.Primitive.Ord (POrd (pleq, plt), pmax, (#<), (#<=))
import Plutarch.TH.Strategy (Strategy (MogensenScott), deriveFor)

-- | @since wip
data PRatio (a :: S -> Type) (s :: S) = PRatio (Term s a) (Term s PPositive)

deriveFor ''PRatio MogensenScott

-- | @since wip
instance (PAdditiveSemigroup a, POrd a) => POrd (PRatio a) where
  plt = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      (pscalePositive # numX # denY) #< (pscalePositive # numY # denX)
  pleq = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      (pscalePositive # numX # denY) #<= (pscalePositive # numY # denX)

-- | @since wip
instance PAdditiveSemigroup (PRatio PPositive) where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #+ (pscalePositive # numY # denX)
          reduction = pgcd # pcoerce newNum # newDen
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          --
          -- Furthermore, we know that the answer must be positive, because if
          -- it were 0, it would mean that `newDen` was 0, which in turn would
          -- imply `denX = 0` or `denY = 0`. Since all of these are not
          -- possible, we can safely specialize.
          finalDen = punsafeSpecialize (pdiv # pcoerce newDen # reduction)
          -- We can use similar reasoning to the above to conclude that this
          -- specialization is also safe.
          finalNum = punsafeSpecialize (pdiv # pcoerce newNum # reduction)
       in pcon . PRatio finalNum $ finalDen
  pscalePositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    let newNum = pscalePositive # num # p
        reduction = pgcd # pcoerce newNum # den
        -- We know that `PPositive` cannot contain zero, and so coercing it to
        -- `PNZInteger` is safe here. We _should_ be able to prove this, but
        -- unfortunately, the hierarchy demonstrated by `PRepresentable`
        -- cannot support this.
        --
        -- Furthermore, we know that the answer must be positive, as otherwise,
        -- it would mean `den = 0`, which is impossible. Thus, we can safely
        -- specialize.
        finalDen = punsafeSpecialize (pdiv # pcoerce den # reduction)
        -- We can use similar reasoning to the above to conclude that this
        -- specialization is also safe.
        finalNum = punsafeSpecialize (pdiv # pcoerce newNum # reduction)
     in pcon . PRatio finalNum $ finalDen

-- | @since wip
instance PAdditiveSemigroup (PRatio PNatural) where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #+ (pscalePositive # numY # denX)
          reduction = pgcd # newNum # newDen
          -- We know that `newDen` is not zero, and that `newDen` is divisible
          -- by `reduction`, so the result can't be zero either.
          finalDen = punsafeCoerce (pdiv # pcoerce newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen
  pscalePositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    let newNum = pscalePositive # num # p
        reduction = pgcd # newNum # den
        -- We know that `den` is not zero, and `den` is divisible by
        -- `reduction`, so the result can't be zero either.
        finalDen = punsafeCoerce (pdiv # pcoerce den # reduction)
        finalNum = pdiv # newNum # reduction
     in pcon . PRatio finalNum $ finalDen

-- | @since wip
instance PAdditiveSemigroup (PRatio PInteger) where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #+ (pscalePositive # numY # denX)
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- We know that `newDen` is strictly positive, and that `newDen` is
          -- divisible by `reduction`, so the result must be strictly positive
          -- too.
          finalDen = punsafeCoerce (pdiv # pgeneralize newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen
  pscalePositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    let newNum = pscalePositive # num # p
        -- We know that `PPositive` cannot contain zero, and so coercing it to
        -- `PNZInteger` is safe here. We _should_ be able to prove this, but
        -- unfortunately, the hierarchy demonstrated by `PRepresentable`
        -- cannot support this.
        reduction = pgcd # newNum # punsafeCoerce den
        -- We know that `den` is strictly positive, and that `den` is
        -- divisible by `reduction`, so the result must be strictly positive
        -- too.
        finalDen = punsafeCoerce (pdiv # pgeneralize den # reduction)
        finalNum = pdiv # newNum # reduction
     in pcon . PRatio finalNum $ finalDen

-- | @since wip
instance
  (PAdditiveSemigroup (PRatio a), PAdditiveMonoid a) =>
  PAdditiveMonoid (PRatio a)
  where
  pzero = pcon . PRatio pzero $ pone

-- | @since wip
instance PAdditiveGroup (PRatio PInteger) where
  pnegate = plam' $ \r -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (pnegate # num) $ den
  pminus = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #- (pscalePositive # numY # denX)
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- We know that `newDen` is strictly positive, and that `newDen` is
          -- divisible by `reduction`, so the result must be strictly positive
          -- too.
          finalDen = punsafeCoerce (pdiv # pgeneralize newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen

-- | @since wip
instance PMultiplicativeSemigroup (PRatio PPositive) where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = numX #* numY
          reduction = pgcd # pcoerce newNum # newDen
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          --
          -- Furthermore, we know that the answer must be positive, because if
          -- it were 0, it would mean that `newDen` was 0, which in turn would
          -- imply `denX = 0` or `denY = 0`. Since all of these are not
          -- possible, we can safely specialize.
          finalDen = punsafeSpecialize (pdiv # pcoerce newDen # reduction)
          -- We can use similar reasoning to the above to conclude that this
          -- specialization is also safe.
          finalNum = punsafeSpecialize (pdiv # pcoerce newNum # reduction)
       in pcon . PRatio finalNum $ finalDen
  ppowPositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (ppowPositive # num # p) $ ppowPositive # den # p

-- | @since wip
instance PMultiplicativeSemigroup (PRatio PNatural) where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = numX #* numY
          reduction = pgcd # newNum # newDen
          -- We know that `newDen` is not zero, and that `newDen` is divisible
          -- by `reduction`, so the result can't be zero either.
          finalDen = punsafeCoerce (pdiv # pcoerce newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen
  ppowPositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (ppowPositive # num # p) $ ppowPositive # den # p

-- | @since wip
instance PMultiplicativeSemigroup (PRatio PNZInteger) where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = numX #* numY
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          reduction = pgcd # pcoerce newNum # punsafeCoerce newDen
          -- We know that `newDen` is strictly positive, and that `newDen` is
          -- divisible by `reduction`, so the result must be strictly positive
          -- too.
          finalDen = punsafeSpecialize . punsafeSpecialize $ pdiv # pgeneralize newDen # reduction
          -- Since we know `newNum` is not 0, `reduction` is not 0, and that
          -- `newNum` is divisible by `reduction`, the result won't be `0`
          -- either. Thus, this specialization is safe.
          finalNum = punsafeSpecialize (pdiv # pcoerce newNum # reduction)
       in pcon . PRatio finalNum $ finalDen
  ppowPositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (ppowPositive # num # p) $ ppowPositive # den # p

-- | @since wip
instance PMultiplicativeSemigroup (PRatio PInteger) where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newDen = denX #* denY
          newNum = numX #* numY
          -- We know that `PPositive` cannot contain zero, and so coercing it to
          -- `PNZInteger` is safe here. We _should_ be able to prove this, but
          -- unfortunately, the hierarchy demonstrated by `PRepresentable`
          -- cannot support this.
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- We know that `newDen` is strictly positive, and that `newDen` is
          -- divisible by `reduction`, so the result must be strictly positive
          -- too.
          finalDen = punsafeSpecialize . punsafeSpecialize $ pdiv # pgeneralize newDen # reduction
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen
  ppowPositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (ppowPositive # num # p) $ ppowPositive # den # p

-- | @since wip
instance
  (PMultiplicativeSemigroup (PRatio a), PMultiplicativeMonoid a) =>
  PMultiplicativeMonoid (PRatio a)
  where
  pone = pcon . PRatio pone $ pone

-- | @since wip
instance (PMultiplicativeSemigroup (PRatio a), PAbs a) => PAbs (PRatio a) where
  pabs = plam' $ \r -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio (pabs # num) $ den

-- | @since wip
instance (PZeroable a, PAdditiveSemigroup (PRatio a)) => PZeroable (PRatio a) where
  type PNonZero (PRatio a) = PRatio (PNonZero a)
  ptoNonZero r whenZero whenNot = pmatch r $ \(PRatio num den) ->
    ptoNonZero num whenZero (plam' $ \nzNum -> whenNot # pcon (PRatio nzNum den))

-- | @since wip
instance PEuclidean (PRatio PNatural) where
  pdiv = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      let newNum = pscalePositive # numX # denY
          newDen = denX #* numY
          reduction = pgcd # newNum # newDen
          -- We know that `newDen` is not zero, and that `newDen` is divisible
          -- by `reduction`, so the result can't be zero either.
          finalDen = punsafeCoerce (pdiv # pcoerce newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRatio finalNum $ finalDen
  pmod = plam' $ \_ -> plam' $ const pzero

  -- A nonzero `PRatio PNatural` will always be bigger, and since we're
  -- guaranteed a nonzero argument, we can specialize safely.
  pgcd = plam' $ \x -> plam' $ \y -> punsafeSpecialize (pmax x (pcoerce y))

-- | @since wip
instance PEuclidean (PRatio PInteger) where
  pdiv = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRatio numX denX) ->
    pmatch y $ \(PRatio numY denY) ->
      -- Since we know that denominators must be positive, we only have to check
      -- that the numerators have the same, or a different, sign.
      pif
        (pzero #<= numX)
        ( pif
            (pzero #<= pcoerce numY)
            -- Non-negative X, positive Y
            (pcoerce (pdiv # punsafeSpecialize @(PRatio PNatural) x # punsafeCoerce y))
            -- Non-negative X, negative Y
            ( let x' = punsafeSpecialize @(PRatio PNatural) x
                  y' = pcon . PRatio (punsafeCoerce (pabs # numY)) $ denY
               in pnegate # pcoerce (pdiv # x' # y')
            ) --
        )
        ( pif
            (pzero #<= pcoerce numY)
            -- Negative X, positive Y
            ( let x' = pcon . PRatio (punsafeSpecialize @PNatural $ pabs # numX) $ denX
               in pnegate # pcoerce (pdiv # x' # punsafeCoerce y)
            )
            -- Negative X, negative Y
            ( let x' = pcon . PRatio (punsafeSpecialize @PNatural $ pabs # numX) $ denX
                  y' = pcon . PRatio (punsafeCoerce $ pabs # numY) $ denY
               in pcoerce (pdiv # x' # y')
            )
        )
  pmod = plam' $ \_ -> plam' $ const pzero

  -- The absolute value of a nonzero `PRatio PInteger` will always be bigger
  -- than a zero `PRatio PInteger`, and since we're guaranteed a nonzero
  -- argument, we can specialize safely.
  pgcd = plam' $ \x -> plam' $ \y -> punsafeSpecialize (pmax (pabs # x) (pabs # pcoerce y))

-- | @since wip
instance
  (PDistributive a, PAdditiveSemigroup (PRatio a), PMultiplicativeSemigroup (PRatio a)) =>
  PDistributive (PRatio a)
  where
  pfromPositive = plam' $ \p -> pcon . PRatio (pfromPositive # p) $ pone

-- | @since wip
instance
  (PSemiring a, PAdditiveSemigroup (PRatio a), PMultiplicativeSemigroup (PRatio a)) =>
  PSemiring (PRatio a)
  where
  pfromNatural = plam' $ \n -> pcon . PRatio (pfromNatural # n) $ pone

-- | @since wip
instance
  (PRing a, PAdditiveGroup (PRatio a), PMultiplicativeSemigroup (PRatio a)) =>
  PRing (PRatio a)
  where
  pfromInteger = plam' $ \i -> pcon . PRatio (pfromInteger # i) $ pone

-- | @since wip
instance PField (PRatio PNatural) where
  precip = plam' $ \r -> pmatch r $ \(PRatio num den) ->
    pcon . PRatio den $ num

-- | @since wip
instance PField (PRatio PInteger) where
  precip = plam' $ \r -> pmatch r $ \(PRatio num den) ->
    pif
      (pzero #<= pcoerce num)
      -- Since the numerator is non-negative, and cannot be zero, we can
      -- coerce directly.
      (pcon . PRatio (punsafeCoerce den) . punsafeCoerce $ num)
      -- A negative numerator means we have to negate both numerator and
      -- denominator first.
      ( let newNum = punsafeSpecialize $ pnegate # (pcoerce . pcoerce $ den)
            newDen = punsafeSpecialize . punsafeSpecialize $ pnegate # pcoerce num
         in pcon . PRatio newNum $ newDen
      )
