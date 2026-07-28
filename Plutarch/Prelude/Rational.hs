{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Prelude.Rational (
  -- * Type
  PRational (PRational),

  -- * Functions
) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, plam', punsafeCoerce)
import Plutarch.Numeric.Additive (
  PAdditiveGroup (pminus, pnegate),
  PAdditiveMonoid (pzero),
  PAdditiveSemigroup (padd, pscalePositive),
  (#+),
  (#-),
 )
import Plutarch.Numeric.Euclidean (pdiv, pgcd)
import Plutarch.Numeric.Multiplicative (
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup (pmultiply, ppowPositive),
  (#*),
 )
import Plutarch.Primitive.Apply (pgeneralize, (#))
import Plutarch.Primitive.Bool (pand)
import Plutarch.Primitive.Con (pcon)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Numeric (PInteger, PPositive)
import Plutarch.TH.Strategy (Strategy (MogensenScott), deriveFor)

-- | @since wip
data PRational (s :: S) = PRational (Term s PInteger) (Term s PPositive)

deriveFor ''PRational MogensenScott

-- | @since wip
instance PAdditiveSemigroup PRational where
  padd = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational numX denX) ->
    pmatch y $ \(PRational numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #+ (pscalePositive # numY # denX)
          -- Given that PPositive cannot contain zero, this is safe
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- Similar reasons to the above
          finalDen = punsafeCoerce (pdiv # pgeneralize newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRational finalNum $ finalDen
  pscalePositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRational num den) ->
    let newNum = pscalePositive # num # p
        -- Given that PPositive cannot contain zero, this is safe
        reduction = pgcd # newNum # punsafeCoerce den
        -- Similar reasons to the above
        finalDen = punsafeCoerce (pdiv # pgeneralize den # reduction)
        finalNum = pdiv # newNum # reduction
     in pcon . PRational finalNum $ finalDen

-- | @since wip
instance PAdditiveMonoid PRational where
  pzero = pcon . PRational pzero $ pone

-- | @since wip
instance PAdditiveGroup PRational where
  pnegate = plam' $ \r -> pmatch r $ \(PRational num den) ->
    pcon . PRational (pnegate # num) $ den
  pminus = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational numX denX) ->
    pmatch y $ \(PRational numY denY) ->
      let newDen = denX #* denY
          newNum = (pscalePositive # numX # denY) #- (pscalePositive # numY # denX)
          -- Given that PPositive cannot contain zero, this is safe
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- Similar reasons to the above
          finalDen = punsafeCoerce (pdiv # pgeneralize newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRational finalNum $ finalDen

-- | @since wip
instance PMultiplicativeSemigroup PRational where
  pmultiply = plam' $ \x -> plam' $ \y -> pmatch x $ \(PRational numX denX) ->
    pmatch y $ \(PRational numY denY) ->
      let newDen = denX #* denY
          newNum = numX #* numY
          -- Given that PPositive cannot contain zero, this is safe
          reduction = pgcd # newNum # punsafeCoerce newDen
          -- Similar reasons to the above
          finalDen = punsafeCoerce (pdiv # pgeneralize newDen # reduction)
          finalNum = pdiv # newNum # reduction
       in pcon . PRational finalNum $ finalDen
  ppowPositive = plam' $ \r -> plam' $ \p -> pmatch r $ \(PRational num den) ->
    pcon . PRational (ppowPositive # num # p) $ ppowPositive # den # p

-- | @since wip
instance PMultiplicativeMonoid PRational where
  pone = pcon . PRational pone $ pone
