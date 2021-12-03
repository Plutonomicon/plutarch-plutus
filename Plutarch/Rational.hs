{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (PTRational) where
  
import Plutarch.Prelude

type PTRational :: PType
type PTRational = PTNew "rational-gqCBcr9I" (PTPair PTInteger PTInteger)

pRational :: TermInterp exp => exp PTInteger -> exp PTInteger -> exp PTRational
pRational x y = new $ pPair x y

instance TermInterp exp => Num (exp PTRational) where
  (+) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger n = pRational (fromInteger n) 1
  x * y =
    pinl (pfst (unnew x)) $ \x1 ->
    pinl (psnd (unnew x)) $ \x2 ->
    pinl (pfst (unnew y)) $ \y1 ->
    pinl (psnd (unnew y)) $ \y2 ->
    pRational (x1 * y1) (x2 * y2)
