{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (PRational) where
  
-- TODO: Make easier way of making newtypes?
data PRational

{-
prational :: Term (PPair PInteger PInteger) -> Term PRational
prational = punsafeCoerce

punRational :: Term PRational -> Term (PPair PInteger PInteger)
punRational = punsafeCoerce

instance Num (Term PTRational) where
  (+) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger n = prational (fromInteger n) 1
  x * y =
    pinl (pfst (punRational x)) $ \x1 ->
    pinl (psnd (punRational x)) $ \x2 ->
    pinl (pfst (punRational y)) $ \y1 ->
    pinl (psnd (punRational y)) $ \y2 ->
    prational (x1 * y1) (x2 * y2)
    -}
