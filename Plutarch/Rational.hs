{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (PRational) where
  
-- TODO: Make easier way of making newtypes?
data PRational

{-
pRational :: Term (PPair PInteger PInteger) -> Term PRational
pRational = pUnsafeCoerce

pUnRational :: Term PRational -> Term (PPair PInteger PInteger)
pUnRational = pUnsafeCoerce

instance Num (Term PTRational) where
  (+) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger n = pRational (fromInteger n) 1
  x * y =
    pInl (pFst (pUnRational x)) $ \x1 ->
    pInl (pSnd (pUnRational x)) $ \x2 ->
    pInl (pFst (pUnRational y)) $ \y1 ->
    pInl (pSnd (pUnRational y)) $ \y2 ->
    pRational (x1 * y1) (x2 * y2)
    -}
