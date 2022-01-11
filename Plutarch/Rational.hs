{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational 
  (PRational
  ,preduce
  ,pnumerator
  ,pdenominator
  ) where

import Plutarch.Prelude

import Plutarch.Integer ( PIntegral(pmod, pdiv), PInteger )
import Plutarch ( PlutusType(..) ) 
import Plutarch.Bool ( POrd(..), PEq(..), pif )
import Data.Ratio ( denominator, numerator )

data PRational s = PRational (Term s PInteger) (Term s PInteger)

instance PlutusType PRational where
  type PInner PRational c = (PInteger :--> PInteger :--> c) :--> c
  pcon' (PRational x y) = plam $ \f -> f # x # y
  pmatch' p f = p #$ plam $ \x y -> f (PRational x y)

instance PEq PRational where
  l #== r = 
    pmatch l (\(PRational ln ld) ->
    pmatch r (\(PRational rn rd) ->
      rd * ln #== rn * ld
      ))

instance POrd PRational where
  l #<= r = 
    pmatch l (\(PRational ln ld) ->
    pmatch r (\(PRational rn rd) ->
      rd * ln #<= rn * ld
      ))
  l #< r = 
    pmatch l (\(PRational ln ld) ->
    pmatch r (\(PRational rn rd) ->
      rd * ln #< rn * ld
      ))

instance Num (Term s PRational) where
  x + y = preduce #
    pmatch x (\(PRational xn xd) ->
    pmatch y (\(PRational yn yd) ->
      pcon $ PRational (xn*yd+yn*xd) (xd*yd)
      ))
  x - y = preduce #
    pmatch x (\(PRational xn xd) ->
    pmatch y (\(PRational yn yd) ->
      pcon $ PRational (xn*yd-yn*xd) (xd*yd)
      ))
  x * y = preduce #
    pmatch x (\(PRational xn xd) ->
    pmatch y (\(PRational yn yd) ->
      pcon (PRational (xn*yn) (xd*yd))
             ))
  negate x = 
    pmatch x (\(PRational xn xd) ->
      pcon $ PRational (negate xn) xd
        )
  abs x = 
    pmatch x (\(PRational xn xd) ->
      pcon $ PRational (abs xn) (abs xd)
      )
  signum x' = plet x' $ \x ->
    pif (x #== 0)
      0
      $ pif (x #<= 0)
        (-1)
        1
  fromInteger n = pcon $ PRational (fromInteger n) 1
            
instance Fractional (Term s PRational) where
  recip x = 
    pmatch x (\(PRational xn xd) ->
      pcon (PRational xd xn))
  x / y = preduce #
    pmatch x (\(PRational xn xd) ->
    pmatch y (\(PRational yn yd) ->
      pcon (PRational (xn*yd) (xd*yn))
             ))
  fromRational r = 
    pcon $ PRational (fromInteger $ numerator r) (fromInteger $ denominator r)

preduce :: Term s (PRational :--> PRational)
preduce = plam $ \x ->
  pmatch x (\(PRational xn xd) ->
    plet (pgcd # xn # xd) $ \r ->
    plet (signum xd) $ \s -> 
      pcon (PRational (s*pdiv # xn # r) (s*pdiv # xd # r)
           ))

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = plam $ \x y -> pgcd' # abs x # abs y

-- assumes non-negative inputs
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = pfix #$ plam $ f
  where
    f self a' b' =
        plet (pmax # a' # b') $ \a ->
        plet (pmin # a' # b') $ \b ->
          pif (b #== 0)
            a
            (self # (pmod # a # b) # b)

pmin,pmax :: POrd a => Term s (a :--> a :--> a)
pmin = plam $ \a b -> pif (a #<= b) a b
pmax = plam $ \a b -> pif (a #<= b) b a

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = plam $ \x -> pmatch x (\(PRational n _) -> n)

pdenominator :: Term s (PRational :--> PInteger)
pdenominator = plam $ \x -> pmatch x (\(PRational _ d) -> d)

