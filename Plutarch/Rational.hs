{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (
  PRational,
  preduce,
  pnumerator,
  pdenominator,
  pfromInteger,
) where

import Plutarch.Prelude

import Data.Ratio (denominator, numerator)
import Plutarch (PlutusType (..))
import Plutarch.Bool (PEq (..), POrd (..), pif)
import Plutarch.Integer (PInteger, PIntegral (pdiv, pmod))

data PRational s = PRational (Term s PInteger) (Term s PInteger)

instance PlutusType PRational where
  type PInner PRational c = (PInteger :--> PInteger :--> c) :--> c
  pcon' (PRational x y) = plam $ \f -> f # x # y
  pmatch' p f = p #$ plam $ \x y -> f (PRational x y)

instance PEq PRational where
  l #== r =
    pmatch l $ \(PRational ln ld) ->
      pmatch r $ \(PRational rn rd) ->
        rd * ln #== rn * ld

instance POrd PRational where
  l #<= r =
    pmatch l $ \(PRational ln ld) ->
      pmatch r $ \(PRational rn rd) ->
        rd * ln #<= rn * ld
  l #< r =
    pmatch l $ \(PRational ln ld) ->
      pmatch r $ \(PRational rn rd) ->
        rd * ln #< rn * ld

instance Num (Term s PRational) where
  x + y = preduce
    #$ pmatch x
    $ \(PRational xn xd) ->
      pmatch y $ \(PRational yn yd) ->
        pcon $ PRational (xn * yd + yn * xd) (xd * yd)
  x - y = preduce
    #$ pmatch x
    $ \(PRational xn xd) ->
      pmatch y $ \(PRational yn yd) ->
        pcon $ PRational (xn * yd - yn * xd) (xd * yd)
  x * y = preduce
    #$ pmatch x
    $ \(PRational xn xd) ->
      pmatch y $ \(PRational yn yd) ->
        pcon $ PRational (xn * yn) (xd * yd)
  negate x =
    pmatch x $ \(PRational xn xd) ->
      pcon $ PRational (negate xn) xd
  abs x =
    pmatch x $ \(PRational xn xd) ->
      pcon $ PRational (abs xn) (abs xd)
  signum x' = plet x' $ \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #< 0)
        (-1)
        1
  fromInteger n = pcon $ PRational (fromInteger n) 1

instance Fractional (Term s PRational) where
  recip x =
    pmatch x $ \(PRational xn xd) ->
      pcon (PRational xd xn)
  x / y = preduce
    #$ pmatch x
    $ \(PRational xn xd) ->
      pmatch y $ \(PRational yn yd) ->
        pcon (PRational (xn * yd) (xd * yn))
  fromRational r =
    pcon $ PRational (fromInteger $ numerator r) (fromInteger $ denominator r)

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational xn xd) ->
      plet (pgcd # xn # xd) $ \r ->
        plet (signum xd) $ \s ->
          pcon $ PRational (s * pdiv # xn # r) (s * pdiv # xd # r)

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' ->
    plet (abs x') $ \x ->
      plet (abs y') $ \y ->
        plet (pmax # x # y) $ \a ->
          plet (pmin # x # y) $ \b ->
            pgcd' # a # b

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = pfix #$ plam $ f
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = plam $ \a b -> pif (a #<= b) a b

pmax :: POrd a => Term s (a :--> a :--> a)
pmax = plam $ \a b -> pif (a #<= b) b a

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PInteger)
pdenominator = plam $ \x -> pmatch x $ \(PRational _ d) -> d

pfromInteger :: Term s (PInteger :--> PRational)
pfromInteger = plam $ \n -> pcon $ PRational n 1
