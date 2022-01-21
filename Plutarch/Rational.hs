module Plutarch.Rational (
  PRational,
  preduce,
  pnumerator,
  pdenominator,
  pratFromInt,
  pround,
  ptruncate,
  pproperFraction,
) where

import Plutarch.Prelude

import Plutarch (PlutusType (..), punsafeCoerce)
import Plutarch.Bool (PEq (..), POrd (..), pif)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData (..),
  pasInt,
  pasList,
  pforgetData,
 )
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.List (PListLike (pcons, phead, pnil, ptail), pmap)
import Plutarch.Natural ()
import Plutarch.Numeric (
  PAdditiveGroup ((#-)),
  PAdditiveMonoid (pzero),
  PAdditiveSemigroup ((#+)),
  PIntegralDomain (pabs, psignum),
  PMultiplicativeGroup (preciprocal),
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup ((#*)),
  pdiv,
  prem,
 )
import Plutarch.Pair (PPair (..))

data PRational s = PRational (Term s PInteger) (Term s PInteger)

instance PIsData PRational where
  pfromData x' =
    phoistAcyclic
      ( plam $ \x ->
          pListToRat #$ pmap # pasInt #$ pasList # pforgetData x
      )
      # x'
  pdata x' =
    phoistAcyclic
      ( plam $ \x ->
          ( punsafeCoerce ::
              Term _ (PAsData (PBuiltinList (PAsData PInteger))) ->
              Term _ (PAsData PRational)
          )
            $ pdata $ pRatToList # x
      )
      # x'

instance PlutusType PRational where
  type PInner PRational c = (PInteger :--> PInteger :--> c) :--> c
  pcon' (PRational x y) = plam $ \f -> f # x # y
  pmatch' p f = p #$ plam $ \x y -> f (PRational x y)

instance PEq PRational where
  l' #== r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd #* ln #== rn #* ld
      )
      # l'
      # r'

instance POrd PRational where
  l' #<= r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd #* ln #<= rn #* ld
      )
      # l'
      # r'

  l' #< r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd #* ln #< rn #* ld
      )
      # l'
      # r'

instance PAdditiveSemigroup PRational where
  x' #+ y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce #$ pmatch x $
            \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn #* yd #+ yn #* xd) (xd #* yd)
      )
      # x'
      # y'

instance PAdditiveMonoid PRational where
  pzero = phoistAcyclic (pcon $ PRational pzero pone)

instance PAdditiveGroup PRational where
  x' #- y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn #* yd #- yn #* xd) (xd #* yd)
      )
      # x'
      # y'

instance PMultiplicativeSemigroup PRational where
  x' #* y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn #* yn) (xd #* yd)
      )
      # x'
      # y'

instance PMultiplicativeMonoid PRational where
  pone = phoistAcyclic (pcon $ PRational pone pone)

instance PMultiplicativeGroup PRational where
  preciprocal x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            pcon (PRational xd xn)
      )
      # x'

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational xn xd) ->
      plet (pgcd # xn # xd) $ \r ->
        plet (psignum xd) $ \s ->
          pcon $ PRational (s #* pdiv xn r) (s #* pdiv xd r)

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' ->
    plet (pabs x') $ \x ->
      plet (pabs y') $ \y ->
        plet (pmax # x # y) $ \a ->
          plet (pmin # x # y) $ \b ->
            pgcd' # a # b

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = phoistAcyclic $ pfix #$ plam $ f
  where
    f self a b =
      pif
        (b #== pzero)
        a
        $ self # b #$ prem a b

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

pmax :: POrd a => Term s (a :--> a :--> a)
pmax = phoistAcyclic $ plam $ \a b -> pif (a #<= b) b a

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PInteger)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pratFromInt :: Term s (PInteger :--> PRational)
pratFromInt = phoistAcyclic $ plam $ \n -> pcon $ PRational n pone

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      plet (pdiv a b) $ \base ->
        plet (prem a b) $ \rem ->
          base
            #+ pif
              (prem b (pconstant 2) #== pone)
              (pif (pdiv b (pconstant 2) #< rem) pone pzero)
              ( pif
                  (pdiv b (pconstant 2) #== rem)
                  (prem base (pconstant 2))
                  (pif (rem #< pdiv b (pconstant 2)) pzero pone)
              )

-- (pdiv # b # 2 + pmod # b # 2 #<= pmod # a # b) 1 0

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      plet (pdiv a b) $ \q ->
        pif
          (pzero #<= a)
          q
          (q #+ pif (prem a b #== pzero) pzero pone)

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction = phoistAcyclic $
  plam $ \x ->
    plet (ptruncate # x) $ \q ->
      pcon $ PPair q (x #- pratFromInt # q)

pRatToList :: Term s (PRational :--> PBuiltinList (PAsData PInteger))
pRatToList =
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      pcons # pdata a
        #$ pcons # pdata b
        #$ punsafeCoerce (pnil :: Term s (PBuiltinList PData))

pListToRat :: Term s (PBuiltinList PInteger :--> PRational)
pListToRat = plam $ \x -> pcon $ PRational (phead # x) (phead #$ ptail # x)
