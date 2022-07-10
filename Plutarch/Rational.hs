{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (
  PRational (..),
  preduce,
  pnumerator,
  pdenominator,
  Plutarch.Rational.pfromInteger,
  pround,
  ptruncate,
  pproperFraction,
  PFractional (..),
) where

import Data.Ratio (denominator, numerator)
import GHC.Generics (Generic)
import Plutarch (
  DPTStrat,
  DerivePlutusType,
  PlutusType,
  PlutusTypeScott,
  Term,
  pcon,
  pfix,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PEq (..), POrd (..), pif)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData,
  pasInt,
  pasList,
  pdata,
  pdataImpl,
  pforgetData,
  pfromDataImpl,
 )
import Plutarch.Integer (PInteger, PIntegral (pdiv, pmod))
import Plutarch.Internal (PType)
import Plutarch.Lift (pconstant)
import Plutarch.List (PListLike (pcons, phead, pnil, ptail), pmap)
import Plutarch.Num (PNum, pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-))
import Plutarch.Pair (PPair (..))
import Plutarch.Show (PShow (pshow'), pshow)
import Plutarch.Trace (ptraceError)
import Plutarch.Unsafe (punsafeCoerce)

class PFractional (a :: PType) where
  (#/) :: Term s a -> Term s a -> Term s a
  precip :: Term s (a :--> a)
  pfromRational :: Term s (PRational :--> a)

data PRational s
  = PRational (Term s PInteger) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance DerivePlutusType PRational where type DPTStrat _ = PlutusTypeScott

instance (PNum a, PFractional a) => Fractional (Term s a) where
  (/) = (#/)
  recip x = precip # x
  fromRational x = pfromRational #$ pcon $ PRational (pconstant (numerator x)) (pconstant (denominator x))

instance PShow PRational where
  pshow' _ x =
    pshowRat # x
    where
      pshowRat = phoistAcyclic $
        plam $ \n -> pmatch n $ \(PRational x y) ->
          pshow x <> "/" <> pshow y

instance PIsData PRational where
  pfromDataImpl x' = phoistAcyclic (plam $ \x -> pListToRat #$ pmap # pasInt #$ pasList # pforgetData x) # x'
  pdataImpl x' =
    phoistAcyclic
      ( plam $ \x ->
          punsafeCoerce (pdata $ pRatToList # x :: Term _ (PAsData (PBuiltinList (PAsData PInteger))))
      )
      # x'

pRatToList :: Term s (PRational :--> PBuiltinList (PAsData PInteger))
pRatToList = plam $ \x -> pmatch x $ \(PRational a b) ->
  pcons # pdata a #$ pcons # pdata b #$ punsafeCoerce (pnil :: Term s (PBuiltinList PData))

pListToRat :: Term s (PBuiltinList PInteger :--> PRational)
pListToRat = plam $ \x -> pcon $ PRational (phead # x) (phead #$ ptail # x)

instance PEq PRational where
  l' #== r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #== rn * ld
      )
      # l'
      # r'

instance POrd PRational where
  l' #<= r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #<= rn * ld
      )
      # l'
      # r'

  l' #< r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #< rn * ld
      )
      # l'
      # r'

instance PNum PRational where
  x' #+ y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce #$ pmatch x $
            \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yd + yn * xd) (xd * yd)
      )
      # x'
      # y'

  x' #- y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yd - yn * xd) (xd * yd)
      )
      # x'
      # y'

  x' #* y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yn) (xd * yd)
      )
      # x'
      # y'

  pnegate =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (negate xn) xd

  pabs =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (abs xn) (abs xd)

  psignum =
    phoistAcyclic $
      plam $ \x' -> plet x' $ \x ->
        pif
          (x #== 0)
          0
          $ pif
            (x #< 0)
            (-1)
            1

  pfromInteger n = pcon $ PRational (fromInteger n) 1

instance PFractional PRational where
  precip =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pfailOnZero # xn # pcon (PRational xd xn)

  x' #/ y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                plet (xd * yn) $ \den ->
                  pfailOnZero # den
                    #$ pcon (PRational (xn * yd) den)
      )
      # x'
      # y'

  pfromRational = phoistAcyclic $ plam id

pfailOnZero :: Term s (PInteger :--> a :--> a)
pfailOnZero = phoistAcyclic $
  plam $ \n x ->
    pif
      (n #== 0)
      (ptraceError "pfailOnZero: division by 0")
      x

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
pgcd' = phoistAcyclic $ pfix #$ plam $ f
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

pmax :: POrd a => Term s (a :--> a :--> a)
pmax = phoistAcyclic $ plam $ \a b -> pif (a #<= b) b a

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PInteger)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pfromInteger :: Term s (PInteger :--> PRational)
pfromInteger = phoistAcyclic $ plam $ \n -> pcon $ PRational n 1

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      plet (pdiv # a # b) $ \base ->
        plet (pmod # a # b) $ \rem ->
          base
            + pif
              (pmod # b # 2 #== 1)
              (pif (pdiv # b # 2 #< rem) 1 0)
              ( pif
                  (pdiv # b # 2 #== rem)
                  (pmod # base # 2)
                  (pif (rem #< pdiv # b # 2) 0 1)
              )

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      plet (pdiv # a # b) $ \q ->
        pif
          (0 #<= a)
          q
          (q + pif (pmod # a # b #== 0) 0 1)

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction = phoistAcyclic $
  plam $ \x ->
    plet (ptruncate # x) $ \q ->
      pcon $ PPair q (x - Plutarch.Rational.pfromInteger # q)
