{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Rational (
  PRational (PRational),
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
  PType,
  PlutusType,
  PlutusTypeScott,
  Term,
  pcon,
  pfix,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  pto,
  runTermCont,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PEq, POrd, pif, (#<), (#<=), (#==))
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
import Plutarch.Integer (PInteger, pdiv, pmod)
import Plutarch.Lift (pconstant)
import Plutarch.List (pcons, phead, plength, pnil, ptail)
import Plutarch.NonZero (PNonZero, ptryNonZero)
import Plutarch.Num (PNum, pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Show (PShow, pshow, pshow')
import Plutarch.TermCont (tcont, unTermCont)
import Plutarch.Trace (ptraceError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

class PFractional (a :: PType) where
  (#/) :: Term s a -> Term s a -> Term s a
  precip :: Term s (a :--> a)
  pfromRational :: Term s (PRational :--> a)

data PRational s
  = PRational (Term s PInteger) (Term s PNonZero)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PRational where type DPTStrat _ = PlutusTypeScott

instance (PNum a, PFractional a) => Fractional (Term s a) where
  (/) = (#/)
  recip x = precip # x
  fromRational x =
    pfromRational #$ pcon $
      PRational
        (pconstant $ numerator x)
        (punsafeDowncast . pconstant $ denominator x)

instance PShow PRational where
  pshow' _ x =
    pshowRat # x
    where
      pshowRat = phoistAcyclic $
        plam $ \n -> pmatch n $ \(PRational x y) ->
          pshow x <> "/" <> pshow (pto y)

instance PIsData PRational where
  pfromDataImpl x' = phoistAcyclic (plam $ \x -> plistToRat #$ pasList # pforgetData x) # x'
    where
      plistToRat :: Term s (PBuiltinList PData :--> PRational)
      plistToRat = plam $ \x ->
        pcon $
          PRational (pasInt #$ phead # x)
            . punsafeDowncast
            $ pasInt #$ phead #$ ptail # x
  pdataImpl x' =
    phoistAcyclic
      ( plam $ \x -> unTermCont $ do
          PRational a b <- tcont $ pmatch x
          let res :: Term _ (PBuiltinList (PAsData PInteger))
              res = pcons # pdata a #$ pcons # pdata (pto b) #$ pnil
          pure $ punsafeCoerce $ pdata res
      )
      # x'

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | NOTE: This instance produces a verified 'PNonZero' as the excess output.
instance PTryFrom PData (PAsData PRational) where
  type PTryFromExcess PData (PAsData PRational) = Flip Term PNonZero
  ptryFrom' opq = runTermCont $ do
    (_, ld) <- tcont $ ptryFrom @(PAsData (PBuiltinList PData)) opq
    tcont $ \f -> pif (plength # ld #== 2) (f ()) (ptraceError "ptryFrom(PRational): data list length should be 2")
    (_, denm) <- tcont $ ptryFrom @(PAsData PInteger) $ phead #$ ptail # ld
    res <- tcont . plet $ ptryNonZero # denm
    pure (punsafeCoerce opq, res)

instance POrd PRational where
  l' #<= r' =
    phoistAcyclic
      ( plam $ \l r -> unTermCont $ do
          PRational ln ld <- tcont $ pmatch l
          PRational rn rd <- tcont $ pmatch r
          pure $ pto rd * ln #<= rn * pto ld
      )
      # l'
      # r'

  l' #< r' =
    phoistAcyclic
      ( plam $ \l r -> unTermCont $ do
          PRational ln ld <- tcont $ pmatch l
          PRational rn rd <- tcont $ pmatch r
          pure $ pto rd * ln #< rn * pto ld
      )
      # l'
      # r'

instance PNum PRational where
  x' #+ y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $
            preduce
              #$ pcon $ PRational (xn * pto yd + yn * pto xd)
              $ punsafeDowncast $ pto xd * pto yd
      )
      # x'
      # y'

  x' #- y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $
            preduce
              #$ pcon $ PRational (xn * pto yd - yn * pto xd)
              $ punsafeDowncast $ pto xd * pto yd
      )
      # x'
      # y'

  x' #* y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          pure $
            preduce
              #$ pcon $ PRational (xn * yn)
              $ punsafeDowncast $ pto xd * pto yd
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
          pcon $ PRational (pto xd) $ ptryNonZero # xn

  x' #/ y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          denm <- tcont . plet $ ptryNonZero #$ pto xd * yn
          pure $ preduce #$ pcon $ PRational (xn * pto yd) denm
      )
      # x'
      # y'

  pfromRational = phoistAcyclic $ plam id

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational xn xd' <- tcont $ pmatch x
    xd <- tcont . plet $ pto xd'
    r <- tcont . plet $ pgcd # xn # xd
    s <- tcont . plet . signum $ xd
    pure . pcon $ PRational (s * pdiv # xn # r) $ punsafeDowncast $ s * pdiv # xd # r

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont . plet $ abs x'
    y <- tcont . plet $ abs y'
    pure $ pgcd' # (pmax # x # y) #$ pmin # x # y

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

pdenominator :: Term s (PRational :--> PNonZero)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pfromInteger :: Term s (PInteger :--> PRational)
pfromInteger = phoistAcyclic $ plam $ \n -> pcon $ PRational n $ punsafeDowncast 1

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational a' b' <- tcont $ pmatch x
    a <- tcont $ plet a'
    b <- tcont $ plet b'
    base <- tcont . plet $ pdiv # a # pto b
    rem <- tcont . plet $ pmod # a # pto b
    let result =
          pif
            (pmod # pto b # 2 #== 1)
            (pif (pdiv # pto b # 2 #< rem) 1 0)
            $ pif
              (pdiv # pto b # 2 #== rem)
              (pmod # base # 2)
              (pif (rem #< pdiv # pto b # 2) 0 1)
    pure $ base + result

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational a' b' <- tcont $ pmatch x
    a <- tcont $ plet a'
    b <- tcont $ plet b'
    q <- tcont . plet $ pdiv # a # pto b
    pure $
      pif
        (0 #<= a)
        q
        (q + pif (pmod # a # pto b #== 0) 0 1)

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction = phoistAcyclic $
  plam $ \x ->
    plet (ptruncate # x) $ \q ->
      pcon $ PPair q (x - Plutarch.Rational.pfromInteger # q)
