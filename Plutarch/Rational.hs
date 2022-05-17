module Plutarch.Rational (
  PRational,
  preduce,
  pnumerator,
  pdenominator,
  pfromInteger,
  pround,
  ptruncate,
  pproperFraction,
  prationalLazy,
  prational,
  punrational,
) where

import Data.Ratio (denominator, numerator)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PlutusType (..),
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
import Plutarch.Pair (PPair (PPair))
import Plutarch.Reducible
import Plutarch.Show (PShow, pshow, pshow')
import Plutarch.TermCont (tcont, unTermCont)
import Plutarch.Trace
import Plutarch.TryFrom
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

data PRational s
  = PRational (Term s PInteger) (Term s PNonZero)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PEq)

{- | NOTE: This instance may delegate the nonzero invariant check during construction.

As such, the constructor is not exported and users are expected to use 'prational', 'prationalLazy', and
'punrational' instead.
-}
instance PlutusType PRational where
  type PInner PRational b = (PInteger :--> PNonZero :--> b) :--> b
  pcon' (PRational x y) = plam $ \f -> f # x # y
  pmatch' ratF f = ratF #$ plam $ \a b -> f $ PRational a b

{- | Builds a rational with lazy non-zero denominator verification.

i.e, if the denominator was zero, it'll error only after 'pmatch'ing on the resulting rational.
-}
prationalLazy :: Term s PInteger -> Term s PNonZero -> Term s PRational
prationalLazy numr denm = pcon $ PRational numr denm

-- | Build a rational, without delegating the denominator invariant.
prational :: Term s (PInteger :--> PNonZero :--> PRational)
prational = phoistAcyclic $ plam $ \numr denm -> pcon $ PRational numr denm

-- | 'pmatch' specialized for 'PRational'.
punrational :: Term s PRational -> ((Term s PInteger, Term s PNonZero) -> Term s b) -> Term s b
punrational r f = pmatch r $ \(PRational x y) -> f (x, y)

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
      plistToRat = plam $ \x -> prationalLazy (pasInt #$ phead # x) $ punsafeDowncast $ pasInt #$ phead #$ ptail # x
  pdataImpl x' =
    phoistAcyclic
      ( plam $ \x -> unTermCont $ do
          PRational a b <- tcont $ pmatch x
          let res :: Term _ (PBuiltinList (PAsData PInteger))
              res = pcons # pdata a #$ pcons # pdata (pto b) #$ pnil
          pure $ punsafeCoerce $ pdata res
      )
      # x'

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

instance Num (Term s PRational) where
  x' + y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $
            preduce
              #$ prationalLazy (xn * pto yd + yn * pto xd)
              $ punsafeDowncast $ pto xd * pto yd
      )
      # x'
      # y'

  x' - y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure $
            preduce
              #$ prationalLazy (xn * pto yd - yn * pto xd)
              $ punsafeDowncast $ pto xd * pto yd
      )
      # x'
      # y'

  x' * y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          pure $
            preduce
              #$ prationalLazy (xn * yn)
              $ punsafeDowncast $ pto xd * pto yd
      )
      # x'
      # y'

  negate x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            prationalLazy (negate xn) xd
      )
      # x'

  abs x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            prationalLazy (abs xn) $ punsafeDowncast $ abs $ pto xd
      )
      # x'

  signum x' =
    phoistAcyclic
      ( plam $ \x ->
          pif
            (x #== 0)
            0
            $ pif
              (x #< 0)
              (-1)
              1
      )
      # x'

  fromInteger n = prationalLazy (pconstant n) $ punsafeDowncast 1

instance Fractional (Term s PRational) where
  recip x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) -> plet (ptryNonZero # xn) $ \denm ->
            prationalLazy (pto xd) denm
      )
      # x'

  x' / y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          denm <- tcont . plet $ ptryNonZero #$ pto xd * yn
          pure $ preduce #$ prationalLazy (xn * pto yd) denm
      )
      # x'
      # y'

  fromRational r =
    prationalLazy (pconstant $ numerator r) $ punsafeDowncast $ pconstant $ denominator r

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational xn xd' <- tcont $ pmatch x
    xd <- tcont . plet $ pto xd'
    r <- tcont . plet $ pgcd # xn # xd
    s <- tcont . plet . signum $ xd
    pure . prationalLazy (s * pdiv # xn # r) $ punsafeDowncast $ s * pdiv # xd # r

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
pfromInteger = phoistAcyclic $ plam $ \n -> prationalLazy n $ punsafeDowncast 1

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
      pcon $ PPair q (x - pfromInteger # q)
