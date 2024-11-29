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
) where

import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (pcond, pif)
import Plutarch.Builtin.Data (PAsData, PBuiltinList, PData, ppairDataBuiltin)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.IsData (PIsData, pdata)
import Plutarch.Internal.Lift (
  PLiftable (
    AsHaskell,
    PlutusRepr,
    fromPlutarch,
    fromPlutarchRepr,
    toPlutarch,
    toPlutarchRepr
  ),
  PLiftedClosed,
  fromPlutarchReprClosed,
  getPLifted,
  mkPLifted,
  pconstant,
  toPlutarchReprClosed,
 )
import Plutarch.Internal.Numeric (
  PNum (pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-)),
  pdiv,
  pmod,
  pquot,
 )
import Plutarch.Internal.Ord (
  POrd (pmax, pmin, (#<), (#<=)),
 )
import Plutarch.Internal.Other (pfix, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Show (PShow, pshow, pshow')
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TermCont (
  runTermCont,
  tcont,
  unTermCont,
 )
import Plutarch.Internal.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.List (phead, pnil, ptail)
import Plutarch.Pair (PPair (PPair))
import Plutarch.Positive (PPositive, ptryPositive)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusTx.Ratio qualified as PlutusTx

{- | A Scott-encoded rational number, with a guaranteed positive denominator
(and thus, a canonical form).

= Note

This is not the Plutarch equivalent of a Plutus @Rational@; for this, you
want @PRationalData@ from @plutarch-ledger-api@. 'PRational' is designed to
optimize for computation: if you want to do any serious work with rational
numbers that isn't just passing them around, you want to use (or convert to)
'PRational'.
-}
data PRational s
  = PRational (Term s PInteger) (Term s PPositive)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PRational where
  type DPTStrat _ = PlutusTypeScott

-- | @since WIP
instance PLiftable PRational where
  type AsHaskell PRational = PlutusTx.Rational
  type PlutusRepr PRational = PLiftedClosed PRational
  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = toPlutarchReprClosed
  {-# INLINEABLE toPlutarch #-}
  toPlutarch r =
    let n = PlutusTx.numerator r
        d = PlutusTx.denominator r
     in mkPLifted . pcon $
          if
            | n == 0 -> PRational 0 1
            | d < 0 -> PRational (pconstant . negate $ n) . punsafeCoerce . pconstant @PInteger . negate $ d
            | otherwise -> PRational (pconstant n) . punsafeCoerce . pconstant @PInteger $ d
  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = fromPlutarchReprClosed
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch t = do
    (n, d) <- fromPlutarch $ mkPLifted $ pmatch (getPLifted t) $ \(PRational n' d') ->
      ppairDataBuiltin # pdata n' # pdata (pto d')
    pure . PlutusTx.unsafeRatio n $ d

instance PEq PRational where
  l' #== r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              pto rd * ln #== rn * pto ld
      )
      # l'
      # r'

-- | @since WIP
instance Fractional (Term s PRational) where
  {-# INLINEABLE (/) #-}
  x / y = inner # x # y
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational :--> PRational)
      inner = phoistAcyclic $ plam $ \x y -> pmatch x $ \(PRational xn xd) ->
        pmatch y $ \(PRational yn yd) ->
          plet (pto xd * yn) $ \denm ->
            pif
              (denm #== 0)
              (ptraceInfoError "Cannot divide by zero")
              ( plet (xn * pto yd) $ \numm ->
                  preduce
                    #$ pif
                      (denm #< 0)
                      (pcon $ PRational (pnegate #$ numm) (punsafeCoerce $ pnegate # denm))
                      (pcon $ PRational numm (punsafeCoerce denm))
              )
  {-# INLINEABLE recip #-}
  recip x = inner # x
    where
      inner :: forall (s :: S). Term s (PRational :--> PRational)
      inner = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational xn xd) ->
        pcond
          [ (xn #== 0, ptraceInfoError "attempted to construct the reciprocal of zero")
          , (xn #< 0, pcon $ PRational (pnegate #$ pto xd) (punsafeCoerce $ pnegate # xn))
          ]
          (pcon $ PRational (pto xd) (punsafeCoerce xn))
  {-# INLINEABLE fromRational #-}
  fromRational = pconstant . PlutusTx.fromGHC

instance PShow PRational where
  pshow' _ x =
    pshowRat # x
    where
      pshowRat = phoistAcyclic $
        plam $ \n -> pmatch n $ \(PRational x y) ->
          pshow x <> "/" <> pshow (pto y)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | NOTE: This instance produces a verified 'PPositive' as the excess output.
instance PTryFrom PData (PAsData PRational) where
  type PTryFromExcess PData (PAsData PRational) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    (_, ld) <- tcont $ ptryFrom @(PAsData (PBuiltinList PData)) opq
    ratTail <- tcont . plet $ ptail # ld
    tcont $ \f -> pif (ptail # ratTail #== pnil) (f ()) $ ptraceInfoError "ptryFrom(PRational): data list length should be 2"
    (_, denm) <- tcont $ ptryFrom @(PAsData PInteger) $ phead # ratTail
    res <- tcont . plet $ ptryPositive # denm
    pure (punsafeCoerce opq, res)

instance POrd PRational where
  {-# INLINEABLE (#<=) #-}
  l' #<= r' =
    phoistAcyclic
      ( plam $ \l r -> unTermCont $ do
          PRational ln ld <- tcont $ pmatch l
          PRational rn rd <- tcont $ pmatch r
          pure $ pto rd * ln #<= rn * pto ld
      )
      # l'
      # r'
  {-# INLINEABLE (#<) #-}
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
  {-# INLINEABLE (#+) #-}
  x' #+ y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure
            $ preduce
              #$ pcon
            $ PRational (xn * pto yd + yn * pto xd)
            $ punsafeDowncast
            $ pto xd * pto yd
      )
      # x'
      # y'
  {-# INLINEABLE (#-) #-}
  x' #- y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd' <- tcont $ pmatch x
          PRational yn yd' <- tcont $ pmatch y
          xd <- tcont $ plet xd'
          yd <- tcont $ plet yd'
          pure
            $ preduce
              #$ pcon
            $ PRational (xn * pto yd - yn * pto xd)
            $ punsafeDowncast
            $ pto xd * pto yd
      )
      # x'
      # y'
  {-# INLINEABLE (#*) #-}
  x' #* y' =
    phoistAcyclic
      ( plam $ \x y -> unTermCont $ do
          PRational xn xd <- tcont $ pmatch x
          PRational yn yd <- tcont $ pmatch y
          pure
            $ preduce
              #$ pcon
            $ PRational (xn * yn)
            $ punsafeDowncast
            $ pto xd * pto yd
      )
      # x'
      # y'
  {-# INLINEABLE pnegate #-}
  pnegate =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (negate xn) xd
  {-# INLINEABLE pabs #-}
  pabs =
    phoistAcyclic $
      plam $ \x ->
        pmatch x $ \(PRational xn xd) ->
          pcon $ PRational (abs xn) xd
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \x ->
    pmatch x $ \(PRational n _) ->
      pcond
        [ (n #== 0, 0)
        , (n #< 0, -1)
        ]
        1
  {-# INLINEABLE pfromInteger #-}
  pfromInteger n = pcon $ PRational (fromInteger n) 1

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational xn xd' <- tcont $ pmatch x
    xd <- tcont . plet $ pto xd'
    r <- tcont . plet $ pgcd # xn # xd
    s <- tcont . plet $ psignum # xd
    pure . pcon $ PRational (s * pdiv # xn # r) $ punsafeDowncast $ s * pdiv # xd # r

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont . plet $ pabs # x'
    y <- tcont . plet $ pabs # y'
    pure $ pgcd' # pmax x y #$ pmin x y

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = phoistAcyclic $ pfix #$ plam f
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PPositive)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pfromInteger :: Term s (PInteger :--> PRational)
pfromInteger = phoistAcyclic $ plam $ \n -> pcon $ PRational n 1

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRational a' b' <- tcont $ pmatch x
    a <- tcont $ plet a'
    b <- tcont $ plet b'
    base <- tcont . plet $ pdiv # a # pto b
    rem <- tcont . plet $ pmod # a # pto b
    let result =
          pcond
            [ (pmod # pto b # 2 #== 1, pif (pdiv # pto b # 2 #< rem) 1 0)
            , (pdiv # pto b # 2 #== rem, pmod # base # 2)
            , (rem #< pdiv # pto b # 2, 0)
            ]
            1
    pure $ base + result

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      pquot # a # pto b

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction = phoistAcyclic $
  plam $ \x ->
    plet (ptruncate # x) $ \q ->
      pcon $ PPair q (x - Plutarch.Rational.pfromInteger # q)
