{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Plutarch.Array (
  -- * Type
  PPullArray,

  -- * Functions
  pfromArray,
  piota,
  pmapArray,
  pimapArray,
  pzipWithArray,
  ppullArrayToList,
  pgenerate,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Array (
  PArray,
  pindexArray,
  plengthOfArray,
 )
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.Data (PBuiltinList (PNil), pconsBuiltin)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Numeric (PNatural)
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PlutusType (PInner, pcon', pmatch'), pcon, pmatch)
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.Subtype (pupcast)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )

newtype PArrOf (a :: S -> Type) (r :: S -> Type) (s :: S)
  = PArrOf ((:-->) (PNatural :--> (PInteger :--> r) :--> r) r s)

newtype PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (PForall (PArrOf a) s)

instance PlutusType (PPullArray a) where
  type PInner (PPullArray a) = PForall (PArrOf a)
  pcon' (PPullArray t) = pcon t
  pmatch' t f = pmatch t $ \t' -> f (PPullArray t')

pfromArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a) ->
  Term s (PPullArray a)
pfromArray arr = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # punsafeCoerce (plengthOfArray # arr) # (pindexArray # arr)

pmapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmapArray f arr = pmatch arr $ \(PPullArray (PForall g)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce g # plam (\len h -> k # len # pcompose h f)

ppullArrayToList ::
  forall (a :: S -> Type) (s :: S).
  PlutusType (PBuiltinList a) =>
  Term s (PPullArray a) ->
  Term s (PBuiltinList a)
ppullArrayToList arr = pmatch arr $ \(PPullArray (PForall f)) ->
  punsafeCoerce f # go
  where
    go :: Term s (PNatural :--> (PInteger :--> a) :--> PBuiltinList a)
    go = phoistAcyclic $ plam $ \len f ->
      phoistAcyclic (pfix # plam goInner) # f # (pupcast len - 1) # pcon PNil
    goInner ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PInteger :--> PBuiltinList a :--> PBuiltinList a) ->
      Term s' (PInteger :--> a) ->
      Term s' PInteger ->
      Term s' (PBuiltinList a) ->
      Term s' (PBuiltinList a)
    goInner self f currIx acc =
      pif
        (currIx #== (-1))
        acc
        (self # f # (currIx - 1) #$ pconsBuiltin # (f # currIx) # acc)

pimapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pimapArray f arr = pmatch arr $ \(PPullArray (PForall g)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce g # plam (\len h -> k # len # picompose f h)

pzipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pzipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray (PForall g1)) ->
  pmatch arr2 $ \(PPullArray (PForall g2)) ->
    pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
      punsafeCoerce g1
        # plam
          ( \len1 g1 ->
              punsafeCoerce g2
                # plam
                  ( \len2 g2 ->
                      k # pmin @PNatural len1 len2 # pliftIndex f g1 g2
                  )
          )

piota ::
  forall (s :: S).
  Term s PNatural ->
  Term s (PPullArray PInteger)
piota len = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # len # pid

pgenerate ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PInteger :--> a) ->
  Term s (PPullArray a)
pgenerate len f = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # len # f

-- Helpers

pcompose ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (b :--> c) ->
  Term s (a :--> c)
pcompose f g = plam $ \x -> g #$ f # x

picompose ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PInteger :--> a) ->
  Term s (PInteger :--> b)
picompose f g = plam $ \ix -> f # ix #$ g # ix

pliftIndex ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PInteger :--> a) ->
  Term s (PInteger :--> b) ->
  Term s (PInteger :--> c)
pliftIndex f g1 g2 = plam $ \i -> f # (g1 # i) # (g2 # i)

pmin ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a ->
  Term s a ->
  Term s a
pmin x y = pif (x #<= y) x y

pid ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> a)
pid = phoistAcyclic $ plam id
