{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Array (
  -- * Type
  PPullArray,

  -- * Functions
  pfromArray,
  piota,
  pmapArray,
  pimapArray,
  pzipWithArray,
  pizipWithArray,
  ppullArrayToList,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
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
import Plutarch.Internal.PlutusType (PlutusType, pcon, pmatch)
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
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))

data PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (Term s PNatural) (Term s (PInteger :--> a))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving
    ( PlutusType
    )
    via (DeriveAsSOPStruct (PPullArray a))

pfromArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a) ->
  Term s (PPullArray a)
pfromArray arr =
  pcon . PPullArray (punsafeCoerce $ plengthOfArray # arr) $ pindexArray # arr

pmapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmapArray f arr = pmatch arr $ \(PPullArray len g) ->
  pcon . PPullArray len $ pcompose g f

ppullArrayToList ::
  forall (a :: S -> Type) (s :: S).
  PlutusType (PBuiltinList a) =>
  Term s (PPullArray a) ->
  Term s (PBuiltinList a)
ppullArrayToList arr = pmatch arr $ \(PPullArray len f) ->
  phoistAcyclic (pfix # plam go) # f # (pupcast len - 1) # pcon PNil
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PInteger :--> PBuiltinList a :--> PBuiltinList a) ->
      Term s' (PInteger :--> a) ->
      Term s' PInteger ->
      Term s' (PBuiltinList a) ->
      Term s' (PBuiltinList a)
    go self f currIx acc =
      pif
        (currIx #== (-1))
        acc
        (self # f # (currIx - 1) #$ pconsBuiltin # (f # currIx) # acc)

pimapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pimapArray f arr = pmatch arr $ \(PPullArray len g) ->
  pcon . PPullArray len . plam $ \ix -> f # ix #$ g # ix

pzipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pzipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray len1 g1) ->
  pmatch arr2 $ \(PPullArray len2 g2) ->
    pcon . PPullArray (pmin len1 len2) . plam $ \ix -> f # (g1 # ix) #$ g2 # ix

pizipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pizipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray len1 g1) ->
  pmatch arr2 $ \(PPullArray len2 g2) ->
    pcon . PPullArray (pmin len1 len2) . plam $ \ix -> f # ix # (g1 # ix) #$ g2 # ix

piota ::
  forall (s :: S).
  Term s PNatural ->
  Term s (PPullArray PInteger)
piota len = pcon . PPullArray len $ pid

-- Helpers

pcompose ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (b :--> c) ->
  Term s (a :--> c)
pcompose f g = plam $ \x -> g #$ f # x

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
