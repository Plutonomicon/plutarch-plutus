{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Array (
  -- * Type
  PPullArray,

  -- * Functions
  pfromArray,
  pmapArray,
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
import Plutarch.Internal.Lift (PLiftable)
import Plutarch.Internal.Numeric (PNatural)
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
  PLiftable (PBuiltinList a) =>
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

-- Helpers

pcompose ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (b :--> c) ->
  Term s (a :--> c)
pcompose f g = plam $ \x -> g #$ f # x
