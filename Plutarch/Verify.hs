{-# OPTIONS_GHC -Wredundant-constraints #-}

module Plutarch.Verify (
  PTryFrom (ptryFrom),
  PTryFromRecur (ptryFromRecur),
) where

import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinMap,
  PBuiltinPair,
  PData,
  PIsData (pfromData),
  pdata,
  pfstBuiltin,
  ppairDataBuiltin,
  psndBuiltin,
 )
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal.Other (
  POpaque,
  PType,
  Term,
  phoistAcyclic,
  plam,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.List (pmap)

import Plutarch.Lift (PLift)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore as PLC

{- |
    Each POpaque can be of any representation as it represents
    the universe of Types. With PTryFrom we establish trust
    in the Data by verifying the requested type matches its
    representation.
-}
class PTryFrom (a :: PType) where
  ptryFrom :: Term s (PData :--> a)

instance PTryFrom PInteger where
  ptryFrom = punsafeBuiltin PLC.UnIData

instance PTryFrom PByteString where
  ptryFrom = punsafeBuiltin PLC.UnBData

{- |
    Note: PAsData POpaque ~ PData
-}
instance PTryFrom (PBuiltinList PData) where
  ptryFrom = punsafeBuiltin PLC.UnListData

instance PTryFrom (PBuiltinMap POpaque POpaque) where
  ptryFrom = punsafeBuiltin PLC.UnMapData

instance PTryFrom (PBuiltinPair (PAsData POpaque) (PAsData POpaque)) where
  ptryFrom = phoistAcyclic $
    plam $ \opq ->
      let tup :: Term _ (PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          tup = punsafeCoerce opq
          chk :: Term _ (PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          chk =
            ppairDataBuiltin
              # (pfstBuiltin # tup)
              # (psndBuiltin # tup)
       in chk

class PTryFromRecur (a :: PType) where
  ptryFromRecur :: Term s (PData :--> a)

instance PTryFromRecur PInteger where
  ptryFromRecur = punsafeBuiltin PLC.UnIData

instance PTryFromRecur PByteString where
  ptryFromRecur = punsafeBuiltin PLC.UnBData

instance (PTryFromRecur a, PLift a, PIsData a) => PTryFromRecur (PBuiltinList (PAsData a)) where
  ptryFromRecur = phoistAcyclic $
    plam $ \opq ->
      let lst :: Term _ (PBuiltinList (PAsData PData))
          lst = punsafeBuiltin PLC.UnListData #$ opq
       in pmap # (plam $ \e -> pdata $ ptryFromRecur @a #$ pfromData e) # lst

instance (PTryFromRecur a, PIsData a, PTryFromRecur b, PIsData b) => PTryFromRecur (PBuiltinPair (PAsData a) (PAsData b)) where
  ptryFromRecur = phoistAcyclic $
    plam $ \opq ->
      let tup :: Term _ (PBuiltinPair (PAsData PData) (PAsData PData))
          tup = punsafeCoerce opq
          fst :: Term _ (PAsData a)
          fst = pdata $ ptryFromRecur @a #$ pfromData $ pfstBuiltin # tup
          snd :: Term _ (PAsData b)
          snd = pdata $ ptryFromRecur @b #$ pfromData $ psndBuiltin # tup
       in ppairDataBuiltin # fst # snd
