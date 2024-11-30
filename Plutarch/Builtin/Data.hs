{-
BuiltinPair and BuiltinList should go into their own module !!
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Data (
  PData (PData),
  PAsData (PAsData),
  pchooseData,
  pasConstr,
  pasMap,
  plistData,
  pasList,
  pasInt,
  pasByteStr,
  pserialiseData,
  pconstrBuiltin,
  PBuiltinPair (PBuiltinPair),
  pfstBuiltin,
  psndBuiltin,
  ppairDataBuiltin,
  PBuiltinList (PCons, PNil),
  pheadBuiltin,
  ptailBuiltin,
  pchooseListBuiltin,
  pnullBuiltin,
  pconsBuiltin,
) where

import Data.Kind (Type)

import Plutarch.Builtin.Bool
import Plutarch.Builtin.ByteString
import Plutarch.Builtin.Integer

import Plutarch.Internal.Term
import PlutusCore qualified as PLC

newtype PData (s :: S) = PData (Term s PData)
newtype PAsData (a :: S -> Type) (s :: S) = PAsData (Term s a)

pchooseData :: Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
pchooseData = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ChooseData

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

pasMap :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData))
pasMap = punsafeBuiltin PLC.UnMapData

plistData :: Term s (PBuiltinList PData :--> PData)
plistData = punsafeBuiltin PLC.ListData

pasList :: Term s (PData :--> PBuiltinList PData)
pasList = punsafeBuiltin PLC.UnListData

pasInt :: Term s (PData :--> PInteger)
pasInt = punsafeBuiltin PLC.UnIData

pasByteStr :: Term s (PData :--> PByteString)
pasByteStr = punsafeBuiltin PLC.UnBData

-- | Serialise any builtin data to its cbor represented by a builtin bytestring
pserialiseData :: Term s (PData :--> PByteString)
pserialiseData = punsafeBuiltin PLC.SerialiseData

pconstrBuiltin :: Term s (PInteger :--> PBuiltinList PData :--> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
pconstrBuiltin = punsafeBuiltin PLC.ConstrData

--

newtype PBuiltinPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBuiltinPair (Term s (PBuiltinPair a b))

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

{- | Construct a builtin pair of 'PData' elements.

Uses 'PAsData' to preserve more information about the underlying 'PData'.
-}
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
ppairDataBuiltin = punsafeBuiltin PLC.MkPairData

-- --------------------------------------------------------------------------------

-- | Plutus 'BuiltinList'
data PBuiltinList (a :: S -> Type) (s :: S)
  = PCons (Term s a) (Term s (PBuiltinList a))
  | PNil

pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

ptailBuiltin :: Term s (PBuiltinList a :--> PBuiltinList a)
ptailBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

pchooseListBuiltin :: Term s (PBuiltinList a :--> b :--> b :--> b)
pchooseListBuiltin = phoistAcyclic $ pforce $ pforce $ punsafeBuiltin PLC.ChooseList

pnullBuiltin :: Term s (PBuiltinList a :--> PBool)
pnullBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.NullList

pconsBuiltin :: Term s (a :--> PBuiltinList a :--> PBuiltinList a)
pconsBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.MkCons
