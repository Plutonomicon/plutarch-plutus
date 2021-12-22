module Plutarch.Builtin (
  PData (..),
  pheadBuiltin,
  ptailBuiltin,
  pnullBuiltin,
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  pasMap,
  pasList,
  pasInt,
  pasByteStr,
  PBuiltinPair,
  PBuiltinList,
) where

import Plutarch (punsafeBuiltin)
import Plutarch.Bool (PBool)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

data PBuiltinList (a :: k -> Type) (s :: k)

pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

ptailBuiltin :: Term s (PBuiltinList a :--> PBuiltinList a)
ptailBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

pnullBuiltin :: Term s (PBuiltinList a :--> PBool)
pnullBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.NullList

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

pasMap :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData))
pasMap = punsafeBuiltin PLC.UnMapData

pasList :: Term s (PData :--> PBuiltinList PData)
pasList = punsafeBuiltin PLC.UnListData

pasInt :: Term s (PData :--> PInteger)
pasInt = punsafeBuiltin PLC.UnIData

pasByteStr :: Term s (PData :--> PByteString)
pasByteStr = punsafeBuiltin PLC.UnBData
