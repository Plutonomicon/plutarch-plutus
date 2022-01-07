{-# LANGUAGE UndecidableInstances #-}

-- This should have been called Plutarch.Data...
module Plutarch.Builtin (
  PData (..),
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  pasMap,
  pasList,
  pasInt,
  pasByteStr,
  PBuiltinPair,
  PBuiltinList (..),
  pdataLiteral,
  PIsData (..),
  PAsData,
  pforgetData,
  ppairDataBuiltin,
  InDefaultUni,
) where

import Plutarch (PlutusType (..), punsafeBuiltin, punsafeCoerce, punsafeFrom)
import Plutarch.Bool (PBool (..), PEq, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift
import Plutarch.List (PListLike (..), plistEquals)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import PlutusTx (Data)

-- | Constraint 'PHaskellType' of 'a' to be a Plutus Core builtin type (i.e 'PLC.DefaultUni').
type InDefaultUni a = PLC.Contains PLC.DefaultUni (PHaskellType a)

-- | Plutus 'BuiltinPair'
data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

deriving via
  PBuiltinType (PBuiltinPair a b) (PHaskellType a, PHaskellType b)
  instance
    ( InDefaultUni a
    , InDefaultUni b
    ) =>
    (PLift (PBuiltinPair a b))

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

{- | Construct a builtin pair of 'PData' elements.

Uses 'PAsData' to preserve more information about the underlying 'PData'.
-}
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
ppairDataBuiltin = punsafeBuiltin PLC.MkPairData

-- | Plutus 'BuiltinList'
data PBuiltinList (a :: k -> Type) (s :: k)
  = PCons (Term s a) (Term s (PBuiltinList a))
  | PNil

deriving via
  PBuiltinType (PBuiltinList a) [PHaskellType a]
  instance
    InDefaultUni a => (PLift (PBuiltinList a))

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

--------------------------------------------------------------------------------

instance InDefaultUni a => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) b = PBuiltinList a
  pcon' :: forall s. PBuiltinList a s -> forall b. Term s (PInner (PBuiltinList a) b)
  pcon' (PCons x xs) = pconsBuiltin # x # pto xs
  pcon' PNil = pconstant @(PBuiltinList a) []
  pmatch' xs f =
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (punsafeFrom $ ptailBuiltin # xs)))

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = InDefaultUni a
  pelimList match_cons match_nil =
    plam $ \ls -> pmatch ls $ \case
      PCons x xs -> match_cons # x # xs
      PNil -> match_nil
  pcons = plam $ \x xs -> pcon (PCons x xs)
  pnil = pcon PNil
  phead = pheadBuiltin
  ptail = ptailBuiltin
  pnull = pnullBuiltin

instance (PElemConstraint PBuiltinList a, PEq a) => PEq (PBuiltinList a) where
  (#==) xs ys = plistEquals # xs # ys

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)
  deriving (PLift) via PBuiltinType PData Data

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

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

{-# DEPRECATED pdataLiteral "Use `pconstant` instead." #-}
pdataLiteral :: Data -> Term s PData
pdataLiteral = pconstant

data PAsData (a :: k -> Type) (s :: k)

pforgetData :: Term s (PAsData a) -> Term s PData
pforgetData = punsafeCoerce

class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)

instance PIsData PData where
  pfromData = punsafeCoerce
  pdata = punsafeCoerce

instance PIsData a => PIsData (PBuiltinList (PAsData a)) where
  pfromData x = punsafeCoerce $ pasList # pforgetData x
  pdata x = punsafeBuiltin PLC.ListData # x

instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x

instance PIsData PByteString where
  pfromData x = pasByteStr # pforgetData x
  pdata x = punsafeBuiltin PLC.BData # x

instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromData x = pasConstr # pforgetData x
  pdata x' = plet x' $ \x -> punsafeBuiltin PLC.ConstrData # (pfstBuiltin # x) #$ psndBuiltin # x

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y
