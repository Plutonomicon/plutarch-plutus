{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  type PBuiltinMap,
) where

import Data.Coerce (Coercible, coerce)
import Plutarch (PlutusType (..), punsafeBuiltin, punsafeCoerce)
import Plutarch.Bool (PBool (..), PEq, pif', (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (
  DerivePConstantViaCoercible (DerivePConstantViaCoercible),
  PConstant,
  PConstantRepr,
  PConstanted,
  PLift,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.List (PListLike (..), plistEquals)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import PlutusTx (Data)

-- | Plutus 'BuiltinPair'
data PBuiltinPair (a :: PType) (b :: PType) (s :: S)

instance (PLift a, PLift b) => PUnsafeLiftDecl (PBuiltinPair a b) where
  type PLifted (PBuiltinPair a b) = (PLifted a, PLifted b)

-- FIXME: figure out good way of deriving this
instance (PConstant a, PConstant b) => PConstant (a, b) where
  type PConstantRepr (a, b) = (PConstantRepr a, PConstantRepr b)
  type PConstanted (a, b) = PBuiltinPair (PConstanted a) (PConstanted b)
  pconstantToRepr (x, y) = (pconstantToRepr x, pconstantToRepr y)
  pconstantFromRepr (x, y) = do
    x' <- pconstantFromRepr @a x
    y' <- pconstantFromRepr @b y
    Just (x', y')

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
data PBuiltinList (a :: PType) (s :: S)
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

instance PConstant a => PConstant [a] where
  type PConstantRepr [a] = [PConstantRepr a]
  type PConstanted [a] = PBuiltinList (PConstanted a)
  pconstantToRepr x = pconstantToRepr <$> x
  pconstantFromRepr x = traverse (pconstantFromRepr @a) x

instance PUnsafeLiftDecl a => PUnsafeLiftDecl (PBuiltinList a) where
  type PLifted (PBuiltinList a) = [PLifted a]

instance PLift a => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) _ = PBuiltinList a
  pcon' (PCons x xs) = pconsBuiltin # x # xs
  pcon' PNil = pconstant []
  pmatch' xs f =
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = PLift a

  pelimList match_cons match_nil ls = pmatch ls $ \case
    PCons x xs -> match_cons x xs
    PNil -> match_nil
  pcons = plam $ \x xs -> pcon (PCons x xs)
  pnil = pcon PNil
  phead = pheadBuiltin
  ptail = ptailBuiltin
  pnull = pnullBuiltin

instance (PLift a, PEq a) => PEq (PBuiltinList a) where
  (#==) xs ys = plistEquals # xs # ys

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)

instance PUnsafeLiftDecl PData where type PLifted PData = Data
deriving via (DerivePConstantViaCoercible Data PData Data) instance (PConstant Data)

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

{- |
  Map type used for Plutus `Data`'s Map constructor.

  Note that the Plutus API doesn't use this most of the time,
  instead encoding as a List of Tuple constructors.

  Not to be confused with `PlutusTx.AssocMap.Map` / `PMap`
-}
type PBuiltinMap a b = (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))

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

type role PAsData representational phantom
data PAsData (a :: PType) (s :: S)

data PAsDataLifted (a :: PType)

instance PConstant (PAsDataLifted a) where
  type PConstantRepr (PAsDataLifted a) = Data
  type PConstanted (PAsDataLifted a) = PAsData a
  pconstantToRepr = \case {}
  pconstantFromRepr _ = Nothing

instance PUnsafeLiftDecl (PAsData a) where type PLifted (PAsData a) = PAsDataLifted a

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

instance PIsData (PBuiltinMap k v) where
  pfromData x = punsafeCoerce $ pasMap # pforgetData x
  pdata x = punsafeBuiltin PLC.MapData # x

instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x

instance PIsData PByteString where
  pfromData x = pasByteStr # pforgetData x
  pdata x = punsafeBuiltin PLC.BData # x

{- |
  Instance for PBool following the Plutus IsData repr
  given by @makeIsDataIndexed ''Bool [('False,0),('True,1)]@,
  which is used in 'TxInfo' via 'Closure'.
-}
instance PIsData PBool where
  pfromData x =
    (phoistAcyclic $ plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pfstBuiltin # (pasConstr # d) #== 1

  pdata x =
    (phoistAcyclic $ plam toData) # x
    where
      toData :: Term s PBool -> Term s (PAsData PBool)
      toData b =
        punsafeBuiltin PLC.ConstrData
          # (pif' # b # 1 # (0 :: Term s PInteger))
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromData x = pasConstr # pforgetData x
  pdata x' = plet x' $ \x -> punsafeBuiltin PLC.ConstrData # (pfstBuiltin # x) #$ psndBuiltin # x

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

instance (forall (s :: S). Coercible (a s) (Term s b), PIsData b) => PIsData (DerivePNewtype a b) where
  pfromData x = pcon . DerivePNewtype $ ptypeOuter target
    where
      target :: Term _ b
      target = pfromData $ pinnerData x
  pdata x = pouterData . pdata $ pto x

pinnerData :: Term s (PAsData a) -> Term s (PAsData (PInner a b))
pinnerData = punsafeCoerce

pouterData :: Term s (PAsData (PInner a b)) -> Term s (PAsData a)
pouterData = punsafeCoerce

ptypeOuter :: forall (x :: PType) y s. Coercible (x s) (Term s y) => Term s y -> x s
ptypeOuter = coerce
