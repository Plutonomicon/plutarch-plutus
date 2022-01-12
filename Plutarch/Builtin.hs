{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  type PBuiltinMap,
  type PList,
  InDefaultUni,
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Traversable (for)
import Plutarch (PlutusType (..), punsafeBuiltin, punsafeCoerce, punsafeFrom)
import Plutarch.Bool (PBool (..), PEq, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (AsDefaultUni (..), PBuiltinType, PLift (..), pconstant, type HasDefaultUni)
import Plutarch.List (PListLike (..), plistEquals)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import PlutusTx (Data)
import qualified PlutusTx (FromData, ToData, fromData, toData)

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

instance
  (HasDefaultUni a, HasDefaultUni b) =>
  AsDefaultUni (PBuiltinPair a b)
  where
  type DefaultUniType (PBuiltinPair a b) = (DefaultUniType a, DefaultUniType b)

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

instance (HasDefaultUni a) => AsDefaultUni (PBuiltinList a) where
  type DefaultUniType (PBuiltinList a) = [DefaultUniType a]

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

instance
  (HasDefaultUni a) =>
  PlutusType (PBuiltinList a)
  where
  type PInner (PBuiltinList a) b = PBuiltinList a
  pcon' :: forall s. PBuiltinList a s -> forall b. Term s (PInner (PBuiltinList a) b)
  pcon' (PCons x xs) = pconsBuiltin # x # pto xs
  pcon' PNil = pdefaultUniConstant []
  pmatch' xs f =
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (punsafeFrom $ ptailBuiltin # xs)))

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = (HasDefaultUni a)

  --PLC.Contains PLC.DefaultUni (DefaultUniType (PBuiltinList a))
  pelimList match_cons match_nil ls = pmatch ls $ \case
    PCons x xs -> match_cons x xs
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
  deriving (PLift, AsDefaultUni) via PBuiltinType PData Data

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

{- |
  Map type used for Plutus `Data`'s Map constructor.

  Note that the Plutus API doesn't use this most of the time,
  instead encoding as a List of Tuple constructors.

  Not to be confused with `PlutusTx.AssocMap.Map` / `PMap`
-}
type PBuiltinMap a b = (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))

-- | List type used for Plutus `Data`'s List constructor.
type PList a = PBuiltinList (PAsData a)

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

instance AsDefaultUni (PAsData a) where
  type DefaultUniType (PAsData a) = Data

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

instance
  ( PIsData k
  , PIsData v
  ) =>
  PIsData (PBuiltinMap k v)
  where
  pfromData x = punsafeCoerce $ pasMap # pforgetData x
  pdata x = punsafeBuiltin PLC.MapData # x

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

-- Overlapped by PBuiltinType instances
instance {-# OVERLAPPABLE #-} (PLift p, PIsData p) => PLift (PAsData p) where
  type PHaskellType (PAsData p) = (PHaskellType p)
  pconstant' =
    pdata . pconstant @p

  plift' t =
    plift' $ pfromData t

instance
  ( PlutusTx.ToData ha
  , PlutusTx.ToData hb
  , PlutusTx.FromData ha
  , PlutusTx.FromData hb
  ) =>
  PLift (PBuiltinType (PBuiltinPair (PAsData a) (PAsData b)) (ha, hb))
  where
  type
    PHaskellType (PBuiltinType (PBuiltinPair (PAsData a) (PAsData b)) (ha, hb)) =
      (ha, hb)

  pconstant' =
    punsafeCoerce
      . pconstant @(PBuiltinPair PData PData)
      . bimap PlutusTx.toData PlutusTx.toData

  plift' t = do
    let t' :: Term _ (PBuiltinPair PData PData)
        t' = punsafeCoerce t

    p <- plift' t'

    case bimap PlutusTx.fromData PlutusTx.fromData p of
      (Just x, Just y) -> Right (x, y)
      _ -> Left $ "failed on fromData"

instance
  ( PlutusTx.ToData h
  , PlutusTx.FromData h
  ) =>
  PLift (PBuiltinType (PBuiltinList (PAsData a)) [h])
  where
  type
    PHaskellType (PBuiltinType (PBuiltinList (PAsData a)) [h]) =
      [h]

  pconstant' =
    punsafeCoerce
      . pconstant @(PBuiltinList PData)
      . fmap PlutusTx.toData

  plift' t = do
    let t' :: Term _ (PBuiltinList PData)
        t' = punsafeCoerce t

    p <- plift' t'

    case traverse PlutusTx.fromData p of
      (Just xs) -> Right xs
      _ -> Left $ "failed on fromData"

instance
  {-# OVERLAPPABLE #-}
  ( PlutusTx.ToData ha
  , PlutusTx.ToData hb
  , PlutusTx.FromData ha
  , PlutusTx.FromData hb
  ) =>
  PLift (PBuiltinType (PBuiltinMap a b) [(ha, hb)])
  where
  type
    PHaskellType
      ( PBuiltinType
          (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))
          [(ha, hb)]
      ) =
      [(ha, hb)]

  pconstant' =
    punsafeCoerce
      . pconstant @(PBuiltinList (PBuiltinPair PData PData))
      . fmap (bimap PlutusTx.toData PlutusTx.toData)

  plift' t = do
    let t' :: Term _ (PBuiltinList (PBuiltinPair PData PData))
        t' = punsafeCoerce t

    ps <- plift' t'

    for (bimap PlutusTx.fromData PlutusTx.fromData <$> ps) $ \case
      (Just x, Just y) -> Right (x, y)
      _ -> Left $ "failed on fromData"
