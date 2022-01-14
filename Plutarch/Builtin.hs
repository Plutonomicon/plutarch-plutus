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
) where

import Plutarch (PlutusType (..), punsafeBuiltin, punsafeCoerce)
import Plutarch.Bool (PBool (..), PEq, pif', (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (DerivePLiftViaCoercible, PLift, PLifted, PLiftedRepr, PUnsafeLiftDecl, pconstant, pliftFromRepr, pliftToRepr)
import Plutarch.List (PListLike (..), plistEquals)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import PlutusTx (Data)

-- | Plutus 'BuiltinPair'
data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

-- FIXME: figure out good way of deriving this
instance (PUnsafeLiftDecl ah a, PUnsafeLiftDecl bh b) => PUnsafeLiftDecl (ah, bh) (PBuiltinPair a b) where
  type PLiftedRepr (PBuiltinPair a b) = (PLiftedRepr a, PLiftedRepr b)
  type PLifted (PBuiltinPair a b) = (PLifted a, PLifted b)
  pliftToRepr (x, y) = (pliftToRepr @_ @_ @a x, pliftToRepr @_ @_ @b y)
  pliftFromRepr (x, y) = do
    x' <- pliftFromRepr @_ @_ @a x
    y' <- pliftFromRepr @_ @_ @b y
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
data PBuiltinList (a :: k -> Type) (s :: k)
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

instance PUnsafeLiftDecl ah a => PUnsafeLiftDecl [ah] (PBuiltinList a) where
  type PLifted (PBuiltinList a) = [PLifted a]
  type PLiftedRepr (PBuiltinList a) = [PLiftedRepr a]
  pliftToRepr x = pliftToRepr @_ @_ @a <$> x
  pliftFromRepr x = traverse (pliftFromRepr @_ @_ @a) x

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
  deriving (PUnsafeLiftDecl Data) via (DerivePLiftViaCoercible Data PData Data)

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

{- |
  Instance for PBool following the Plutus IsData repr
  given by @makeIsDataIndexed ''Bool [('False,0),('True,1)]@,
  which is used in 'TxInfo' via 'Closure'.
-}
instance PIsData PBool where
  pfromData x =
    (phoistAcyclic $ plam $ \d -> toBool #$ tag # d)
      # pforgetData x
    where
      toBool :: Term s (PInteger :--> PBool)
      toBool = phoistAcyclic $ plam (#== 1)

      tag :: Term s (PData :--> PInteger)
      tag = phoistAcyclic $ plam $ \d -> pfstBuiltin #$ pasConstr # d

  pdata x =
    (phoistAcyclic $ plam $ \b -> constr #$ toInt # b)
      # x
    where
      toInt :: Term s (PBool :--> PInteger)
      toInt = phoistAcyclic $ plam $ \b -> pif' # b # 1 # 0

      nil :: Term s (PBuiltinList PData)
      nil = pnil

      constr :: Term s (PInteger :--> PAsData PBool)
      constr = phoistAcyclic $
        plam $ \x ->
          punsafeBuiltin PLC.ConstrData # x # nil

instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromData x = pasConstr # pforgetData x
  pdata x' = plet x' $ \x -> punsafeBuiltin PLC.ConstrData # (pfstBuiltin # x) #$ psndBuiltin # x

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y
