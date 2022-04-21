{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin (
  PData,
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  pasMap,
  pasList,
  pasInt,
  pconstantData,
  pconstrBuiltin,
  pasByteStr,
  PBuiltinPair,
  PBuiltinList (..),
  pdataLiteral,
  PIsData (..),
  pdata,
  pfromData,
  PAsData,
  pforgetData,
  prememberData,
  prememberData',
  ppairDataBuiltin,
  pchooseListBuiltin,
  type PBuiltinMap,
) where

import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (Proxy))
import Plutarch (
  DerivePNewtype,
  PInner,
  POpaque,
  PType,
  PlutusType,
  S,
  Term,
  pcon,
  pcon',
  pdelay,
  pforce,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  pmatch',
  pto,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PBool (..), PEq, pif', (#&&), (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstant,
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLift,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.List (
  PListLike (
    PElemConstraint,
    pcons,
    pelimList,
    phead,
    pnil,
    pnull,
    ptail
  ),
  phead,
  plistEquals,
  pmap,
  pshowList,
  ptail,
 )
import Plutarch.Show (PShow (pshow'), pshow)
import Plutarch.Unit (PUnit)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce, punsafeDowncast)
import qualified PlutusCore as PLC
import PlutusTx (Data (Constr), ToData)
import qualified PlutusTx

import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)

import Plutarch.Reducible (Reducible (Reduce))

import Data.Functor.Const (Const)

import Plutarch.TryFrom (PSubtype, PTryFrom, PTryFromExcess, ptryFrom, ptryFrom', pupcast)

-- | Plutus 'BuiltinPair'
data PBuiltinPair (a :: PType) (b :: PType) (s :: S)

instance (PLift a, PLift b) => PUnsafeLiftDecl (PBuiltinPair a b) where
  type PLifted (PBuiltinPair a b) = (PLifted a, PLifted b)

-- FIXME: figure out good way of deriving this
instance (PConstant a, PConstant b) => PConstantDecl (a, b) where
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

instance (PShow a, PLift a) => PShow (PBuiltinList a) where
  pshow' _ x = pshowList @PBuiltinList @a # x

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

instance PConstant a => PConstantDecl [a] where
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
  pmatch' xs' f = plet xs' $ \xs ->
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

instance {-# OVERLAPPING #-} PIsData a => PEq (PBuiltinList (PAsData a)) where
  xs #== ys = pdata xs #== pdata ys

instance {-# OVERLAPPING #-} PEq (PBuiltinList PData) where
  xs #== ys = pdata xs #== pdata ys

data PData (s :: S) = PData (Term s PData)

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' (PData t) = t
  pmatch' t f = f (PData t)

instance PUnsafeLiftDecl PData where type PLifted PData = Data
deriving via (DerivePConstantDirect Data PData) instance PConstantDecl Data

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
data PAsData (a :: PType) (s :: S) = PAsData (Term s a)

instance PIsData a => PlutusType (PAsData a) where
  type PInner (PAsData a) _ = PData
  pcon' (PAsData t) = pforgetData $ pdata t
  pmatch' t f = f (PAsData $ pfromData $ punsafeCoerce t)

type role PAsDataLifted nominal
data PAsDataLifted (a :: PType)

instance PConstantDecl (PAsDataLifted a) where
  type PConstantRepr (PAsDataLifted a) = Data
  type PConstanted (PAsDataLifted a) = PAsData a
  pconstantToRepr = \case {}
  pconstantFromRepr _ = Nothing

instance PUnsafeLiftDecl (PAsData a) where type PLifted (PAsData a) = PAsDataLifted a

pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
pforgetData = pupcast

-- FIXME: remove, broken

{- | Like 'pforgetData', except it works for complex types.
 Equivalent to 'pupcastF'.
-}
pforgetData' :: forall a (p :: PType -> PType) s. Proxy p -> Term s (p (PAsData a)) -> Term s (p PData)
pforgetData' _ = punsafeCoerce

-- | Inverse of 'pforgetData''.
prememberData :: forall (p :: PType -> PType) s. Proxy p -> Term s (p PData) -> Term s (p (PAsData PData))
prememberData Proxy = punsafeCoerce

-- | Like 'prememberData' but generalised.
prememberData' :: forall a (p :: PType -> PType) s. PSubtype PData a => Proxy p -> Term s (p a) -> Term s (p (PAsData a))
prememberData' Proxy = punsafeCoerce

{- | Laws:
 - If @PSubtype PData a@, then @pdataImpl a@ must be `pupcast`.
 - pdataImpl . pupcast . pfromDataImpl ≡ id
 - pfromDataImpl . punsafeDowncast . pdataImpl ≡ id
-}
class PIsData a where
  pfromDataImpl :: Term s (PAsData a) -> Term s a
  pdataImpl :: Term s a -> Term s PData

pfromData :: PIsData a => Term s (PAsData a) -> Term s a
pfromData = pfromDataImpl
pdata :: PIsData a => Term s a -> Term s (PAsData a)
pdata = punsafeCoerce . pdataImpl

instance PIsData PData where
  pfromDataImpl = pupcast
  pdataImpl = id

instance PIsData a => PIsData (PBuiltinList (PAsData a)) where
  pfromDataImpl x = punsafeCoerce $ pasList # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.ListData # x

newtype Helper2 f a s = Helper2 (PAsData (f a) s)

instance PIsData (PBuiltinList PData) where
  pfromDataImpl = pforgetData' @PData (Proxy @PBuiltinList) . pfromData . coerce (prememberData (Proxy @(Helper2 PBuiltinList)))

  -- pdataImpl = coerce (pforgetData' @PData (Proxy @(Helper2 PBuiltinList))) . pdata . (prememberData (Proxy @PBuiltinList))
  pdataImpl = punsafeCoerce . pdata . (prememberData (Proxy @PBuiltinList)) -- FIXME

instance PIsData (PBuiltinMap k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

instance PIsData PInteger where
  pfromDataImpl x = pasInt # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.IData # x

instance PIsData PByteString where
  pfromDataImpl x = pasByteStr # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.BData # x

{- |
  Instance for PBool following the Plutus IsData repr
  given by @makeIsDataIndexed ''Bool [('False,0),('True,1)]@,
  which is used in 'TxInfo' via 'Closure'.
-}
instance PIsData PBool where
  pfromDataImpl x =
    (phoistAcyclic $ plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pfstBuiltin # (pasConstr # d) #== 1

  pdataImpl x =
    (phoistAcyclic $ plam toData) # x
    where
      toData :: Term s PBool -> Term s PData
      toData b =
        punsafeBuiltin PLC.ConstrData
          # (pif' # b # 1 # (0 :: Term s PInteger))
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

-- | NB: `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` have the same representation.
instance PIsData (PBuiltinPair (PAsData a) (PAsData b)) where
  pfromDataImpl x = f # x
    where
      f = phoistAcyclic $
        plam $ \pairDat -> plet (psndBuiltin #$ pasConstr # pforgetData pairDat) $
          \pd -> ppairDataBuiltin # punsafeCoerce (phead # pd) #$ punsafeCoerce (phead #$ ptail # pd)
  pdataImpl x = pupcast target
    where
      target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
      target = f # punsafeCoerce x
      f = phoistAcyclic $
        plam $ \pair -> pconstrBuiltin # 0 #$ pcons # (pfstBuiltin # pair) #$ pcons # (psndBuiltin # pair) # pnil

newtype Helper3 f b a s = Helper3 (PAsData (f a b) s)

newtype Helper4 f b a s = Helper4 (f a b s)

instance PIsData (PBuiltinPair PData PData) where
  pfromDataImpl = f . pfromData . g
    where
      g :: Term s (PAsData (PBuiltinPair PData PData)) -> Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData)))
      g =
        (coerce (prememberData (Proxy @(Helper3 PBuiltinPair (PAsData PData)))) :: Term s (PAsData (PBuiltinPair PData (PAsData PData))) -> Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData))))
          . coerce (prememberData (Proxy @(Helper2 (PBuiltinPair PData))))

      f :: Term s (PBuiltinPair (PAsData PData) (PAsData PData)) -> Term s (PBuiltinPair PData PData)
      f =
        coerce (pforgetData' (Proxy @(Helper4 PBuiltinPair PData)))
          . (pforgetData' @PData (Proxy @(PBuiltinPair (PAsData PData))))
  pdataImpl = pupcast . f . pdata . g
    where
      g :: Term s (PBuiltinPair PData PData) -> Term s (PBuiltinPair (PAsData PData) (PAsData PData))
      g = coerce (prememberData (Proxy @(Helper4 PBuiltinPair (PAsData PData)))) . prememberData (Proxy @(PBuiltinPair PData))

      f :: Term s (PAsData (PBuiltinPair (PAsData PData) (PAsData PData))) -> Term s (PAsData (PBuiltinPair PData PData))
      f =
        (coerce (pforgetData' @PData (Proxy @(Helper3 PBuiltinPair PData))) :: Term s (PAsData (PBuiltinPair (PAsData PData) PData)) -> Term s (PAsData (PBuiltinPair PData PData)))
          . coerce (pforgetData' @PData (Proxy @(Helper2 (PBuiltinPair (PAsData PData)))))

instance (PShow a, PShow b) => PShow (PBuiltinPair a b) where
  pshow' _ pair = "(" <> pshow (pfstBuiltin # pair) <> "," <> pshow (psndBuiltin # pair) <> ")"

instance (PEq a, PEq b) => PEq (PBuiltinPair a b) where
  p1 #== p2 = pfstBuiltin # p1 #== pfstBuiltin # p2 #&& psndBuiltin # p1 #== psndBuiltin # p2

instance PIsData PUnit where
  pfromDataImpl _ = pconstant ()
  pdataImpl _ = pconstant (Constr 0 [])

-- This instance is kind of useless. There's no safe way to use 'pdata'.
instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromDataImpl x = pasConstr # pupcast x
  pdataImpl x' = pupcast $ plet x' $ \x -> pconstrBuiltin # (pfstBuiltin # x) #$ psndBuiltin # x

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

instance
  ( forall (s :: S). Coercible (a s) (Term s b)
  , PInner a POpaque ~ b
  , PIsData b
  ) =>
  PIsData (DerivePNewtype a b)
  where
  pfromDataImpl x = punsafeDowncast $ (pfromDataImpl (punsafeCoerce x) :: Term _ b)
  pdataImpl x = pdataImpl $ pto x

instance (PIsData a, PShow a) => PShow (PAsData a) where
  pshow' w x = pshow' w (pfromData x)

pconstrBuiltin :: Term s (PInteger :--> PBuiltinList PData :--> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
pconstrBuiltin = punsafeBuiltin $ PLC.ConstrData

{- | Create a Plutarch-level 'PAsData' constant, from a Haskell value.
Example:
> pconstantData @PInteger 42
-}
pconstantData :: forall p h s. (ToData h, PLifted p ~ h, PConstanted h ~ p) => h -> Term s (PAsData p)
pconstantData x = punsafeCoerce $ pconstant $ PlutusTx.toData x

newtype Flip f a b = Flip (f b a)

instance Reducible (f x y) => Reducible (Flip f y x) where
  type Reduce (Flip f y x) = Reduce (f x y)

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, ver)

instance
  ( PTryFrom PData (PAsData a)
  , PTryFrom PData (PAsData b)
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = Flip Term (PBuiltinMap a b)
  ptryFrom' opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- tcont $ ptryFrom @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- tcont $ ptryFrom @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, ver)

{- |
    This verifies a list to be indeed a list but doesn't recover the inner data
    use this instance instead of the one for `PData (PAsData (PBuiltinList (PAsData a)))`
    as this is O(1) instead of O(n)
-}

-- TODO: add the excess inner type list
instance PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = Flip Term (PBuiltinList PData)
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PBuiltinList (PAsData a)`
-}
instance
  ( PTryFrom PData (PAsData a)
  , PIsData a
  ) =>
  PTryFrom PData (PAsData (PBuiltinList (PAsData a)))
  where
  type PTryFromExcess PData (PAsData (PBuiltinList (PAsData a))) = Flip Term (PBuiltinList (PAsData a))
  ptryFrom' opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pasList # opq
        verify :: Term _ (PData :--> PAsData a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- tcont $ ptryFrom @(PAsData a) $ e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PAsData (PBuiltinPair a b)`
-}
instance
  ( PTryFrom PData a
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom PData b
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom PData (PAsData (PBuiltinPair a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = Flip Term (PBuiltinPair a b)
  ptryFrom' opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> tcont (ptryFrom @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> tcont (ptryFrom @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, ver)

----------------------- other utility functions -----------------------------------------

instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Const ()
  ptryFrom' opq = runTermCont $ pure (pdata opq, ())
