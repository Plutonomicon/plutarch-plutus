{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Data where

import Data.Kind (Type)

import Data.Proxy (Proxy (Proxy))
import {-# SOURCE #-} Plutarch.Builtin.Bool
import {-# SOURCE #-} Plutarch.ByteString
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift
import Plutarch.Internal.List
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PLam
import Plutarch.Internal.PlutusType
import Plutarch.Internal.Term
import Plutarch.TryFrom
import PlutusCore qualified as PLC
import PlutusTx (Data)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), BuiltinData (BuiltinData))

newtype PData (s :: S) = PData (Term s PData)

instance PlutusType PData where
  type PInner PData = PData
  type PCovariant' PData = ()
  type PContravariant' PData = ()
  type PVariant' PData = ()
  pcon' (PData t) = t
  pmatch' t f = f (PData t)

instance PIsData PData where
  pfromDataImpl = pupcast
  pdataImpl = id

-- | @since WIP
deriving via
  DeriveBuiltinPLiftable PData Data
  instance
    PLiftable PData

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

-- instance PShow PData where
--   pshow' b t0 = wrap (go0 # t0)
--     where
--       wrap s = pif (pconstant b) ("(" <> s <> ")") s
--       go0 :: Term s (PData :--> PString)
--       go0 = phoistAcyclic $
--         pfix #$ plam $ \go t ->
--           let pshowConstr pp0 = plet pp0 $ \pp ->
--                 "Constr "
--                   <> pshow' False (pfstBuiltin # pp)
--                   <> " "
--                   <> pshowListPString # (pmap # go # (psndBuiltin # pp))
--               pshowMap pplist =
--                 "Map " <> pshowListPString # (pmap # pshowPair # pplist)
--               pshowPair = plam $ \pp0 -> plet pp0 $ \pp ->
--                 "("
--                   <> (go # (pfstBuiltin # pp))
--                   <> ", "
--                   <> (go # (psndBuiltin # pp))
--                   <> ")"
--               pshowList xs = "List " <> pshowListPString # (pmap # go # xs)
--               pshowListPString = phoistAcyclic $
--                 plam $ \plist ->
--                   "["
--                     <> pelimList
--                       ( \x0 xs0 ->
--                           x0 <> (pfoldr' (\x r -> ", " <> x <> r) # ("" :: Term s PString) # xs0)
--                       )
--                       ""
--                       plist
--                     <> "]"
--            in pforce $
--                 pchooseData
--                   # t
--                   # pdelay (pshowConstr (pasConstr # t))
--                   # pdelay (pshowMap (pasMap # t))
--                   # pdelay (pshowList (pasList # t))
--                   # pdelay ("I " <> pshow (pasInt # t))
--                   # pdelay ("B " <> pshow (pasByteStr # t))

-- pchooseData :: Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
-- pchooseData = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ChooseData

-- pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
-- pasConstr = punsafeBuiltin PLC.UnConstrData

-- pasMap :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData))
-- pasMap = punsafeBuiltin PLC.UnMapData

-- plistData :: Term s (PBuiltinList PData :--> PData)
-- plistData = punsafeBuiltin PLC.ListData

-- pasList :: Term s (PData :--> PBuiltinList PData)
-- pasList = punsafeBuiltin PLC.UnListData

-- pasInt :: Term s (PData :--> PInteger)
-- pasInt = punsafeBuiltin PLC.UnIData

-- pasByteStr :: Term s (PData :--> PByteString)
-- pasByteStr = punsafeBuiltin PLC.UnBData

-- -- | Serialise any builtin data to its cbor represented by a builtin bytestring
-- pserialiseData :: Term s (PData :--> PByteString)
-- pserialiseData = punsafeBuiltin PLC.SerialiseData

-- {-# DEPRECATED pdataLiteral "Use `pconstant` instead." #-}
-- pdataLiteral :: Data -> Term s PData
-- pdataLiteral = pconstant

-- This belows to Plutarch.Show
-- instance PShow PData where
--   pshow' b t0 = wrap (go0 # t0)
--     where
--       wrap s = pif (pconstant b) ("(" <> s <> ")") s
--       go0 :: Term s (PData :--> PString)
--       go0 = phoistAcyclic $
--         pfix #$ plam $ \go t ->
--           let pshowConstr pp0 = plet pp0 $ \pp ->
--                 "Constr "
--                   <> pshow' False (pfstBuiltin # pp)
--                   <> " "
--                   <> pshowListPString # (pmap # go # (psndBuiltin # pp))
--               pshowMap pplist =
--                 "Map " <> pshowListPString # (pmap # pshowPair # pplist)
--               pshowPair = plam $ \pp0 -> plet pp0 $ \pp ->
--                 "("
--                   <> (go # (pfstBuiltin # pp))
--                   <> ", "
--                   <> (go # (psndBuiltin # pp))
--                   <> ")"
--               pshowList xs = "List " <> pshowListPString # (pmap # go # xs)
--               pshowListPString = phoistAcyclic $
--                 plam $ \plist ->
--                   "["
--                     <> pelimList
--                       ( \x0 xs0 ->
--                           x0 <> (pfoldr' (\x r -> ", " <> x <> r) # ("" :: Term s PString) # xs0)
--                       )
--                       ""
--                       plist
--                     <> "]"
--            in pforce $
--                 pchooseData
--                   # t
--                   # pdelay (pshowConstr (pasConstr # t))
--                   # pdelay (pshowMap (pasMap # t))
--                   # pdelay (pshowList (pasList # t))
--                   # pdelay ("I " <> pshow (pasInt # t))
--                   # pdelay ("B " <> pshow (pasByteStr # t))

-- --------------------------------------------------------------------------------

newtype PBuiltinPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBuiltinPair (Term s (PBuiltinPair a b))

instance PlutusType (PBuiltinPair a b) where
  type PInner (PBuiltinPair a b) = PBuiltinPair a b
  type PCovariant' (PBuiltinPair a b) = (PCovariant' a, PCovariant' b)
  type PContravariant' (PBuiltinPair a b) = (PContravariant' a, PContravariant' b)
  type PVariant' (PBuiltinPair a b) = (PVariant' a, PVariant' b)
  pcon' (PBuiltinPair x) = x
  pmatch' x f = f (PBuiltinPair x)

-- | @since WIP
instance
  ( PLiftable a
  , PLC.Contains PLC.DefaultUni (PlutusRepr a)
  , PLiftable b
  , PLC.Contains PLC.DefaultUni (PlutusRepr b)
  ) =>
  PLiftable (PBuiltinPair a b)
  where
  type AsHaskell (PBuiltinPair a b) = (AsHaskell a, AsHaskell b)
  type PlutusRepr (PBuiltinPair a b) = (PlutusRepr a, PlutusRepr b)

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr (a, b) = (toPlutarchRepr @a a, toPlutarchRepr @b b)

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr (ar, br) = do
    a <- fromPlutarchRepr @a ar
    b <- fromPlutarchRepr @b br
    pure (a, b)

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

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

-- instance
--   ( PShow a
--   , PLC.Contains PLC.DefaultUni (PlutusRepr a)
--   ) =>
--   PShow (PBuiltinList a)
--   where
--   pshow' _ x = pshowList @PBuiltinList @a # x

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

instance PLC.Contains PLC.DefaultUni (PlutusRepr a) => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) = PBuiltinList a
  type PCovariant' (PBuiltinList a) = PCovariant' a
  type PContravariant' (PBuiltinList a) = PContravariant' a
  type PVariant' (PBuiltinList a) = PVariant' a
  pcon' (PCons x xs) = pconsBuiltin # x # xs
  pcon' PNil = getPLifted $ unsafeToUni @[PlutusRepr a] []
  pmatch' xs' f = plet xs' $ \xs ->
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

-- | @since WIP
instance (PLiftable a, PLC.Contains PLC.DefaultUni (PlutusRepr a)) => PLiftable (PBuiltinList a) where
  type AsHaskell (PBuiltinList a) = [AsHaskell a]
  type PlutusRepr (PBuiltinList a) = [PlutusRepr a]

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = map (toPlutarchRepr @a)

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = traverse (fromPlutarchRepr @a)

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = (PLC.Contains PLC.DefaultUni (PlutusRepr a))

  pelimList match_cons match_nil ls = pmatch ls $ \case
    PCons x xs -> match_cons x xs
    PNil -> match_nil

  pcons = plam $ \x xs -> pcon (PCons x xs)
  pnil = pcon PNil
  phead = pheadBuiltin
  ptail = ptailBuiltin
  pnull = pnullBuiltin

type family F (a :: S -> Type) :: Bool where
  F PData = 'True
  F (PAsData _) = 'True
  F _ = 'False

class Fc (x :: Bool) (a :: S -> Type) where
  fc :: Proxy x -> Term s (PBuiltinList a) -> Term s (PBuiltinList a) -> Term s PBool

instance (PEq a, PLC.Contains PLC.DefaultUni (PlutusRepr a)) => Fc 'False a where
  fc _ xs ys = plistEquals # xs # ys

instance PIsData (PBuiltinList a) => Fc 'True a where
  fc _ xs ys = pdata xs #== pdata ys

instance Fc (F a) a => PEq (PBuiltinList a) where
  (#==) = fc (Proxy @(F a))

newtype PAsData (a :: S -> Type) (s :: S) = PAsData (Term s a)

type family IfSameThenData (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  IfSameThenData a a = PData
  IfSameThenData _ POpaque = PData
  IfSameThenData _ b = PAsData b

instance PIsData a => PlutusType (PAsData a) where
  type PInner (PAsData a) = IfSameThenData a (PInner a)
  type PCovariant' (PAsData a) = PCovariant' a
  type PContravariant' (PAsData a) = PContravariant' a
  type PVariant' (PAsData a) = PVariant' a
  pcon' (PAsData t) = punsafeCoerce $ pdata t
  pmatch' t f = f (PAsData $ pfromData $ punsafeCoerce t)

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

instance {-# OVERLAPPING #-} PLiftable (PAsData PByteString) where
  type AsHaskell (PAsData PByteString) = AsHaskell PByteString
  type PlutusRepr (PAsData PByteString) = Data
  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = PlutusTx.toData . BuiltinByteString
  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni
  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr x = (\(BuiltinByteString str) -> str) <$> PlutusTx.fromData x
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

instance {-# OVERLAPPING #-} PLiftable (PAsData PData) where
  type AsHaskell (PAsData PData) = AsHaskell PData
  type PlutusRepr (PAsData PData) = Data
  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = PlutusTx.toData . BuiltinData
  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni
  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr x = (\(BuiltinData str) -> str) <$> PlutusTx.fromData x
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

instance (PlutusTx.ToData (AsHaskell a), PlutusTx.FromData (AsHaskell a), PIsData a) => PLiftable (PAsData a) where
  type AsHaskell (PAsData a) = AsHaskell a
  type PlutusRepr (PAsData a) = Data

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = PlutusTx.toData

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = PlutusTx.fromData

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

-- instance (PIsData a, PShow a) => PShow (PAsData a) where
--   pshow' w x = pshow' w (pfromData x)
