{-# LANGUAGE FlexibleInstances #-}
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
  plistData,
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
  pserialiseData,
  ppairDataBuiltin,
  pchooseListBuiltin,
  pchooseData,
  PDataNewtype (..),
) where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool, pif, pif', (#&&), (#||))
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable (AsHaskell, PlutusRepr, fromPlutarch, fromPlutarchRepr, toPlutarch, toPlutarchRepr),
  PLifted (PLifted),
  fromPlutarchUni,
  getPLifted,
  pconstant,
  toPlutarchUni,
  unsafeToUni,
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd ((#<), (#<=)))
import Plutarch.Internal.Other (POpaque, pfix, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PCovariant,
  PVariant,
  PlutusType (PContravariant', PCovariant', PInner, PVariant', pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TermCont (runTermCont, tcont, unTermCont)
import Plutarch.Internal.Witness (witness)
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
  pfoldr',
  phead,
  plistEquals,
  pmap,
  pshowList,
  ptail,
 )
import Plutarch.Show (PShow (pshow'), pshow)
import Plutarch.String (PString)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.TryFrom (PSubtype, PTryFrom, PTryFromExcess, ptryFrom, ptryFrom', pupcast, pupcastF)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusTx (Data (Constr), ToData)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), BuiltinData (BuiltinData))

instance
  ( PShow a
  , PLC.Contains PLC.DefaultUni (PlutusRepr a)
  ) =>
  PShow (PBuiltinList a)
  where
  pshow' _ x = pshowList @PBuiltinList @a # x

instance PShow PData where
  pshow' b t0 = wrap (go0 # t0)
    where
      wrap s = pif (pconstant b) ("(" <> s <> ")") s
      go0 :: Term s (PData :--> PString)
      go0 = phoistAcyclic $
        pfix #$ plam $ \go t ->
          let pshowConstr pp0 = plet pp0 $ \pp ->
                "Constr "
                  <> pshow' False (pfstBuiltin # pp)
                  <> " "
                  <> pshowListPString # (pmap # go # (psndBuiltin # pp))
              pshowMap pplist =
                "Map " <> pshowListPString # (pmap # pshowPair # pplist)
              pshowPair = plam $ \pp0 -> plet pp0 $ \pp ->
                "("
                  <> (go # (pfstBuiltin # pp))
                  <> ", "
                  <> (go # (psndBuiltin # pp))
                  <> ")"
              pshowList xs = "List " <> pshowListPString # (pmap # go # xs)
              pshowListPString = phoistAcyclic $
                plam $ \plist ->
                  "["
                    <> pelimList
                      ( \x0 xs0 ->
                          x0 <> (pfoldr' (\x r -> ", " <> x <> r) # ("" :: Term s PString) # xs0)
                      )
                      ""
                      plist
                    <> "]"
           in pforce $
                pchooseData
                  # t
                  # pdelay (pshowConstr (pasConstr # t))
                  # pdelay (pshowMap (pasMap # t))
                  # pdelay (pshowList (pasList # t))
                  # pdelay ("I " <> pshow (pasInt # t))
                  # pdelay ("B " <> pshow (pasByteStr # t))

{-# DEPRECATED pdataLiteral "Use `pconstant` instead." #-}
pdataLiteral :: Data -> Term s PData
pdataLiteral = pconstant

instance (PIsData a, PShow a) => PShow (PAsData a) where
  pshow' w x = pshow' w (pfromData x)

instance forall (a :: S -> Type). PSubtype PData a => PIsData (PBuiltinList a) where
  pfromDataImpl x = punsafeCoerce $ pasList # pforgetData x
  pdataImpl x = plistData # pupcastF @PData @a (Proxy @PBuiltinList) x

instance (PShow a, PShow b) => PShow (PBuiltinPair a b) where
  pshow' _ pair = "(" <> pshow (pfstBuiltin # pair) <> "," <> pshow (psndBuiltin # pair) <> ")"

{- | Create a Plutarch-level 'PAsData' constant, from a Haskell value.
Example:
> pconstantData @PInteger 42
-}
pconstantData ::
  forall (p :: S -> Type) (h :: Type) (s :: S).
  (ToData h, AsHaskell p ~ h) =>
  h ->
  Term s (PAsData p)
pconstantData x = let _ = witness (Proxy @(AsHaskell p ~ h)) in punsafeCoerce $ pconstant @PData $ PlutusTx.toData x

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure (punsafeCoerce opq, ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
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
    pure (punsafeCoerce opq, ver)

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
    pure (punsafeCoerce opq, ver)

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
    pure (punsafeCoerce opq, ver)

----------------------- other utility functions -----------------------------------------

instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Const ()
  ptryFrom' opq = runTermCont $ pure (pdata opq, ())

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = Const ()
  ptryFrom' opq f = f (opq, ())

-- | @since 1.7.0
newtype PDataNewtype (a :: S -> Type) (s :: S) = PDataNewtype (Term s (PAsData a))
  deriving stock
    ( -- | @since 1.7.0
      Generic
    )

-- | @since 1.7.0
instance PlutusType (PDataNewtype a) where
  type PInner (PDataNewtype a) = PData
  pcon' (PDataNewtype a) = pforgetData a
  pmatch' x' f = f (PDataNewtype (punsafeCoerce x'))

-- | @since 1.7.0
instance PIsData (PDataNewtype a) where
  pfromDataImpl = punsafeCoerce
  pdataImpl = punsafeCoerce

-- | @since 1.7.0
instance PEq (PDataNewtype a) where
  a #== b = pto a #== pto b

-- | @since 1.7.0
instance (PIsData a, POrd a) => POrd (PDataNewtype a) where
  {-# INLINEABLE (#<=) #-}
  a #<= b =
    pmatch a $ \(PDataNewtype a') ->
      pmatch b $ \(PDataNewtype b') ->
        pfromData a' #<= pfromData b'
  {-# INLINEABLE (#<) #-}
  a #< b =
    pmatch a $ \(PDataNewtype a') ->
      pmatch b $ \(PDataNewtype b') ->
        pfromData a' #< pfromData b'

-- | @since 1.7.0
instance (PIsData a, PShow a) => PShow (PDataNewtype a) where
  pshow' x t =
    pmatch t \(PDataNewtype t') -> pshow' x $ pfromData t'

-- | @since 1.7.0
instance (PIsData a, PTryFrom PData (PAsData a)) => PTryFrom PData (PDataNewtype a)

-- | @since 1.7.0
instance PTryFrom PData (PAsData (PDataNewtype a))

-- | @since 1.7.0
instance PTryFrom PData (PAsData PBool) where
  type PTryFromExcess PData (PAsData PBool) = Const ()
  ptryFrom' opq = runTermCont $ do
    asConstr <- tcont . plet $ pasConstr # opq
    let ix = pfstBuiltin # asConstr
    tcont $ \f ->
      pif
        (ix #== 0 #|| ix #== 1)
        (f ())
        (ptraceInfoError "PTryFrom(PAsData PBool): invalid constructor tag")
    let dat = psndBuiltin # asConstr
    tcont $ \f ->
      pif
        (pnull # dat)
        (f ())
        (ptraceInfoError "PTryFrom(PAsData PBool): non-empty constructor list")
    pure (punsafeCoerce opq, ())
