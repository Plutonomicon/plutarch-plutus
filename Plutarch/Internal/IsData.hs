{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.IsData (
  PInnerMostIsData,
  PIsData,
  pfromDataImpl,
  pdataImpl,
  pdata,
  pfromData,
  pforgetData,
  prememberData,
  pforgetData',
  prememberData',
) where

import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import Plutarch.Builtin.Bool (PBool, pif')
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
  pasConstr,
  pasList,
  pconstrBuiltin,
  pfstBuiltin,
  plistData,
  ppairDataBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger, pconstantInteger)
import Plutarch.Builtin.Unit (PUnit, punit)

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))

import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.ListLike (
  PListLike (pcons, phead, pnil, ptail),
 )

import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (PLamN (plam))
import Plutarch.Internal.PlutusType (
  PCovariant,
  PInnerMost,
  PVariant,
  PlutusType (PInner),
 )
import Plutarch.Internal.Subtype (PSubtype, pupcast, pupcastF)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  punsafeCoerce,
  punsafeConstantInternal,
  (#),
  (#$),
 )
import Plutarch.Internal.Witness (witness)
import Plutarch.Unsafe (punsafeDowncast)

import PlutusCore qualified as PLC
import PlutusTx qualified as PTx

type family PInnerMostIsData' msg a b :: Constraint where
  PInnerMostIsData' _ _ PData = ()
  PInnerMostIsData' ('Just msg) a b =
    TypeError
      ( 'Text msg
          ':$$: 'Text "Inner most representation of \""
            ':<>: 'ShowType a
            ':<>: 'Text "\" is \""
            ':<>: 'ShowType b
            ':<>: 'Text "\""
      )
      ~ ()
  PInnerMostIsData' 'Nothing a b =
    TypeError
      ( 'Text "Inner most representation of \""
          ':<>: 'ShowType a
          ':<>: 'Text "\" is \""
          ':<>: 'ShowType b
          ':<>: 'Text "\""
      )
      ~ ()

class (PInnerMostIsData' msg a (PInnerMost a), PInnerMost a ~ PData) => PInnerMostIsData msg a
instance (PInnerMostIsData' msg a (PInnerMost a), PInnerMost a ~ PData) => PInnerMostIsData msg a

{- | Laws:
 - If @PSubtype PData a@, then @pdataImpl a@ must be `pupcast`.
 - pdataImpl . pupcast . pfromDataImpl ≡ id
 - pfromDataImpl . punsafeDowncast . pdataImpl ≡ id
-}
class PIsData a where
  pfromDataImpl :: Term s (PAsData a) -> Term s a
  default pfromDataImpl :: PIsData (PInner a) => Term s (PAsData a) -> Term s a
  pfromDataImpl x = punsafeDowncast $ pfromDataImpl (punsafeCoerce x :: Term _ (PAsData (PInner a)))

  pdataImpl :: Term s a -> Term s PData
  default pdataImpl :: PIsData (PInner a) => Term s a -> Term s PData
  pdataImpl x = pdataImpl $ pto x

pfromData :: PIsData a => Term s (PAsData a) -> Term s a
pfromData = pfromDataImpl

pdata :: PIsData a => Term s a -> Term s (PAsData a)
pdata = punsafeCoerce . pdataImpl

pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
pforgetData = punsafeCoerce

-- FIXME: remove, broken

{- | Like 'pforgetData', except it works for complex types.
 Equivalent to 'pupcastF'.
-}
pforgetData' ::
  forall a (p :: (S -> Type) -> S -> Type) (s :: S).
  PCovariant p =>
  Proxy p ->
  Term s (p (PAsData a)) ->
  Term s (p PData)
pforgetData' _ = let _ = witness (Proxy @(PCovariant p)) in punsafeCoerce

-- | Inverse of 'pforgetData''.
prememberData ::
  forall (p :: (S -> Type) -> S -> Type) (s :: S).
  PVariant p =>
  Proxy p ->
  Term s (p PData) ->
  Term s (p (PAsData PData))
prememberData Proxy = let _ = witness (Proxy @(PVariant p)) in punsafeCoerce

-- | Like 'prememberData' but generalised.
prememberData' ::
  forall a (p :: (S -> Type) -> S -> Type) (s :: S).
  (PInnerMostIsData 'Nothing a, PSubtype PData a, PVariant p) =>
  Proxy p ->
  Term s (p a) ->
  Term s (p (PAsData a))
prememberData' Proxy =
  let _ = witness (Proxy @(PInnerMostIsData 'Nothing a, PSubtype PData a, PVariant p))
   in punsafeCoerce

instance PIsData PData where
  pfromDataImpl = pupcast
  pdataImpl = id

instance PIsData PBool where
  pfromDataImpl x =
    phoistAcyclic (plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pfstBuiltin # (pasConstr # d) #== pconstantInteger 1

  pdataImpl x =
    phoistAcyclic (plam toData) # x
    where
      toData :: Term s PBool -> Term s PData
      toData b =
        punsafeBuiltin PLC.ConstrData
          # (pif' # b # pconstantInteger 1 # (pconstantInteger 0 :: Term s PInteger))
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

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
        plam $
          \pair -> pconstrBuiltin # pconstantInteger 0 #$ pcons # (pfstBuiltin # pair) #$ pcons # (psndBuiltin # pair) # pnil

instance PIsData (PBuiltinPair PData PData) where
  pfromDataImpl = pfromDataImpl @(PBuiltinPair PData PData) . punsafeCoerce
  pdataImpl = punsafeCoerce . pdataImpl @(PBuiltinPair PData PData)

-- This instance is kind of useless. There's no safe way to use 'pdata'.
instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromDataImpl x = pasConstr # pupcast x
  pdataImpl x' = pupcast $ plet x' $ \x -> pconstrBuiltin # (pfstBuiltin # x) #$ psndBuiltin # x

instance PIsData PInteger where
  pfromDataImpl x = punsafeBuiltin PLC.UnIData # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.IData # x

instance PIsData PByteString where
  pfromDataImpl x = punsafeBuiltin PLC.UnBData # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.BData # x

instance PIsData PUnit where
  pfromDataImpl _ = punit
  pdataImpl _ = punsafeConstantInternal $ PLC.someValue (PTx.Constr 0 [])

instance
  forall (a :: S -> Type).
  ( PInnerMostIsData ('Just "PBuiltinList only implements PIsData when inner most type of its elements are PData") a
  , PSubtype PData a
  ) =>
  PIsData (PBuiltinList a)
  where
  pfromDataImpl x = punsafeCoerce $ pasList # pforgetData x
  pdataImpl x = plistData # pupcastF @PData @a (Proxy @PBuiltinList) x
