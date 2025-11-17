{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.IsData (
  PInnermostIsData,
  PIsData,
  pfromDataImpl,
  pdataImpl,
  pdata,
  pfromData,
  pforgetData,
) where

import Data.Kind (Constraint, Type)
import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (Symbol)
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair (PBuiltinPair),
  PData,
  pasConstr,
  pasList,
  pconstrBuiltin,
  plistData,
  ppairDataBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger, pconstantInteger)
import Plutarch.Builtin.Unit (PUnit, punit)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.ListLike (
  PListLike (pcons, phead, pnil, ptail),
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (PLamN (plam))
import Plutarch.Internal.PlutusType (
  PInnermost,
  PlutusType (PInner),
  pmatch,
 )
import Plutarch.Internal.Subtype (PSubtype, pupcast)
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
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusTx qualified as PTx

type family PInnermostIsData' (msg :: Maybe Symbol) (a :: k) (b :: S -> Type) :: Constraint where
  PInnermostIsData' _ _ PData = ()
  PInnermostIsData' ('Just msg) a b =
    TypeError
      ( 'Text msg
          ':$$: 'Text "Inner most representation of \""
            ':<>: 'ShowType a
            ':<>: 'Text "\" is \""
            ':<>: 'ShowType b
            ':<>: 'Text "\""
      )
      ~ ()
  PInnermostIsData' 'Nothing a b =
    TypeError
      ( 'Text "Inner most representation of \""
          ':<>: 'ShowType a
          ':<>: 'Text "\" is \""
          ':<>: 'ShowType b
          ':<>: 'Text "\""
      )
      ~ ()

class (PInnermostIsData' msg a (PInnermost a), PInnermost a ~ PData) => PInnermostIsData msg a
instance (PInnermostIsData' msg a (PInnermost a), PInnermost a ~ PData) => PInnermostIsData msg a

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

instance PIsData PData where
  pfromDataImpl = pupcast
  pdataImpl = id

instance PIsData PBool where
  pfromDataImpl x =
    phoistAcyclic (plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pmatch (pasConstr # d) $ \(PBuiltinPair x _) ->
        x #== pconstantInteger 1

  pdataImpl x =
    phoistAcyclic (plam toData) # x
    where
      toData :: Term s PBool -> Term s PData
      toData b =
        punsafeBuiltin PLC.ConstrData
          # pif b (pconstantInteger 1) (pconstantInteger 0 :: Term s PInteger)
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

instance PIsData (PBuiltinPair (PAsData a) (PAsData b)) where
  pfromDataImpl x = f # x
    where
      f = phoistAcyclic $
        plam $ \pairDat -> pmatch (pasConstr # pforgetData pairDat) $ \(PBuiltinPair _ y) ->
          plet y $ \pd -> ppairDataBuiltin # punsafeCoerce (phead # pd) #$ punsafeCoerce (phead #$ ptail # pd)
  pdataImpl x = pupcast target
    where
      target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
      target = f # punsafeCoerce x
      f = phoistAcyclic $
        plam $ \pair -> pmatch pair $ \(PBuiltinPair x y) ->
          pconstrBuiltin # pconstantInteger 0 #$ pcons # x #$ pcons # y # pnil

instance PIsData (PBuiltinPair PData PData) where
  pfromDataImpl = pfromDataImpl @(PBuiltinPair PData PData) . punsafeCoerce
  pdataImpl = punsafeCoerce . pdataImpl @(PBuiltinPair PData PData)

-- This instance is kind of useless. There's no safe way to use 'pdata'.
instance PIsData (PBuiltinPair PInteger (PBuiltinList PData)) where
  pfromDataImpl x = pasConstr # pupcast x
  pdataImpl x' = pupcast $ plet x' $ \x ->
    pmatch x $ \(PBuiltinPair x1 x2) ->
      pconstrBuiltin # x1 # x2

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
  ( PInnermostIsData ('Just "PBuiltinList only implements PIsData when inner most type of its elements are PData") a
  , PSubtype PData a
  ) =>
  PIsData (PBuiltinList a)
  where
  pfromDataImpl x = punsafeCoerce $ pasList # pforgetData x
  pdataImpl x = plistData # punsafeCoerce x
