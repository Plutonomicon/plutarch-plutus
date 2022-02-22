{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap ( PMap (PMap)
                                , plookup
                                , plookup'
                                , (#!?)
                                , (#!) 
                                ) where

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as PlutusMap

import Plutarch.Builtin (PBuiltinMap)
import Plutarch.Lift (
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Prelude

import Plutarch.Util ( type (:$))

import Plutarch.List ( pfind )
import Plutarch.Maybe ( pfromMaybe )

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

instance
  ( Plutus.ToData (PLifted v)
  , Plutus.ToData (PLifted k)
  , Plutus.FromData (PLifted v)
  , Plutus.FromData (PLifted k)
  , PLift k
  , PLift v
  ) =>
  PUnsafeLiftDecl (PMap k v)
  where
  type PLifted (PMap k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PLifted (PConstanted k) ~ k
  , Plutus.ToData v
  , Plutus.FromData v
  , Plutus.ToData k
  , Plutus.FromData k
  , PConstant k
  , PLifted (PConstanted v) ~ v
  , Plutus.FromData v
  , Plutus.ToData v
  , PConstant v
  ) =>
  PConstant (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    flip traverse m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')


infixl 9 #!?, #!

{- | 
    operator for 'plookup'
-}
(#!?) ::
  forall a b s.
  ( PIsData a
  , PIsData b
  , PEq a
  ) =>
  Term s :$ PMap a b ->
  Term s a ->
  Term s :$ PMaybe b
m #!? k = plookup # k # m

{- | 
    operator for 'plookup''
-}
(#!) ::
  forall a b s.
  ( PIsData a
  , PIsData b
  , PEq a
  ) =>
  Term s :$ PMap a b ->
  Term s a ->
  Term s b
m #! k = plookup' # k # m

{- |
    traverses a PMap and returns a PJust if the specified 
    element is in a key in the map, PNothing otherwise
-}
plookup ::
  forall a b s.
  ( PIsData a
  , PIsData b
  , PEq a
  ) =>
  Term s :$ a :--> PMap a b :--> PMaybe b
plookup = phoistAcyclic $
  plam $ \key map -> unTermCont $ do
    let pred :: Term _ :$ PBuiltinPair (PAsData a) (PAsData b) :--> PBool
        pred = plam $ \tup -> (pfromData $ pfstBuiltin # tup) #== key
        tup :: Term _ :$ PMaybe (PBuiltinPair (PAsData a) (PAsData b))
        tup = pfind # pred # pto map
    res <- tcont $ pmatch tup
    pure $ case res of
      PNothing -> pcon PNothing
      PJust tup' -> pcon . PJust $ pfromData $ psndBuiltin # tup'

{- | 
    plookup but throws an error upon getting a PNothing 
-}
plookup' ::
  forall a b s.
  ( PIsData a
  , PIsData b
  , PEq a
  ) =>
  Term s :$ a :--> PMap a b :--> b
plookup' = plam $ \key map -> pfromMaybe #$ plookup # key # map


