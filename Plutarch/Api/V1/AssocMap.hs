{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap (
  PMap (PMap),
  lookup,
  lookupData,
  singleton,
  singletonData,
) where

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as PlutusMap

import Plutarch.Builtin (PBuiltinMap, ppairDataBuiltin)
import Plutarch.Lift (
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeFrom)

import Prelude hiding (lookup)

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap k v)
  where
  type PLifted (PMap k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    flip traverse m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

-- | Look up the given key in a 'PMap'.
lookup :: (PIsData k, PIsData v) => Term (s :: S) (k :--> PMap k v :--> PMaybe v)
lookup = phoistAcyclic $
  plam $ \key ->
    lookupDataWith
      # (phoistAcyclic $ plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

-- | Look up the given key data in a 'PMap'.
lookupData :: (PIsData k, PIsData v, POrd k) => Term (s :: S) (PAsData k :--> PMap k v :--> PMaybe (PAsData v))
lookupData = lookupDataWith # (phoistAcyclic $ plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

-- | Look up the given key data in a 'PMap', applying the given function to the found key-value pair.
lookupDataWith ::
  (PIsData k, PIsData v) =>
  Term
    (s :: S)
    ( (PBuiltinPair (PAsData k) (PAsData v) :--> PMaybe x)
        :--> PAsData k
        :--> PMap k v
        :--> PMaybe x
    )
lookupDataWith = phoistAcyclic $
  phoistAcyclic $
    plam $ \unwrap key map ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== key)
              (unwrap # x)
              (self # xs)
        )
        (const $ pcon PNothing)
        # pto map

-- | Construct a singleton 'PMap' with the given key and value.
singleton :: (PIsData k, PIsData v) => Term (s :: S) (k :--> v :--> PMap k v)
singleton = phoistAcyclic $ plam $ \key value -> singletonData # pdata key # pdata value

-- | Construct a singleton 'PMap' with the given data-encoded key and value.
singletonData :: (PIsData k, PIsData v) => Term (s :: S) (PAsData k :--> PAsData v :--> PMap k v)
singletonData = phoistAcyclic $
  plam $ \key value -> punsafeFrom (pcons # (ppairDataBuiltin # key # value) # pnil)
