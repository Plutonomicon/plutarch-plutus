{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap (
  PMap (PMap),
  KeyGuarantees (Unsorted, Sorted),

  -- * Creation
  pempty,
  psingleton,
  psingletonData,
  pinsert,
  pinsertData,
  pdelete,
  pfromAscList,
  passertSorted,
  pforgetSorted,

  -- * Lookups
  plookup,
  plookupData,
  pfindWithDefault,
  pfoldAt,
  pnull,

  -- * Folds
  pall,
  pany,

  -- * Filters and traversals
  pfilter,
  pmap,
  pmapData,
  pmapMaybe,
  pmapMaybeData,

  -- * Combining
  pdifference,
  punionWith,
  punionWithData,
) where

import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.AssocMap as PlutusMap
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Semigroup as PlutusTx

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
import qualified Plutarch.List as List
import Plutarch.Prelude hiding (pall, pany, pfilter, pmap, pnull, psingleton)
import Plutarch.Show (PShow)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

import Prelude hiding (all, any, filter, lookup, null)

data KeyGuarantees = Sorted | Unsorted

type role PMap nominal nominal nominal nominal
newtype PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)
instance DerivePlutusType (PMap keysort k v) where type DPTStrat _ = PlutusTypeNewtype

instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap 'Unsorted k v)
  where
  type PLifted (PMap 'Unsorted k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap 'Unsorted (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    flip traverse m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

-- | Tests whether the map is empty.
pnull :: Term s (PMap _ k v :--> PBool)
pnull = plam (\map -> List.pnull # pto map)

-- | Look up the given key in a 'PMap'.
plookup :: (PIsData k, PIsData v) => Term s (k :--> PMap _ k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \key ->
    plookupDataWith
      # (phoistAcyclic $ plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

-- | Look up the given key data in a 'PMap'.
plookupData :: (PIsData k, PIsData v) => Term s (PAsData k :--> PMap _ k v :--> PMaybe (PAsData v))
plookupData = plookupDataWith # (phoistAcyclic $ plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

-- | Look up the given key data in a 'PMap', applying the given function to the found key-value pair.
plookupDataWith ::
  (PIsData k, PIsData v) =>
  Term
    s
    ( (PBuiltinPair (PAsData k) (PAsData v) :--> PMaybe x)
        :--> PAsData k
        :--> PMap _ k v
        :--> PMaybe x
    )
plookupDataWith = phoistAcyclic $
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

-- | Look up the given key in a 'PMap', returning the default value if the key is absent.
pfindWithDefault :: (PIsData k, PIsData v) => Term s (v :--> k :--> PMap _ k v :--> v)
pfindWithDefault = phoistAcyclic $ plam $ \def key -> foldAtData # pdata key # def # plam pfromData

{- | Look up the given key in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
pfoldAt :: (PIsData k, PIsData v) => Term s (k :--> r :--> (PAsData v :--> r) :--> PMap _ k v :--> r)
pfoldAt = phoistAcyclic $
  plam $ \key -> foldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
foldAtData ::
  (PIsData k, PIsData v) =>
  Term s (PAsData k :--> r :--> (PAsData v :--> r) :--> PMap _ k v :--> r)
foldAtData = phoistAcyclic $
  plam $ \key def apply map ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (apply #$ psndBuiltin # x)
            (self # xs)
      )
      (const def)
      # pto map

-- | Insert a new key/value pair into the map, overiding the previous if any.
pinsert :: (POrd k, PIsData k, PIsData v) => Term s (k :--> v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # (plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #)) # key

-- | Insert a new data-encoded key/value pair into the map, overiding the previous if any.
pinsertData ::
  (POrd k, PIsData k) =>
  Term s (PAsData k :--> PAsData v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsertData = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # (plam (pcons # (ppairDataBuiltin # key # val) #)) # pfromData key

-- | Delete a key from the map.
pdelete :: (POrd k, PIsData k) => Term s (k :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pdelete = rebuildAtKey # plam id

-- | Rebuild the map at the given key.
rebuildAtKey ::
  (POrd k, PIsData k) =>
  Term
    s
    ( ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
          :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
      )
        :--> k
        :--> PMap g k v
        :--> PMap g k v
    )
rebuildAtKey = phoistAcyclic $
  plam $ \handler key map ->
    punsafeDowncast $
      precList
        ( \self x xs ->
            plet (pfromData $ pfstBuiltin # x) $ \k ->
              plam $ \prefix ->
                pif
                  (k #< key)
                  (self # xs #$ plam $ \suffix -> prefix #$ pcons # x # suffix)
                  ( pif
                      (k #== key)
                      (prefix #$ handler # xs)
                      (prefix #$ handler #$ pcons # x # xs)
                  )
        )
        (const $ plam (#$ handler # pnil))
        # pto map
        # plam id

-- | Construct an empty 'PMap'.
pempty :: Term s (PMap 'Sorted k v)
pempty = punsafeDowncast pnil

-- | Construct a singleton 'PMap' with the given key and value.
psingleton :: (PIsData k, PIsData v) => Term s (k :--> v :--> PMap 'Sorted k v)
psingleton = phoistAcyclic $ plam $ \key value -> psingletonData # pdata key # pdata value

-- | Construct a singleton 'PMap' with the given data-encoded key and value.
psingletonData :: Term s (PAsData k :--> PAsData v :--> PMap 'Sorted k v)
psingletonData = phoistAcyclic $
  plam $ \key value -> punsafeDowncast (pcons # (ppairDataBuiltin # key # value) # pnil)

-- | Construct a 'PMap' from a list of key-value pairs, sorted by ascending key data.
pfromAscList :: (POrd k, PIsData k, PIsData v) => Term s (PBuiltinMap k v :--> PMap 'Sorted k v)
pfromAscList = plam $ (passertSorted #) . pcon . PMap

-- | Assert the map is properly sorted.
passertSorted :: (POrd k, PIsData k, PIsData v) => Term s (PMap _ k v :--> PMap 'Sorted k v)
passertSorted = phoistAcyclic $
  plam $ \map ->
    precList
      ( \self x xs ->
          plet (pfromData $ pfstBuiltin # x) $ \k ->
            plam $ \badKey ->
              pif
                (badKey # k)
                (ptraceError "unsorted map")
                (self # xs # plam (#< k))
      )
      (const $ plam $ const map)
      # pto map
      # plam (const $ pcon PFalse)

-- | Forget the knowledge that keys were sorted.
pforgetSorted :: Term s (PMap 'Sorted k v) -> Term s (PMap g k v)
pforgetSorted v = punsafeDowncast (pto v)

instance
  (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) =>
  Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionWith # plam (<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) =>
  Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Semigroup (Term s v)) =>
  PlutusTx.Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionWith # plam (PlutusTx.<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Semigroup (Term s v)) =>
  PlutusTx.Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Group (Term s v)) =>
  PlutusTx.Group (Term s (PMap 'Sorted k v))
  where
  inv a = pmap # plam PlutusTx.inv # a

{- | Combine two 'PMap's applying the given function to any two values that
 share the same key.
-}
punionWith ::
  (POrd k, PIsData k, PIsData v) =>
  Term s ((v :--> v :--> v) :--> PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
punionWith = phoistAcyclic $
  plam $
    \combine -> punionWithData #$ plam $
      \x y -> pdata (combine # pfromData x # pfromData y)

data MapUnionCarrier k v s = MapUnionCarrier
  { merge :: Term s (PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
  , mergeInsert :: Term s (PBuiltinPair (PAsData k) (PAsData v) :--> PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance DerivePlutusType (MapUnionCarrier k v) where type DPTStrat _ = PlutusTypeScott

mapUnionCarrier :: (POrd k, PIsData k) => Term s ((PAsData v :--> PAsData v :--> PAsData v) :--> MapUnionCarrier k v :--> MapUnionCarrier k v)
mapUnionCarrier = phoistAcyclic $ plam \combine self ->
  let mergeInsert = (pmatch self \(MapUnionCarrier {mergeInsert}) -> mergeInsert)
      merge = (pmatch self \(MapUnionCarrier {merge}) -> merge)
   in pcon $
        MapUnionCarrier
          { merge = plam $ \xs ys -> pmatch xs $ \case
              PNil -> ys
              PCons x xs' -> mergeInsert # x # xs' # ys
          , mergeInsert = plam $ \x xs ys ->
              pmatch ys $ \case
                PNil -> pcons # x # xs
                PCons y1 ys' ->
                  plet y1 $ \y ->
                    plet (pfstBuiltin # x) $ \xk ->
                      plet (pfstBuiltin # y) $ \yk ->
                        pif
                          (xk #== yk)
                          ( pcons
                              # (ppairDataBuiltin # xk #$ combine # (psndBuiltin # x) # (psndBuiltin # y))
                              #$ merge
                              # xs
                              # ys'
                          )
                          ( pif
                              (pfromData xk #< pfromData yk)
                              ( pcons
                                  # x
                                  # (mergeInsert # y # ys' # xs)
                              )
                              ( pcons
                                  # y
                                  # (mergeInsert # x # xs # ys')
                              )
                          )
          }

mapUnion :: forall k v s. (POrd k, PIsData k) => Term s ((PAsData v :--> PAsData v :--> PAsData v) :--> MapUnionCarrier k v)
mapUnion = phoistAcyclic $ plam \combine -> (punsafeCoerce pfix) # (mapUnionCarrier # combine :: Term _ (MapUnionCarrier k v :--> MapUnionCarrier k v))

{- | Combine two 'PMap's applying the given function to any two data-encoded
 values that share the same key.
-}
punionWithData ::
  (POrd k, PIsData k) =>
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon $ PMap $ (pmatch (mapUnion # combine) \(MapUnionCarrier {merge}) -> merge) # pto x # pto y

-- | Difference of two maps. Return elements of the first map not existing in the second map.
pdifference ::
  (PIsData k, PIsData a, PIsData b) =>
  Term s (PMap g k a :--> PMap _ k b :--> PMap g k a)
pdifference = phoistAcyclic $
  plam $ \left right ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pfoldAt
                # (pfromData $ pfstBuiltin # x)
                # (pcons # x # xs')
                # (plam $ const xs')
                # right
        )
        (const pnil)
        # pto left

-- | Tests if all values in the map satisfy the given predicate.
pall :: PIsData v => Term s ((v :--> PBool) :--> PMap _ k v :--> PBool)
pall = phoistAcyclic $
  plam $ \pred map ->
    List.pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto map

-- | Tests if anu value in the map satisfies the given predicate.
pany :: PIsData v => Term s ((v :--> PBool) :--> PMap _ k v :--> PBool)
pany = phoistAcyclic $
  plam $ \pred map ->
    List.pany # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto map

-- | Filters the map so it contains only the values that satisfy the given predicate.
pfilter :: PIsData v => Term s ((v :--> PBool) :--> PMap g k v :--> PMap g k v)
pfilter = phoistAcyclic $
  plam $ \pred ->
    pmapMaybe #$ plam $ \v -> pif (pred # v) (pcon $ PJust v) (pcon PNothing)

-- | Maps and filters the map, much like 'Data.List.mapMaybe'.
pmapMaybe ::
  (PIsData a, PIsData b) =>
  Term s ((a :--> PMaybe b) :--> PMap g k a :--> PMap g k b)
pmapMaybe = phoistAcyclic $
  plam $ \f -> pmapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

pmapMaybeData ::
  Term s ((PAsData a :--> PMaybe (PAsData b)) :--> PMap g k a :--> PMap g k b)
pmapMaybeData = phoistAcyclic $
  plam $ \f map ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pmatch (f #$ psndBuiltin # x) $ \case
                PNothing -> xs'
                PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
        )
        (const pnil)
        # pto map

-- | Applies a function to every value in the map, much like 'Data.List.map'.
pmap ::
  (PIsData a, PIsData b) =>
  Term s ((a :--> b) :--> PMap g k a :--> PMap g k b)
pmap = phoistAcyclic $
  plam $ \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

pmapData ::
  Term s ((PAsData a :--> PAsData b) :--> PMap g k a :--> PMap g k b)
pmapData = phoistAcyclic $
  plam $ \f map ->
    pcon . PMap $
      precList
        ( \self x xs ->
            pcons
              # (ppairDataBuiltin # (pfstBuiltin # x) # (f #$ psndBuiltin # x))
              # (self # xs)
        )
        (const pnil)
        # pto map
