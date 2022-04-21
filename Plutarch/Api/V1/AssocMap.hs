{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap (
  PMap (PMap),

  -- * Creation
  empty,
  singleton,
  singletonData,
  insert,
  insertData,
  delete,

  -- * Lookups
  lookup,
  lookupData,
  findWithDefault,
  foldAt,
  null,

  -- * Folds
  all,

  -- * Filters and traversals
  filter,
  mapMaybe,
  mapMaybeData,
  mapEitherWithKey,
  mapEitherWithKeyData,

  -- * Combining
  difference,
  unionWith,
  unionWithData,
) where

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as PlutusMap

import Plutarch.Builtin (PBuiltinList (PCons, PNil), PBuiltinMap, ppairDataBuiltin)
import Plutarch.Either (peither)
import Plutarch.Lift (
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Prelude (
  DerivePNewtype (..),
  PAsData,
  PBool,
  PBuiltinPair,
  PCon (pcon),
  PConstantData,
  PEither (..),
  PEq ((#==)),
  PIsData (..),
  PLiftData,
  PListLike (pcons, pnil),
  PMatch (pmatch),
  PMaybe (..),
  POrd ((#<)),
  PPair (..),
  PType,
  PlutusType,
  S,
  Term,
  pall,
  pfstBuiltin,
  phoistAcyclic,
  pif,
  plam,
  plet,
  pnull,
  precList,
  psndBuiltin,
  pto,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Rec (ScottEncoded, ScottEncoding, field, letrec)
import Plutarch.Show (PShow)
import Plutarch.Unsafe (punsafeFrom)

import qualified Rank2.TH

import Prelude hiding (all, filter, lookup, null)

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData, PEq, PShow) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

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

null :: Term s (PMap k v :--> PBool)
null = plam (\map -> pnull # pto map)

-- | Look up the given key in a 'PMap'.
lookup :: (PIsData k, PIsData v) => Term (s :: S) (k :--> PMap k v :--> PMaybe v)
lookup = phoistAcyclic $
  plam $ \key ->
    lookupDataWith
      # (phoistAcyclic $ plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

-- | Look up the given key data in a 'PMap'.
lookupData :: (PIsData k, PIsData v) => Term (s :: S) (PAsData k :--> PMap k v :--> PMaybe (PAsData v))
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
findWithDefault :: (PIsData k, PIsData v) => Term (s :: S) (v :--> k :--> PMap k v :--> v)
findWithDefault = phoistAcyclic $ plam $ \def key -> foldAtData # pdata key # def # plam pfromData

{- | Look up the given key in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
foldAt :: (PIsData k, PIsData v) => Term (s :: S) (k :--> r :--> (PAsData v :--> r) :--> PMap k v :--> r)
foldAt = phoistAcyclic $
  plam $ \key -> foldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
foldAtData ::
  (PIsData k, PIsData v) =>
  Term (s :: S) (PAsData k :--> r :--> (PAsData v :--> r) :--> PMap k v :--> r)
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
insert :: (POrd k, PIsData k, PIsData v) => Term (s :: S) (k :--> v :--> PMap k v :--> PMap k v)
insert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # (plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #)) # key

-- | Insert a new data-encoded key/value pair into the map, overiding the previous if any.
insertData ::
  (POrd k, PIsData k, PIsData v) =>
  Term (s :: S) (PAsData k :--> PAsData v :--> PMap k v :--> PMap k v)
insertData = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # (plam (pcons # (ppairDataBuiltin # key # val) #)) # pfromData key

-- | Delete a key from the map.
delete :: (POrd k, PIsData k, PIsData v) => Term (s :: S) (k :--> PMap k v :--> PMap k v)
delete = rebuildAtKey # plam id

-- | Rebuild the map at the given key.
rebuildAtKey ::
  (POrd k, PIsData k) =>
  Term
    s
    ( ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
          :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
      )
        :--> k
        :--> PMap k v
        :--> PMap k v
    )
rebuildAtKey = phoistAcyclic $
  plam $ \handler key map ->
    punsafeFrom $
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
empty :: Term (s :: S) (PMap k v)
empty = punsafeFrom pnil

-- | Construct a singleton 'PMap' with the given key and value.
singleton :: (PIsData k, PIsData v) => Term (s :: S) (k :--> v :--> PMap k v)
singleton = phoistAcyclic $ plam $ \key value -> singletonData # pdata key # pdata value

-- | Construct a singleton 'PMap' with the given data-encoded key and value.
singletonData :: (PIsData k, PIsData v) => Term (s :: S) (PAsData k :--> PAsData v :--> PMap k v)
singletonData = phoistAcyclic $
  plam $ \key value -> punsafeFrom (pcons # (ppairDataBuiltin # key # value) # pnil)

data MapUnion k v f = MapUnion
  { merge :: f (PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
  , mergeInsert :: f (PBuiltinPair (PAsData k) (PAsData v) :--> PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
  }

type instance
  ScottEncoded (MapUnion k v) a =
    (PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
      :--> (PBuiltinPair (PAsData k) (PAsData v) :--> PBuiltinMap k v :--> PBuiltinMap k v :--> PBuiltinMap k v)
      :--> a

$(Rank2.TH.deriveAll ''MapUnion)

instance (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) => Semigroup (Term s (PMap k v)) where
  a <> b = unionWith # plam (<>) # a # b

{- | Combine two 'PMap's applying the given function to any two values that
 share the same key.
-}
unionWith ::
  (POrd k, PIsData k, PIsData v) =>
  Term (s :: S) ((v :--> v :--> v) :--> PMap k v :--> PMap k v :--> PMap k v)
unionWith = phoistAcyclic $
  plam $
    \combine -> unionWithData #$ plam $
      \x y -> pdata (combine # pfromData x # pfromData y)

{- | Combine two 'PMap's applying the given function to any two data-encoded
 values that share the same key.
-}
unionWithData ::
  (POrd k, PIsData k, PIsData v) =>
  Term
    (s :: S)
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap k v
        :--> PMap k v
        :--> PMap k v
    )
unionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon $ PMap $ mapUnion # combine # field merge # pto x # pto y

mapUnion ::
  (POrd k, PIsData k, PIsData v) =>
  Term (s :: S) ((PAsData v :--> PAsData v :--> PAsData v) :--> ScottEncoding (MapUnion k v) (a :: PType))
mapUnion = plam $ \combine ->
  letrec $ \MapUnion {merge, mergeInsert} ->
    MapUnion
      { merge = plam $ \xs ys -> pmatch xs $ \case
          PNil -> ys
          PCons x xs' -> mergeInsert # x # xs' # ys
      , mergeInsert = plam $ \x xs ys -> pmatch ys $ \case
          PNil -> pcons # x # xs
          PCons y ys' ->
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

-- | Difference of two maps. Return elements of the first map not existing in the second map.
difference ::
  (POrd k, PIsData k, PIsData a, PIsData b) =>
  Term (s :: S) (PMap k a :--> PMap k b :--> PMap k a)
difference = phoistAcyclic $
  plam $ \left right ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              foldAt
                # (pfromData $ pfstBuiltin # x)
                # (pcons # x # xs')
                # (plam $ const xs')
                # right
        )
        (const pnil)
        # pto left

all :: PIsData v => Term (s :: S) ((v :--> PBool) :--> PMap k v :--> PBool)
all = phoistAcyclic $
  plam $ \pred map ->
    pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto map

filter :: (PIsData k, PIsData a) => Term (s :: S) ((a :--> PBool) :--> PMap k a :--> PMap k a)
filter = phoistAcyclic $
  plam $ \pred ->
    mapMaybe #$ plam $ \v -> pif (pred # v) (pcon $ PJust v) (pcon PNothing)

mapMaybe ::
  (PIsData k, PIsData a, PIsData b) =>
  Term (s :: S) ((a :--> PMaybe b) :--> PMap k a :--> PMap k b)
mapMaybe = phoistAcyclic $
  plam $ \f -> mapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

mapMaybeData ::
  forall s k a b.
  (PIsData k, PIsData a, PIsData b) =>
  Term (s :: S) ((PAsData a :--> PMaybe (PAsData b)) :--> PMap k a :--> PMap k b)
mapMaybeData = phoistAcyclic $
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

-- | Map keys/values and separate the @Left@ and @Right@ results.
mapEitherWithKey ::
  (PIsData k, PIsData a, PIsData b, PIsData c) =>
  Term (s :: S) ((k :--> a :--> PEither b c) :--> PMap k a :--> PPair (PMap k b) (PMap k c))
mapEitherWithKey = phoistAcyclic $
  plam $ \f ->
    mapEitherWithKeyData #$ plam $ \k v ->
      bidata #$ f # pfromData k # pfromData v

bidata ::
  forall (s :: S) (a :: PType) (b :: PType).
  (PIsData a, PIsData b) =>
  Term (s :: S) (PEither a b :--> PEither (PAsData a) (PAsData b))
bidata = peither # plam (pcon . PLeft . pdata) # plam (pcon . PRight . pdata)

-- | Map data-encoded keys/values and separate the @Left@ and @Right@ results.
mapEitherWithKeyData ::
  (PIsData k, PIsData a, PIsData b, PIsData c) =>
  Term
    (s :: S)
    ( (PAsData k :--> PAsData a :--> PEither (PAsData b) (PAsData c))
        :--> PMap k a
        :--> PPair (PMap k b) (PMap k c)
    )
mapEitherWithKeyData = phoistAcyclic $
  plam $ \f map ->
    ( flip pmatch $ \(PPair lefts rights) ->
        pcon $ PPair (pcon $ PMap lefts) (pcon $ PMap rights)
    )
      $ precList
        ( \self x xs ->
            pmatch (self # xs) $ \(PPair lefts rights) ->
              plet (pfstBuiltin # x) $ \key ->
                peither
                  # (plam $ \l -> pcon $ PPair (pcons # (ppairDataBuiltin # key # l) # lefts) rights)
                  # (plam $ \r -> pcon $ PPair lefts (pcons # (ppairDataBuiltin # key # r) # rights))
                  # (f # key #$ psndBuiltin # x)
        )
        (const $ pcon $ PPair pnil pnil)
        # pto map
