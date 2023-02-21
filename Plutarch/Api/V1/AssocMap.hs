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
  pzipWith,
  pzipWithData,
  pdifference,
  pintersectionWith,
  pintersectionWithData,

  -- * Partial order operations
  pcheckBinRel,
) where

import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusMap
import PlutusTx.Monoid qualified as PlutusTx
import PlutusTx.Semigroup qualified as PlutusTx

import Plutarch.Builtin (
  pasMap,
  pdataImpl,
  pforgetData,
  pfromDataImpl,
  ppairDataBuiltin,
 )
import Plutarch.Internal (punsafeBuiltin)
import Plutarch.Internal.Witness (witness)
import Plutarch.Lift (
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.List qualified as List
import Plutarch.Prelude hiding (pall, pany, pfilter, pmap, pnull, psingleton, pzipWith)
import Plutarch.Prelude qualified as PPrelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC

import Data.Foldable (foldl')
import GHC.Exts (IsList (Item, fromList, toList))
import Prelude hiding (all, any, filter, lookup, null)

import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)

import Data.Bifunctor (bimap)
import Plutarch.Bool (PSBool (PSFalse, PSTrue), pstrue)

data KeyGuarantees = Sorted | Unsorted

type PBuiltinListOfPairs k v = PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

type role PMap nominal nominal nominal nominal
newtype PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)
instance DerivePlutusType (PMap keysort k v) where type DPTStrat _ = PlutusTypeNewtype

instance PIsData (PMap keysort k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

instance PEq (PMap 'Sorted k v) where
  x #== y = peqViaData # x # y
    where
      peqViaData :: Term s (PMap 'Sorted k v :--> PMap 'Sorted k v :--> PBool)
      peqViaData = phoistAcyclic $ plam $ \m0 m1 -> pdata m0 #== pdata m1

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
  pconstantToRepr m = bimap Plutus.toData Plutus.toData <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    for m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

instance
  ( PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Unsorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Unsorted k v)) = Flip Term (PMap 'Unsorted k v)
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasMap # opq
    unwrapped <- tcont . plet $ List.pmap # ptryFromPair # opq'
    pure (punsafeCoerce opq, pcon . PMap $ unwrapped)
    where
      ptryFromPair :: Term s (PBuiltinPair PData PData :--> PBuiltinPair (PAsData k) (PAsData v))
      ptryFromPair = plam $ \p ->
        ppairDataBuiltin
          # ptryFrom (pfstBuiltin # p) fst
          # ptryFrom (psndBuiltin # p) fst

instance
  ( POrd k
  , PIsData k
  , PIsData v
  , PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Sorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Sorted k v)) = Flip Term (PMap 'Sorted k v)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PMap 'Unsorted k v)) opq
    unwrapped <- tcont $ plet . papp passertSorted . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | Tests whether the map is empty.
pnull :: Term s (PMap any k v :--> PBool)
pnull = plam (\m -> List.pnull # pto m)

-- | Look up the given key in a 'PMap'.
plookup :: (PIsData k, PIsData v) => Term s (k :--> PMap any k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \key ->
    plookupDataWith
      # phoistAcyclic (plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

-- | Look up the given key data in a 'PMap'.
plookupData :: Term s (PAsData k :--> PMap any k v :--> PMaybe (PAsData v))
plookupData = plookupDataWith # phoistAcyclic (plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

-- | Look up the given key data in a 'PMap', applying the given function to the found key-value pair.
plookupDataWith ::
  Term
    s
    ( (PBuiltinPair (PAsData k) (PAsData v) :--> PMaybe x)
        :--> PAsData k
        :--> PMap any k v
        :--> PMaybe x
    )
plookupDataWith = phoistAcyclic $
  plam $ \unwrap key m ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (unwrap # x)
            (self # xs)
      )
      (const $ pcon PNothing)
      # pto m

-- | Look up the given key in a 'PMap', returning the default value if the key is absent.
pfindWithDefault :: (PIsData k, PIsData v) => Term s (v :--> k :--> PMap any k v :--> v)
pfindWithDefault = phoistAcyclic $ plam $ \def key -> foldAtData # pdata key # def # plam pfromData

{- | Look up the given key in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
pfoldAt :: PIsData k => Term s (k :--> r :--> (PAsData v :--> r) :--> PMap any k v :--> r)
pfoldAt = phoistAcyclic $
  plam $
    \key -> foldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.
-}
foldAtData :: Term s (PAsData k :--> r :--> (PAsData v :--> r) :--> PMap any k v :--> r)
foldAtData = phoistAcyclic $
  plam $ \key def apply m ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (apply #$ psndBuiltin # x)
            (self # xs)
      )
      (const def)
      # pto m

-- | Insert a new key/value pair into the map, overiding the previous if any.
pinsert :: (POrd k, PIsData k, PIsData v) => Term s (k :--> v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #) # key

-- | Insert a new data-encoded key/value pair into the map, overiding the previous if any.
pinsertData ::
  (POrd k, PIsData k) =>
  Term s (PAsData k :--> PAsData v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsertData = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # key # val) #) # pfromData key

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
  plam $ \handler key m ->
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
        # pto m
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
  plam $
    \key value -> punsafeDowncast (pcons # (ppairDataBuiltin # key # value) # pnil)

-- | Construct a 'PMap' from a list of key-value pairs, sorted by ascending key data.
pfromAscList :: (POrd k, PIsData k, PIsData v) => Term s (PBuiltinListOfPairs k v :--> PMap 'Sorted k v)
pfromAscList = plam $ (passertSorted #) . pcon . PMap

-- | Assert the map is properly sorted.
passertSorted :: forall k v any s. (POrd k, PIsData k, PIsData v) => Term s (PMap any k v :--> PMap 'Sorted k v)
passertSorted =
  let _ = witness (Proxy :: Proxy (PIsData v))
   in phoistAcyclic $
        plam $ \m ->
          precList
            ( \self x xs ->
                plet (pfromData $ pfstBuiltin # x) $ \k ->
                  plam $ \badKey ->
                    pif
                      (badKey # k)
                      (ptraceError "unsorted map")
                      (self # xs # plam (#< k))
            )
            -- this is actually the empty map so we can
            -- safely assum that it is sorted
            (const . plam . const $ punsafeCoerce m)
            # pto m
            # plam (const $ pcon PFalse)

-- | Forget the knowledge that keys were sorted.
pforgetSorted :: Term s (PMap 'Sorted k v) -> Term s (PMap g k v)
pforgetSorted v = punsafeDowncast (pto v)

{- | Given a 'Foldable' of key-value pairs, construct an unsorted 'PMap'.
 Performs linearly with respect to its argument.

 = Note

 If there are duplicate keys in the input, the /last/ key will \'win\' in a
 lookup.
-}
punsortedMapFromFoldable ::
  forall (k :: PType) (v :: PType) (f :: Type -> Type) (s :: S).
  (Foldable f, PIsData k, PIsData v) =>
  f (Term s k, Term s v) ->
  Term s (PMap 'Unsorted k v)
punsortedMapFromFoldable = pcon . PMap . foldl' go (pcon PNil)
  where
    go ::
      forall (s' :: S).
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))) ->
      (Term s' k, Term s' v) ->
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v)))
    go acc (key, val) =
      pcon . PCons (ppairDataBuiltin # pdata key # pdata val) $ acc

{- | Given a 'Foldable' of (not necessarily sorted) key-value pairs, construct a
 'PMap' which is guaranteed sorted. Performs a linear number of ordered
 insertions with respect to the length of its argument.

 = Note

 If there are duplicate keys, only the /last/ key-value pair will remain in
 the result.
-}
psortedMapFromFoldable ::
  forall (k :: PType) (v :: PType) (f :: Type -> Type) (s :: S).
  (Foldable f, POrd k, PIsData k, PIsData v) =>
  f (Term s k, Term s v) ->
  Term s (PMap 'Sorted k v)
psortedMapFromFoldable = foldl' go pempty
  where
    go ::
      forall (s' :: S).
      Term s' (PMap 'Sorted k v) ->
      (Term s' k, Term s' v) ->
      Term s' (PMap 'Sorted k v)
    go acc (key, val) = pinsert # key # val # acc

instance (PIsData k, PIsData v, POrd k) => IsList (Term s (PMap 'Unsorted k v)) where
  type Item (Term s (PMap 'Unsorted k v)) = (Term s k, Term s v)
  fromList = punsortedMapFromFoldable
  toList = error "unimplemented"

instance (PIsData k, PIsData v, POrd k) => IsList (Term s (PMap 'Sorted k v)) where
  type Item (Term s (PMap 'Sorted k v)) = (Term s k, Term s v)
  fromList = psortedMapFromFoldable
  toList = error "unimplemented"

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). Monoid (Term s' v)) =>
  Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = pzipWith (Just mempty) (Just mempty) # plam (<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). Monoid (Term s' v)) =>
  Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). PlutusTx.Monoid (Term s' v)) =>
  PlutusTx.Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = pzipWith (Just PlutusTx.mempty) (Just PlutusTx.mempty) # plam (PlutusTx.<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). PlutusTx.Monoid (Term s' v)) =>
  PlutusTx.Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). PlutusTx.Group (Term s' v)) =>
  PlutusTx.Group (Term s (PMap 'Sorted k v))
  where
  inv a = pmap # plam PlutusTx.inv # a

-- | Apply given Plutarch fun with given reified (on Haskell-level) arg order.
applyOrder ::
  forall (s :: S) (a :: PType) (b :: PType).
  -- | The reified order to resolve first/second arg to left/right.
  PSBool s ->
  -- | A function that expects argument order 'left right'.
  Term s (a :--> a :--> b) ->
  -- | First arg.
  Term s a ->
  -- | Second arg.
  Term s a ->
  Term s b
applyOrder argOrder pfun a b =
  case argOrder of
    PSTrue -> (pfun # a # b)
    PSFalse -> (pfun # b # a)

zipMergeInsert ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  Maybe (ClosedTerm (PAsData v)) ->
  Maybe (ClosedTerm (PAsData v)) ->
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeInsert defLeft defRight =
  plam $ \combine -> pfix #$ plam $ \self argOrder x xs' ys ->
    pmatch ys $ \case
      PNil ->
        pmatch argOrder \argOrder' ->
          let defY = if (argOrder' == PSTrue) then defRight else defLeft
           in case defY of
                Just defY' ->
                  let handle = plam $ \x' -> applyOrder argOrder' combine x' defY'
                   in pto $ pmapData # handle # pcon (PMap $ pcons # x # xs')
                Nothing -> pcon PNil
      PCons y1 ys' -> unTermCont $ do
        y <- tcont $ plet y1
        xk <- tcont $ plet (pfstBuiltin # x)
        yk <- tcont $ plet (pfstBuiltin # y)
        pure $
          pif
            (xk #== yk)
            ( pcons
                # ( ppairDataBuiltin
                      # xk
                        #$ pmatch argOrder \argOrder' ->
                          applyOrder
                            argOrder'
                            combine
                            (psndBuiltin # x)
                            (psndBuiltin # y)
                  )
                  #$ pmatch argOrder \argOrder' ->
                    applyOrder
                      argOrder'
                      (zipMerge self combine defLeft)
                      xs'
                      ys'
            )
            ( pif
                (pfromData xk #< pfromData yk)
                ( pmatch argOrder $ \argOrder' ->
                    let -- default for the y-side
                        def = if (argOrder' == PSTrue) then defRight else defLeft
                        notOrder = pcon $ if (argOrder' == PSTrue) then PSFalse else PSTrue
                     in case def of
                          Just def' ->
                            pcons
                              # ( ppairDataBuiltin
                                    # xk
                                      #$ applyOrder argOrder' combine (psndBuiltin # x) def'
                                )
                              # (self # notOrder # y # ys' # xs')
                          Nothing -> self # notOrder # y # ys' # xs'
                )
                ( pmatch argOrder $ \argOrder' ->
                    let -- default for the x-side
                        def = if (argOrder' == PSTrue) then defLeft else defRight
                     in case def of
                          Just def' ->
                            pcons
                              # ( ppairDataBuiltin
                                    # yk
                                      #$ applyOrder argOrder' combine def' (psndBuiltin # y)
                                )
                              # (self # argOrder # x # xs' # ys')
                          Nothing -> self # argOrder # x # xs' # ys'
                )
            )

zipMerge ::
  forall (s :: S) (k :: PType) (v :: PType).
  Term
    s
    ( PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    ) ->
  Term s (PAsData v :--> PAsData v :--> PAsData v) ->
  Maybe (ClosedTerm (PAsData v)) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMerge mergeInsertRec combine defLeft = plam $ \ls rs -> pmatch ls $ \case
  PNil -> case defLeft of
    Just defLeft' -> pto $ pmapData # (combine # defLeft') # pcon (PMap rs)
    Nothing -> pcon PNil
  PCons l ls' -> mergeInsertRec # pstrue # l # ls' # rs

{- | Build the zip of two 'PMap's, merging data-encoded values that share
 the same key using the given function.

  Giving defaults is similar to SQL joins (though with the NULL replaced by the
  default):
  - 'Nothing' 'Nothing': inner join, a.k.a. intersection
  - 'Just' 'Nothing': left outer join
  - 'Nothing' 'Just': right outer join
  - 'Just' 'Just': full outer join, a.k.a. union
-}
pzipWithData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  -- | Default value for left side. If it is 'Nothing', occurrence of a given
  -- key on the left side is mandatory for it to occur in the output. In this
  -- case, key-value-pairs with a certain key that only occurs on the right side
  -- get dropped.
  Maybe (ClosedTerm (PAsData v)) ->
  -- | Default value for right side. If it is 'Nothing', occurrence of a given
  -- key on the right side is mandatory for it to occur in the output. In this
  -- case, key-value-pairs with a certain key that only occurs on the left side
  -- get dropped.
  Maybe (ClosedTerm (PAsData v)) ->
  -- | Value-merging-function, left side, right side.
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithData defLeft defRight = phoistAcyclic $
  plam $ \combine x y ->
    pcon $ PMap $ zipMerge (zipMergeInsert defLeft defRight # combine) combine defLeft # pto x # pto y

{- | Build the zip of two 'PMap's, merging values that share
 the same key using the given function.

  Giving defaults is similar to SQL joins (though with the NULL replaced by the
  default):
  - 'Nothing' 'Nothing': inner join, a.k.a. intersection
  - 'Just' 'Nothing': left outer join
  - 'Nothing' 'Just': right outer join
  - 'Just' 'Just': full outer join, a.k.a. union
-}
pzipWith ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k, PIsData v) =>
  -- | Default value for left side. If it is 'Nothing', occurrence of a given
  -- key on the left side is mandatory for it to occur in the output. In this
  -- case, key-value-pairs with a certain key that only occurs on the right side
  -- get dropped.
  Maybe (ClosedTerm v) ->
  -- | Default value for right side. If it is 'Nothing', occurrence of a given
  -- key on the right side is mandatory for it to occur in the output. In this
  -- case, key-value-pairs with a certain key that only occurs on the left side
  -- get dropped.
  Maybe (ClosedTerm v) ->
  -- | Value-merging-function, left side, right side.
  Term
    s
    ( (v :--> v :--> v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWith defLeft defRight = phoistAcyclic $
  plam $ \combine ls rs ->
    pzipWithData (pdata' <$> defLeft) (pdata' <$> defRight)
      # (plam $ \x y -> pdata (combine # pfromData x # pfromData y))
      # ls
      # rs
  where
    pdata' :: forall (a :: PType). PIsData a => ClosedTerm a -> ClosedTerm (PAsData a)
    pdata' a = pdata a

{- | Build the intersection of two 'PMap's, merging values that share the same key using the
given function.
-}
pintersectionWith ::
  (POrd k, PIsData k, PIsData v) =>
  Term s ((v :--> v :--> v) :--> PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pintersectionWith = pzipWith Nothing Nothing

{- | Build the intersection of two 'PMap's, merging data-encoded values that share the same key using the
given function.
-}
pintersectionWithData ::
  (POrd k, PIsData k) =>
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pintersectionWithData = pzipWithData Nothing Nothing

-- | Difference of two maps. Return elements of the first map not existing in the second map.
pdifference :: PIsData k => Term s (PMap g k a :--> PMap any k b :--> PMap g k a)
pdifference = phoistAcyclic $
  plam $ \left right ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pfoldAt
                # pfromData (pfstBuiltin # x)
                # (pcons # x # xs')
                # plam (const xs')
                # right
        )
        (const pnil)
        # pto left

-- | Tests if all values in the map satisfy the given predicate.
pall :: PIsData v => Term s ((v :--> PBool) :--> PMap any k v :--> PBool)
pall = phoistAcyclic $
  plam $ \pred m ->
    List.pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto m

-- | Tests if anu value in the map satisfies the given predicate.
pany :: PIsData v => Term s ((v :--> PBool) :--> PMap any k v :--> PBool)
pany = phoistAcyclic $
  plam $ \pred m ->
    List.pany # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto m

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
  plam $ \f m ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pmatch (f #$ psndBuiltin # x) $ \case
                PNothing -> xs'
                PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
        )
        (const pnil)
        # pto m

-- | Applies a function to every value in the map, much like 'Data.List.map'.
pmap ::
  (PIsData a, PIsData b) =>
  Term s ((a :--> b) :--> PMap g k a :--> PMap g k b)
pmap = phoistAcyclic $
  plam $
    \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

pmapData ::
  Term s ((PAsData a :--> PAsData b) :--> PMap g k a :--> PMap g k b)
pmapData = phoistAcyclic $
  plam $ \f m ->
    pcon . PMap $
      precList
        ( \self x xs ->
            pcons
              # (ppairDataBuiltin # (pfstBuiltin # x) # (f #$ psndBuiltin # x))
              # (self # xs)
        )
        (const pnil)
        # pto m

{- | Given a comparison function and a "zero" value, check whether a binary relation holds over
2 sorted 'PMap's.

This is primarily intended to be used with 'PValue'.
-}
pcheckBinRel ::
  forall k v s.
  (POrd k, PIsData k, PIsData v) =>
  Term
    s
    ( (v :--> v :--> PBool)
        :--> v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f z m1 m2 ->
    let inner = pfix #$ plam $ \self l1 l2 ->
          pelimList
            ( \x xs ->
                plet (pfromData $ psndBuiltin # x) $ \v1 ->
                  pelimList
                    ( \y ys -> unTermCont $ do
                        v2 <- tcont . plet . pfromData $ psndBuiltin # y
                        k1 <- tcont . plet . pfromData $ pfstBuiltin # x
                        k2 <- tcont . plet . pfromData $ pfstBuiltin # y
                        pure
                          $ pif
                            (k1 #== k2)
                            ( f
                                # v1
                                # v2
                                  #&& self
                                # xs
                                # ys
                            )
                          $ pif
                            (k1 #< k2)
                            (f # v1 # z #&& self # xs # l2)
                          $ f
                            # z
                            # v2
                              #&& self
                            # l1
                            # ys
                    )
                    ( f
                        # v1
                        # z
                          #&& PPrelude.pall
                        # plam (\p -> f # pfromData (psndBuiltin # p) # z)
                        # xs
                    )
                    l2
            )
            (PPrelude.pall # plam (\p -> f # z #$ pfromData $ psndBuiltin # p) # l2)
            l1
     in inner # pto m1 # pto m2
