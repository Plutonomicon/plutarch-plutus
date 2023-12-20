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
  BothPresentHandler,
  BothPresentHandler_ (..),
  BothPresentHandlerCommutative,
  BothPresentHandlerCommutative_ (..),
  OnePresentHandler,
  OnePresentHandler_ (..),
  MergeHandler,
  MergeHandler_ (..),
  MergeHandlerCommutative,
  MergeHandlerCommutative_ (..),
  SomeMergeHandler,
  SomeMergeHandler_ (..),
  Commutativity (..),
  pzipWith,
  pzipWithData,
  pzipWithDefault,
  pzipWithDataDefault,
  pzipWithDefaults,
  pzipWithDataDefaults,
  pintersectionWith,
  pintersectionWithData,
  pleftBiasedUnion,
  punionResolvingCollisionsWith,
  punionResolvingCollisionsWithData,
  pdifference,
  punsortedDifference,

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
import Plutarch.Bool (PSBool (PSFalse, PSTrue), psfalse, pstrue)

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

-- | Insert a new key/value pair into the map, overriding the previous if any.
pinsert :: (POrd k, PIsData k, PIsData v) => Term s (k :--> v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #) # key

-- | Insert a new data-encoded key/value pair into the map, overriding the previous if any.
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
            -- safely assume that it is sorted
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
  (POrd k, PIsData k, PIsData v, Semigroup (Term s v)) =>
  Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionResolvingCollisionsWith NonCommutative # plam (<>) # a # b

instance
  (POrd k, PIsData k, PIsData v, forall (s' :: S). Monoid (Term s' v)) =>
  Monoid (Term s (PMap 'Sorted k v))
  where
  mempty = pempty

instance
  (POrd k, PIsData k, PIsData v, PlutusTx.Semigroup (Term s v)) =>
  PlutusTx.Semigroup (Term s (PMap 'Sorted k v))
  where
  a <> b = punionResolvingCollisionsWith NonCommutative # plam (PlutusTx.<>) # a # b

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

type BothPresentHandler k v s = BothPresentHandler_ (->) (Term s k) (Term s v)

data BothPresentHandler_ f k v
  = DropBoth
  | -- | True ~ left, False ~ right
    PassArg Bool
  | HandleBoth (k `f` (v `f` (v `f` v)))

deriving stock instance (forall a b. Show (f a b)) => Show (BothPresentHandler_ f k v)

type BothPresentHandlerCommutative k v s = BothPresentHandlerCommutative_ (->) (Term s k) (Term s v)

data BothPresentHandlerCommutative_ f k v
  = DropBothCommutative
  | HandleBothCommutative (k `f` (v `f` (v `f` v)))

deriving stock instance (forall a b. Show (f a b)) => Show (BothPresentHandlerCommutative_ f k v)

type OnePresentHandler k v s = OnePresentHandler_ (->) (Term s k) (Term s v)

data OnePresentHandler_ f k v
  = DropOne
  | PassOne
  | HandleOne (k `f` (v `f` v))

deriving stock instance (forall a b. Show (f a b)) => Show (OnePresentHandler_ f k v)

type MergeHandler k v s = MergeHandler_ (->) (Term s k) (Term s v)

{- | Signals how to handle value merging for matching keys in 'pzipWith'.

 No restrictions on commutativity: Safe to use for both commutative and
 non-commutative merging operations.
-}
data MergeHandler_ f k v = MergeHandler
  { mhBoth :: BothPresentHandler_ f k v
  , mhLeft :: OnePresentHandler_ f k v
  , mhRight :: OnePresentHandler_ f k v
  }

deriving stock instance (forall a b. Show (f a b)) => Show (MergeHandler_ f k v)

type MergeHandlerCommutative k v s = MergeHandlerCommutative_ (->) (Term s k) (Term s v)

{- | Signals how to handle value merging for matching keys in 'pzipWith'.

 Safe to use for commutative merging operations only.
-}
data MergeHandlerCommutative_ f k v = MergeHandlerCommutative
  { mhcBoth :: BothPresentHandlerCommutative_ f k v
  , mhcOne :: OnePresentHandler_ f k v
  }

deriving stock instance (forall a b. Show (f a b)) => Show (MergeHandlerCommutative_ f k v)

type SomeMergeHandler k v s = SomeMergeHandler_ (->) (Term s k) (Term s v)

-- | Signals how to handle value merging for matching keys in 'pzipWith'.
data SomeMergeHandler_ f k v
  = SomeMergeHandlerCommutative (MergeHandlerCommutative_ f k v)
  | SomeMergeHandler (MergeHandler_ f k v)

deriving stock instance (forall a b. Show (f a b)) => Show (SomeMergeHandler_ f k v)

data Commutativity = Commutative | NonCommutative deriving stock (Eq, Ord, Show)

bothPresentOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  BothPresentHandler k v s ->
  BothPresentHandler (PAsData k) (PAsData v) s
bothPresentOnData = \case
  DropBoth -> DropBoth
  PassArg arg -> PassArg arg
  HandleBoth f -> HandleBoth \k x y -> pdata $ f (pfromData k) (pfromData x) (pfromData y)

bothPresentCommutativeOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  BothPresentHandlerCommutative k v s ->
  BothPresentHandlerCommutative (PAsData k) (PAsData v) s
bothPresentCommutativeOnData = \case
  DropBothCommutative -> DropBothCommutative
  HandleBothCommutative f ->
    HandleBothCommutative \k x y -> pdata $ f (pfromData k) (pfromData x) (pfromData y)

onePresentOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  OnePresentHandler k v s ->
  OnePresentHandler (PAsData k) (PAsData v) s
onePresentOnData = \case
  DropOne -> DropOne
  PassOne -> PassOne
  HandleOne f -> HandleOne \x y -> pdata $ f (pfromData x) (pfromData y)

mergeHandlerOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  MergeHandler k v s ->
  MergeHandler (PAsData k) (PAsData v) s
mergeHandlerOnData (MergeHandler bothPresent leftPresent rightPresent) =
  MergeHandler (bothPresentOnData bothPresent) (onePresentOnData leftPresent) (onePresentOnData rightPresent)

mergeHandlerCommutativeOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  MergeHandlerCommutative k v s ->
  MergeHandlerCommutative (PAsData k) (PAsData v) s
mergeHandlerCommutativeOnData (MergeHandlerCommutative bothPresent onePresent) =
  MergeHandlerCommutative (bothPresentCommutativeOnData bothPresent) (onePresentOnData onePresent)

branchOrder :: forall (s :: S) (a :: Type). PSBool s -> a -> a -> a
branchOrder PSTrue t _ = t
branchOrder PSFalse _ f = f

-- | Apply given Plutarch fun with given reified (on Haskell-level) arg order.
applyOrder ::
  forall (s :: S) (a :: PType) (b :: PType).
  -- | 'PSTrue' means first arg is left, second arg is right.
  PSBool s ->
  -- | A function that expects argument order 'left right'.
  Term s (a :--> a :--> b) ->
  -- | First arg.
  Term s a ->
  -- | Second arg.
  Term s a ->
  Term s b
applyOrder argOrder pfun a b = branchOrder argOrder (pfun # a # b) (pfun # b # a)

applyOrder' ::
  forall (s :: S) (a :: Type) (b :: Type).
  -- | 'PSTrue' means first arg is left, second arg is right.
  PSBool s ->
  -- | A function that expects argument order 'left right'.
  (a -> a -> b) ->
  -- | First arg.
  a ->
  -- | Second arg.
  a ->
  b
applyOrder' argOrder fun a b = branchOrder argOrder (fun a b) (fun b a)

zipMergeInsert ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  MergeHandler (PAsData k) (PAsData v) s ->
  -- | 'PSTrue' means first arg is left, second arg is right.
  -- The first list gets passed in deconstructed form as head and tail
  -- separately.
  Term
    s
    ( PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeInsert (MergeHandler bothPresent leftPresent rightPresent) = unTermCont $ do
  -- deduplicates all the zipMerge calls through plet, almost as good as hoisting
  zipMerge' <- tcont $ plet $ plam $ zipMerge rightPresent
  pure $ pfix #$ plam $ \self argOrder' x xs' ys -> unTermCont $ do
    let zipMergeRec = zipMerge' # self
        xs = pcons # x # xs'
        zipMergeOrdered xSide ySide =
          pmatch argOrder' $ \argOrder -> applyOrder argOrder zipMergeRec xSide ySide
    pure $ pmatch ys $ \case
      PNil -> pmatch argOrder' $ \argOrder ->
        -- picking handler for presence of x-side only
        case branchOrder argOrder leftPresent rightPresent of
          DropOne -> pcon PNil
          PassOne -> pcons # x # xs'
          HandleOne handler ->
            List.pmap
              # plam
                ( \pair ->
                    plet (pfstBuiltin # pair) \k ->
                      ppairDataBuiltin # k # handler k (psndBuiltin # pair)
                )
              # xs
      PCons y1 ys' -> unTermCont $ do
        y <- tcont $ plet y1
        xk <- tcont $ plet (pfstBuiltin # x)
        yk <- tcont $ plet (pfstBuiltin # y)
        pure $
          pif
            (xk #== yk)
            ( case bothPresent of
                DropBoth -> zipMergeOrdered xs' ys'
                PassArg passLeft -> pmatch argOrder' $ \argOrder ->
                  pcons
                    # (if passLeft == (argOrder == PSTrue) then x else y)
                    # applyOrder argOrder zipMergeRec xs' ys'
                HandleBoth merge -> pmatch argOrder' $ \argOrder ->
                  pcons
                    # ( ppairDataBuiltin
                          # xk
                          #$ applyOrder' argOrder (merge xk) (psndBuiltin # x) (psndBuiltin # y)
                      )
                    #$ applyOrder argOrder zipMergeRec xs' ys'
            )
            ( pif
                (pfromData xk #< pfromData yk)
                ( pmatch argOrder' $ \argOrder ->
                    -- picking handler for presence of only x-side
                    case branchOrder argOrder leftPresent rightPresent of
                      DropOne -> self # branchOrder argOrder psfalse pstrue # y # ys' # xs'
                      PassOne -> pcons # x # applyOrder argOrder zipMergeRec xs' ys
                      HandleOne handler ->
                        pcons
                          # (ppairDataBuiltin # xk # handler xk (psndBuiltin # x))
                          # (self # branchOrder argOrder psfalse pstrue # y # ys' # xs')
                )
                ( pmatch argOrder' $ \argOrder ->
                    -- picking handler for presence of only y-side
                    case branchOrder argOrder rightPresent leftPresent of
                      DropOne -> self # argOrder' # x # xs' # ys'
                      PassOne -> pcons # y # applyOrder argOrder zipMergeRec xs ys'
                      HandleOne handler ->
                        pcons
                          # (ppairDataBuiltin # yk # handler yk (psndBuiltin # y))
                          # (self # argOrder' # x # xs' # ys')
                )
            )

zipMerge ::
  forall (s :: S) (k :: PType) (v :: PType).
  OnePresentHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    ) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMerge rightPresent mergeInsertRec = plam $ \ls rs -> pmatch ls $ \case
  PNil ->
    case rightPresent of
      DropOne -> pcon PNil
      PassOne -> rs
      HandleOne handler ->
        List.pmap
          # plam
            ( \pair ->
                plet (pfstBuiltin # pair) \k ->
                  ppairDataBuiltin # k # handler k (psndBuiltin # pair)
            )
          # rs
  PCons l ls' -> mergeInsertRec # pstrue # l # ls' # rs

zipMergeInsertCommutative ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  MergeHandlerCommutative (PAsData k) (PAsData v) s ->
  -- | 'PSTrue' means first arg is left, second arg is right.
  -- The first list gets passed in deconstructed form as head and tail
  -- separately.
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeInsertCommutative (MergeHandlerCommutative bothPresent onePresent) = unTermCont $ do
  -- deduplicates all the zipMerge calls through plet, almost as good as hoisting
  zipMerge' <- tcont $ plet $ plam $ zipMergeCommutative onePresent
  pure $ pfix #$ plam $ \self x xs' ys -> unTermCont $ do
    let zipMergeRec = zipMerge' # self
        xs = pcons # x # xs'
    pure $ pmatch ys $ \case
      PNil ->
        -- picking handler for presence of x-side only
        case onePresent of
          DropOne -> pcon PNil
          PassOne -> pcons # x # xs'
          HandleOne handler ->
            List.pmap
              # plam
                ( \pair ->
                    plet (pfstBuiltin # pair) \k ->
                      ppairDataBuiltin # k # handler k (psndBuiltin # pair)
                )
              # xs
      PCons y1 ys' -> unTermCont $ do
        y <- tcont $ plet y1
        xk <- tcont $ plet (pfstBuiltin # x)
        yk <- tcont $ plet (pfstBuiltin # y)
        pure $
          pif
            (xk #== yk)
            ( case bothPresent of
                DropBothCommutative -> zipMergeRec # xs' # ys'
                HandleBothCommutative merge ->
                  pcons
                    # ( ppairDataBuiltin
                          # xk
                          #$ merge xk (psndBuiltin # x) (psndBuiltin # y)
                      )
                    #$ zipMergeRec
                    # xs'
                    # ys'
            )
            ( pif
                (pfromData xk #< pfromData yk)
                ( case onePresent of
                    DropOne -> self # y # ys' # xs'
                    PassOne -> pcons # x # (zipMergeRec # xs' # ys)
                    HandleOne handler ->
                      pcons
                        # (ppairDataBuiltin # xk # handler xk (psndBuiltin # x))
                        # (self # y # ys' # xs')
                )
                ( case onePresent of
                    DropOne -> self # x # xs' # ys'
                    PassOne -> pcons # y # (zipMergeRec # ys' # xs)
                    HandleOne handler ->
                      pcons
                        # (ppairDataBuiltin # yk # handler yk (psndBuiltin # y))
                        # (self # x # xs' # ys')
                )
            )

zipMergeCommutative ::
  forall (s :: S) (k :: PType) (v :: PType).
  OnePresentHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    ) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeCommutative onePresent mergeInsertRec = plam $ \ls rs -> pmatch ls $ \case
  PNil ->
    case onePresent of
      DropOne -> pcon PNil
      PassOne -> rs
      HandleOne handler ->
        List.pmap
          # plam
            ( \pair ->
                plet (pfstBuiltin # pair) \k ->
                  ppairDataBuiltin # k # handler k (psndBuiltin # pair)
            )
          # rs
  PCons l ls' -> mergeInsertRec # l # ls' # rs

{- | Zip two 'PMap's, using a 'SomeMergeHandler' to decide how to merge based on key
 presence on the left and right.

 Note that using a 'MergeHandlerCommutative' is less costly than a 'MergeHandler'.
-}
pzipWithData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  SomeMergeHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithData (SomeMergeHandler mh@(MergeHandler _ _ rightPresent)) =
  plam $ \x y ->
    pcon $ PMap $ zipMerge rightPresent (zipMergeInsert mh) # pto x # pto y
pzipWithData (SomeMergeHandlerCommutative mh@(MergeHandlerCommutative _ onePresent)) =
  plam $ \x y ->
    pcon $ PMap $ zipMergeCommutative onePresent (zipMergeInsertCommutative mh) # pto x # pto y

{- | Zip two 'PMap's, using a 'SomeMergeHandler' to decide how to merge based on key
 presence on the left and right.

 Note that using a 'MergeHandlerCommutative' is less costly than a 'MergeHandler'.
-}
pzipWith ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k, PIsData v) =>
  SomeMergeHandler k v s ->
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWith (SomeMergeHandler mh) =
  pzipWithData (SomeMergeHandler $ mergeHandlerOnData mh)
pzipWith (SomeMergeHandlerCommutative mhc) =
  pzipWithData (SomeMergeHandlerCommutative $ mergeHandlerCommutativeOnData mhc)

defaultMergeHandlerNonCommutative ::
  forall (s :: S) (v :: PType) (k :: PType).
  Term s v ->
  Term s v ->
  Term s (v :--> (v :--> v)) ->
  SomeMergeHandler k v s
defaultMergeHandlerNonCommutative defLeft defRight combine =
  SomeMergeHandler $
    MergeHandler
      (HandleBoth \_ vl vr -> combine # vl # vr)
      (HandleOne \_ vl -> combine # vl # defRight)
      (HandleOne \_ vr -> combine # defLeft # vr)

defaultMergeHandlerCommutative ::
  forall (s :: S) (v :: PType) (k :: PType).
  Term s v ->
  Term s (v :--> (v :--> v)) ->
  SomeMergeHandler k v s
defaultMergeHandlerCommutative def combine =
  SomeMergeHandlerCommutative $
    MergeHandlerCommutative
      (HandleBothCommutative \_ vl vr -> combine # vl # vr)
      (HandleOne \_ vl -> combine # vl # def)

{- | Zip two 'PMap's, using the given value merge function for key collisions,
 the commutativity of the merge function, and a default value that can stand in
 for both sides.

 Note that using 'Commutative' is less costly than 'NonCommutative'.
-}
pzipWithDataDefault ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  ClosedTerm (PAsData v) ->
  Commutativity ->
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithDataDefault def NonCommutative =
  phoistAcyclic $
    plam $
      pzipWithData . defaultMergeHandlerNonCommutative def def
pzipWithDataDefault def Commutative =
  phoistAcyclic $
    plam $
      pzipWithData . defaultMergeHandlerCommutative def

{- | Zip two 'PMap's, using the given value merge function for key collisions,
 the commutativity of the merge function, and a default value that can stand in
 for both sides.

 Note that using 'Commutative' is less costly than 'NonCommutative'.
-}
pzipWithDefault ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k, PIsData v) =>
  ClosedTerm v ->
  Commutativity ->
  Term
    s
    ( (v :--> v :--> v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithDefault def NonCommutative =
  phoistAcyclic $
    plam $
      pzipWith . defaultMergeHandlerNonCommutative def def
pzipWithDefault def Commutative =
  phoistAcyclic $
    plam $
      pzipWith . defaultMergeHandlerCommutative def

{- | Zip two 'PMap's, using the given potentially non-commutative value merge
 function for key collisions, and different values for the sides.

 Use `pzipWithDataDefault` if your merge function is commutative for better
 performance.
-}
pzipWithDataDefaults ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  ClosedTerm (PAsData v) ->
  ClosedTerm (PAsData v) ->
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithDataDefaults defLeft defRight =
  phoistAcyclic $
    plam $
      pzipWithData . defaultMergeHandlerNonCommutative defLeft defRight

{- | Zip two 'PMap's, using the given potentially non-commutative value merge
 function for key collisions, and different values for the sides.

 Use `pzipWithDefault` if your merge function is commutative for better
 performance.
-}
pzipWithDefaults ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k, PIsData v) =>
  ClosedTerm v ->
  ClosedTerm v ->
  Term
    s
    ( (v :--> v :--> v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithDefaults defLeft defRight =
  phoistAcyclic $
    plam $
      pzipWith . defaultMergeHandlerNonCommutative defLeft defRight

{- | Build the union of two 'PMap's. Take the value from the left argument for colliding keys.

 Prefer this over 'punionResolvingCollisionsWith NonCommutative # plam const'. It performs better.
-}
pleftBiasedUnion ::
  (POrd k, PIsData k, PIsData v) =>
  Term s (PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pleftBiasedUnion =
  phoistAcyclic $
    pzipWith $
      SomeMergeHandler $
        MergeHandler (PassArg True) PassOne PassOne

unionMergeHandler ::
  forall (s :: S) (k :: PType) (v :: PType).
  Commutativity ->
  Term s (v :--> (v :--> v)) ->
  SomeMergeHandler k v s
unionMergeHandler NonCommutative merge =
  SomeMergeHandler $ MergeHandler (HandleBoth \_ vl vr -> merge # vl # vr) PassOne PassOne
unionMergeHandler Commutative merge =
  SomeMergeHandlerCommutative $
    MergeHandlerCommutative (HandleBothCommutative \_ vl vr -> merge # vl # vr) PassOne

{- | Build the union of two 'PMap's, merging values that share the same key using the
given function.
-}
punionResolvingCollisionsWithData ::
  (POrd k, PIsData k) =>
  Commutativity ->
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
punionResolvingCollisionsWithData commutativity =
  phoistAcyclic $
    plam $
      pzipWithData . unionMergeHandler commutativity

{- | Build the union of two 'PMap's, merging values that share the same key using the
given function.
-}
punionResolvingCollisionsWith ::
  (POrd k, PIsData k, PIsData v) =>
  Commutativity ->
  Term s ((v :--> v :--> v) :--> PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
punionResolvingCollisionsWith commutativity =
  phoistAcyclic $
    plam $
      pzipWith . unionMergeHandler commutativity

intersectionMergeHandler ::
  forall (s :: S) (k :: PType) (v :: PType).
  Commutativity ->
  Term s (v :--> (v :--> v)) ->
  SomeMergeHandler k v s
intersectionMergeHandler NonCommutative merge =
  SomeMergeHandler $ MergeHandler (HandleBoth \_ vl vr -> merge # vl # vr) DropOne DropOne
intersectionMergeHandler Commutative merge =
  SomeMergeHandlerCommutative $
    MergeHandlerCommutative (HandleBothCommutative \_ vl vr -> merge # vl # vr) DropOne

{- | Build the intersection of two 'PMap's, merging data-encoded values that share the same key using the
given function.
-}
pintersectionWithData ::
  (POrd k, PIsData k) =>
  Commutativity ->
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pintersectionWithData commutativity =
  phoistAcyclic $
    plam $
      pzipWithData . intersectionMergeHandler commutativity

{- | Build the intersection of two 'PMap's, merging values that share the same key using the
given function.
-}
pintersectionWith ::
  (POrd k, PIsData k, PIsData v) =>
  Commutativity ->
  Term s ((v :--> v :--> v) :--> PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pintersectionWith commutativity =
  phoistAcyclic $
    plam $
      pzipWith . intersectionMergeHandler commutativity

-- | Difference of two maps. Return elements of the first map not existing in the second map.
pdifference ::
  (PIsData k, POrd k, PIsData v) =>
  Term s (PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pdifference =
  phoistAcyclic $
    pzipWith $
      SomeMergeHandler $
        MergeHandler DropBoth PassOne DropOne

{- | Difference of two maps. Return elements of the first map not existing in the second map.
 Warning: O(n^2).
-}
punsortedDifference :: PIsData k => Term s (PMap g k a :--> PMap any k b :--> PMap g k a)
punsortedDifference = phoistAcyclic $
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
