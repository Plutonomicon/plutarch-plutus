{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is designed to be imported qualified, as many of its
identifiers clash with the Plutarch prelude.
-}
module Plutarch.LedgerApi.AssocMap (
  -- * Types
  PMap (..),
  KeyGuarantees (..),
  Commutativity (..),
  MergeHandler (..),
  BothPresentHandler (..),
  OnePresentHandler (..),

  -- * Functions

  -- ** Creation
  pempty,
  psingleton,
  psingletonData,
  punsortedMapFromFoldable,
  psortedMapFromFoldable,

  -- ** Transformation
  passertSorted,
  pforgetSorted,
  pmap,
  pmapData,
  pmapWithKey,
  pmapMaybe,
  pmapMaybeData,

  -- ** Relational lift
  pcheckBinRel,

  -- ** Comparison
  pkeysEqual,
  pkeysEqualUnsorted,

  -- ** Fold
  pall,
  pany,
  pfoldMapWithKey,
  pfoldlWithKey,

  -- ** Combination
  zipWithBuilder,
  zipWithDataBuilder,
  punionResolvingCollisionsWith,
  punionResolvingCollisionsWithData,
  pleftBiasedUnion,
  pdifference,
  pzipWithDefaults,
  pintersectionWith,
  pintersectionWithData,

  -- ** Query
  pnull,
  plookup,
  plookupData,
  plookupDataWith,
  pfindWithDefault,
  pfoldAt,
  pfoldAtData,
  ptryLookup,

  -- ** Modification
  pinsert,
  pdelete,
  pupdate,
  padjust,

  -- ** Key-value pair manipulation
  pkvPairKey,
  pkvPairValue,
  pkvPairLt,

  -- ** Conversion
  pkeys,
) where

import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (CouldNotDecodeData))
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.Internal.Witness (witness)
import Plutarch.LedgerApi.AssocMap.Zip (
  BothPresentHandler (..),
  MergeHandler (..),
  OnePresentHandler (..),
 )
import Plutarch.LedgerApi.AssocMap.Zip qualified as Zip
import Plutarch.Prelude hiding (pall, pany, pmap, pnull, psingleton, pzipWith)
import Plutarch.Prelude qualified as PPrelude
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusMap
import Prelude hiding (pred)

-- TODO: Rename this, because this is actually a _sorting_ guarantee!

-- | @since 2.0.0
data KeyGuarantees = Sorted | Unsorted

-- | @since 2.0.0
newtype PMap (keysort :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S)
  = PMap (Term s (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      SOP.Generic
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType (PMap keysort k v))

-- | @since 3.3.0
instance
  ( Plutus.ToData (AsHaskell k)
  , Plutus.ToData (AsHaskell v)
  , Plutus.FromData (AsHaskell k)
  , Plutus.FromData (AsHaskell v)
  ) =>
  PLiftable (PMap 'Unsorted k v)
  where
  type AsHaskell (PMap 'Unsorted k v) = PlutusMap.Map (AsHaskell k) (AsHaskell v)
  type PlutusRepr (PMap 'Unsorted k v) = [(Plutus.Data, Plutus.Data)]
  {-# INLINEABLE haskToRepr #-}
  haskToRepr = fmap (bimap Plutus.toData Plutus.toData) . PlutusMap.toList
  {-# INLINEABLE reprToHask #-}
  reprToHask x =
    PlutusMap.unsafeFromList
      <$> traverse
        ( \(k, v) ->
            (,)
              <$> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) k
              <*> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) v
        )
        x
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 2.0.0
instance PIsData (PMap keysort k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 2.0.0
instance PEq (PMap 'Sorted k v) where
  x #== y = peqViaData # x # y
    where
      peqViaData ::
        forall (s :: S).
        Term s (PMap 'Sorted k v :--> PMap 'Sorted k v :--> PBool)
      peqViaData = phoistAcyclic $ plam $ \m0 m1 -> pdata m0 #== pdata m1

-- | @since 3.4.0
instance
  ( PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Unsorted k v))
  where
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasMap # opq
    unwrapped <- tcont . plet $ PPrelude.pmap # ptryFromPair # opq'
    pure (pdata . pcon . PMap $ unwrapped, ())
    where
      ptryFromPair ::
        forall (s :: S).
        Term s (PBuiltinPair PData PData :--> PBuiltinPair (PAsData k) (PAsData v))
      ptryFromPair = plam $ \p ->
        ppairDataBuiltin
          # ptryFrom (pfstBuiltin # p) fst
          # ptryFrom (psndBuiltin # p) fst

-- | @since 3.4.0
instance
  ( POrd k
  , PIsData k
  , PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Sorted k v))
  where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PMap 'Unsorted k v)) opq
    unwrapped <- tcont $ plet . papp passertSorted . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since 2.0.0
data Commutativity = Commutative | NonCommutative
  deriving stock
    ( -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      Ord
    , -- | @since 2.0.0
      Show
    )

-- TODO: Rename this, because the name is confusing.

{- | Given a 'PMap' of uncertain order, yield a 'PMap' that is known to be
sorted.

@since 2.0.0
-}
passertSorted ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
  Term s (PMap any k v :--> PMap 'Sorted k v)
passertSorted =
  let _ = witness (Proxy :: Proxy (k ~ k))
   in phoistAcyclic $
        plam $ \m ->
          precList
            ( \self x xs ->
                plet (pfromData $ pfstBuiltin # x) $ \k ->
                  plam $ \badKey ->
                    pif
                      (badKey # k)
                      (ptraceInfoError "unsorted map")
                      (self # xs # plam (#< k))
            )
            -- this is actually the empty map so we can
            -- safely assume that it is sorted
            (const . plam . const $ punsafeCoerce m)
            # pto m
            # plam (const $ pcon PFalse)

{- | Construct an empty 'PMap'.

@since 2.0.0
-}
pempty :: Term s (PMap 'Sorted k v)
pempty = punsafeDowncast pnil

{- | Given a comparison function and a "zero" value, check whether a binary relation holds over
2 sorted 'PMap's.

= Important note

This is primarily intended to be used with 'PValue'. We assume that the comparison behaves like
a comparison would (thus, being at least a partial order, or possibly a total order or
equivalence), and that the starting value does not break it. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
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

{- | Verifies all values in the map satisfy the given predicate.

@since 2.0.0
-}
pall ::
  forall (any :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsData v =>
  Term s ((v :--> PBool) :--> PMap any k v :--> PBool)
pall = phoistAcyclic $
  plam $ \pred m ->
    PPrelude.pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto m

{- | Build the union of two 'PMap's, merging values that share the same key using the
given function.

@since 2.0.0
-}
punionResolvingCollisionsWith ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term
    s
    ( (v :--> v :--> v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
punionResolvingCollisionsWith =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.unionMergeHandler

{- | Build the union of two 'PMap's, merging values that share the same key
using the given function.

@since 2.1.1
-}
punionResolvingCollisionsWithData ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
punionResolvingCollisionsWithData =
  phoistAcyclic $
    plam $
      zipWithDataBuilder . Zip.unionMergeHandler

{- | Maps and filters the map, much like 'Data.PPrelude.mapMaybe'.

@since 2.0.0
-}
pmapMaybe ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsData a
  , PIsData b
  ) =>
  Term s ((a :--> PMaybe b) :--> PMap g k a :--> PMap g k b)
pmapMaybe = phoistAcyclic $
  plam $ \f -> pmapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

{- | As 'pmapMaybe', but over Data representation.

@since 2.0.0
-}
pmapMaybeData ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
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

{- | Tests whether the map is empty.

@since 2.0.0
-}
pnull ::
  forall (any :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  Term s (PMap any k v :--> PBool)
pnull = plam (\m -> PPrelude.pnull # pto m)

{- | Applies a function to every value in the map, much like 'Data.PPrelude.map'.

@since 2.0.0
-}
pmap ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsData a
  , PIsData b
  ) =>
  Term s ((a :--> b) :--> PMap g k a :--> PMap g k b)
pmap = phoistAcyclic $
  plam $
    \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

{- | As 'pmap', but gives key access as well.

@since 2.1.1
-}
pmapWithKey ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (keysort :: KeyGuarantees) (s :: S).
  ( PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term s ((k :--> a :--> b) :--> PMap keysort k a :--> PMap 'Unsorted k b)
pmapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    pmatch kvs $ \(PMap kvs') ->
      pcon . PMap $
        PPrelude.pmap
          # plam
            ( \x ->
                plet (pkvPairKey # x) $ \key ->
                  ppairDataBuiltin
                    # pdata key
                    #$ pdata
                    $ f # key # (pkvPairValue # x)
            )
          # kvs'

{- | As 'pmap', but over Data representations.

@since 2.0.0
-}
pmapData ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
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

{- | Look up the given key in a 'PMap'.

@since 2.1.1
-}
plookup ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s (k :--> PMap any k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \key ->
    plookupDataWith
      # phoistAcyclic (plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

{- | As 'plookup', but errors when the key is missing.

@since 2.1.1
-}
ptryLookup ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s (k :--> PMap keys k v :--> v)
ptryLookup = phoistAcyclic $
  plam $ \k kvs ->
    passertPJust
      # "plookupPartial: No value found for key."
      # (plookup # k # kvs)

{- | as 'plookup', except over Data representation.

@since 2.1.1
-}
plookupData ::
  Term s (PAsData k :--> PMap any k v :--> PMaybe (PAsData v))
plookupData = plookupDataWith # phoistAcyclic (plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

{- | Look up the given key data in a 'PMap', applying the given function to the found key-value pair.

@since 2.1.1
-}
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

{- | Construct a singleton 'PMap' with the given key and value.

@since 2.1.1
-}
psingleton ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s (k :--> v :--> PMap 'Sorted k v)
psingleton = phoistAcyclic $ plam $ \key value -> psingletonData # pdata key # pdata value

{- | Construct a singleton 'PMap' with the given data-encoded key and value.

@since 2.1.1
-}
psingletonData ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  Term s (PAsData k :--> PAsData v :--> PMap 'Sorted k v)
psingletonData = phoistAcyclic $
  plam $
    \key value -> punsafeDowncast (pcons # (ppairDataBuiltin # key # value) # pnil)

{- | Look up the given key in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.

 @since 2.1.1
-}
pfoldAt ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (r :: S -> Type) (s :: S).
  PIsData k =>
  Term s (k :--> r :--> (PAsData v :--> r) :--> PMap any k v :--> r)
pfoldAt = phoistAcyclic $
  plam $
    \key -> pfoldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
 absent or apply the argument function to the value data if present.

 @since 2.1.1
-}
pfoldAtData ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (r :: S -> Type) (s :: S).
  Term s (PAsData k :--> r :--> (PAsData v :--> r) :--> PMap any k v :--> r)
pfoldAtData = phoistAcyclic $
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

{- | Build the union of two 'PMap's. Take the value from the left argument for colliding keys.

 @since 2.1.1
-}
pleftBiasedUnion ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pleftBiasedUnion =
  phoistAcyclic $
    zipWithBuilder Zip.leftBiasedUnionMergeHandler

{- Difference of two maps. Return elements of the first map not existing in the second map.

@since 2.1.1
-}
pdifference ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pdifference =
  phoistAcyclic $
    zipWithBuilder Zip.differenceMergeHandler

{- | Tests if anu value in the map satisfies the given predicate.

@since 2.1.1
-}
pany ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (s :: S).
  PIsData v =>
  Term s ((v :--> PBool) :--> PMap any k v :--> PBool)
pany = phoistAcyclic $
  plam $ \pred m ->
    PPrelude.pany # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto m

{- | Look up the given key in a 'PMap', returning the default value if the key is absent.

@since 2.1.1
-}
pfindWithDefault ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s (v :--> k :--> PMap any k v :--> v)
pfindWithDefault = phoistAcyclic $ plam $ \def key -> pfoldAtData # pdata key # def # plam pfromData

{- | Insert a new key/value pair into the map, overriding the previous if any.

@since 2.1.1
-}
pinsert ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term s (k :--> v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsert = phoistAcyclic $
  plam $ \key val ->
    rebuildAtKey # plam (pcons # (ppairDataBuiltin # pdata key # pdata val) #) # key

{- | Delete a key from the map.

@since 2.1.1
-}
pdelete ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
  Term s (k :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pdelete = rebuildAtKey # plam id

{- | Build a function that zips two sorted 'PMap's together using a custom 'MergeHandler'.

The provided 'MergeHandler' determines how to merge entries based on whether a
key is present in the left map, the right map, or both.

NOTE: This function itself cannot be hoisted with 'phoistAcyclic' because it
depends on the supplied 'MergeHandler'. However, once you specialize it
by providing a specific merge handler, the resulting function /should/ be
hoisted if it will be reused, to avoid duplication.

@since 3.5.0
-}
zipWithBuilder ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  ( POrd k
  , PIsData k
  , PIsData a
  , PIsData b
  , PIsData c
  ) =>
  MergeHandler s k a b c ->
  Term
    s
    ( PMap 'Sorted k a
        :--> PMap 'Sorted k b
        :--> PMap 'Sorted k c
    )
zipWithBuilder mergeHandler =
  zipWithDataBuilder (Zip.mergeHandlerOnData mergeHandler)

{- | Build a function that zips two sorted 'PMap's together using a custom 'MergeHandler'.

The provided 'MergeHandler' determines how to merge entries based on whether a
key is present in the left map, the right map, or both.

Unlike 'zipWithBuilder', 'zipWithDataBuilder' operates on values wrapped in
'PAsData' (typed BuiltinData).

NOTE: This function itself cannot be hoisted with 'phoistAcyclic' because it
depends on the supplied 'MergeHandler'. However, once you specialize it
by providing a specific merge handler, the resulting function /should/ be
hoisted if it will be reused, to avoid duplication.

@since 3.5.0
-}
zipWithDataBuilder ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  ( POrd k
  , PIsData k
  ) =>
  MergeHandler s (PAsData k) (PAsData a) (PAsData b) (PAsData c) ->
  Term
    s
    ( PMap 'Sorted k a
        :--> PMap 'Sorted k b
        :--> PMap 'Sorted k c
    )
zipWithDataBuilder mergeHandler =
  plam $ \mapL mapR ->
    pcon (PMap $ Zip.zipWorker mergeHandler # pto mapL # pto mapR)

{- | Zip two 'PMap's, using the given value merge function for key collisions,
    and different values for the sides.

 @since 2.1.1
-}
pzipWithDefaults ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData a
  , PIsData b
  , PIsData c
  ) =>
  (forall (s' :: S). Term s' a) ->
  (forall (s' :: S). Term s' b) ->
  Term
    s
    ( (a :--> b :--> c)
        :--> PMap 'Sorted k a
        :--> PMap 'Sorted k b
        :--> PMap 'Sorted k c
    )
pzipWithDefaults defLeft defRight =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.defaultMergeHandler defLeft defRight

{- | Build the intersection of two 'PMap's, merging values that share the same
key using the given function.

@since 2.1.1
-}
pintersectionWith ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData a
  , PIsData b
  , PIsData c
  ) =>
  Term
    s
    ( (a :--> b :--> c)
        :--> PMap 'Sorted k a
        :--> PMap 'Sorted k b
        :--> PMap 'Sorted k c
    )
pintersectionWith =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.intersectionMergeHandler

{- | Build the intersection of two 'PMap's, merging data-encoded values that
share the same key using the given function.

@since 2.1.1
-}
pintersectionWithData ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
  Term
    s
    ( (PAsData a :--> PAsData b :--> PAsData c)
        :--> PMap 'Sorted k a
        :--> PMap 'Sorted k b
        :--> PMap 'Sorted k c
    )
pintersectionWithData =
  phoistAcyclic $
    plam $
      zipWithDataBuilder . Zip.intersectionMergeHandler

{- | Forget the knowledge that keys were sorted.

@since 2.1.1
-}
pforgetSorted ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  Term s (PMap 'Sorted k v) ->
  Term s (PMap g k v)
pforgetSorted v = punsafeDowncast (pto v)

{- | Gives 'PTrue' if both argument 'PMap's contain mappings for exactly the
 same set of keys. Requires a number of equality comparisons between keys
 proportional to the length of the shorter argument.

 @since 2.1.1
-}
pkeysEqual ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PEq k
  , PIsData k
  ) =>
  Term s (PMap 'Sorted k a :--> PMap 'Sorted k b :--> PBool)
pkeysEqual = phoistAcyclic $
  plam $ \kvs kvs' ->
    pmatch kvs $ \(PMap ell) ->
      pmatch kvs' $ \(PMap ell') ->
        go # ell # ell'
  where
    go ::
      forall (s' :: S).
      Term
        s'
        ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData a))
            :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData b))
            :--> PBool
        )
    go = phoistAcyclic $
      pfix #$ plam $ \self ell ell' ->
        pmatch (PPrelude.puncons # ell) $ \case
          PNothing -> pmatch (PPrelude.puncons # ell') $ \case
            PNothing -> pcon PTrue -- no mismatches found
            PJust _ -> pcon PFalse -- one argument too long
          PJust kv -> pmatch (PPrelude.puncons # ell') $ \case
            PNothing -> pcon PFalse -- one argument too long
            PJust kv' -> pmatch kv $ \(PPair h t) ->
              pmatch kv' $ \(PPair h' t') ->
                pif
                  ((pkvPairKey # h) #== (pkvPairKey # h'))
                  (self # t # t') -- continue
                  (pcon PFalse) -- key mismatch

{- | As 'pkeysEqual', but requires only 'PEq' constraints for the keys, and
 works for 'Unsorted' 'PMap's. This requires a number of equality comparisons
 between keys proportional to the product of the lengths of both arguments:
 that is, this function is quadratic.

 @since 2.1.1
-}
pkeysEqualUnsorted ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term s (PMap 'Unsorted k a :--> PMap 'Unsorted k b :--> PBool)
pkeysEqualUnsorted = phoistAcyclic $
  plam $ \kvs kvs' ->
    pmatch kvs $ \(PMap ell) ->
      pmatch kvs' $ \(PMap ell') ->
        go # kvs # kvs' # ell # ell'
  where
    go ::
      forall (s' :: S).
      Term
        s'
        ( PMap 'Unsorted k a
            :--> PMap 'Unsorted k b
            :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData a))
            :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData b))
            :--> PBool
        )
    go = phoistAcyclic $
      pfix #$ plam $ \self kvs kvs' ell ell' ->
        pmatch (PPrelude.puncons # ell) $ \case
          PNothing -> pmatch (PPrelude.puncons # ell') $ \case
            -- We reached the end, so we match
            PNothing -> pcon PTrue
            PJust ht' -> pmatch ht' $ \(PPair h' t') ->
              pmatch (plookup # (pkvPairKey # h') # kvs) $ \case
                -- We mismatch, so fail
                PNothing -> pcon PFalse
                -- We match, so continue
                PJust _ -> self # kvs # kvs' # ell # t'
          PJust ht -> pmatch ht $ \(PPair h t) ->
            pmatch (PPrelude.puncons # ell') $ \case
              PNothing -> pmatch (plookup # (pkvPairKey # h) # kvs') $ \case
                -- We mismatch, so fail
                PNothing -> pcon PFalse
                -- We match, so continue
                PJust _ -> self # kvs # kvs' # t # ell'
              -- To save some effort, we try both matches in one shot
              PJust ht' -> pmatch ht' $ \(PPair h' t') ->
                pmatch (plookup # (pkvPairKey # h) # kvs') $ \case
                  -- We mismatch, so fail
                  PNothing -> pcon PFalse
                  -- Try the other direction
                  PJust _ -> pmatch (plookup # (pkvPairKey # h') # kvs) $ \case
                    -- We mismatch, so fail
                    PNothing -> pcon PFalse
                    -- Both succeeded, so continue on tails
                    PJust _ -> self # kvs # kvs' # t # t'

{- | Get the key of a key-value pair.

@since 2.1.1
-}
pkvPairKey ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsData k =>
  Term s (PBuiltinPair (PAsData k) (PAsData v) :--> k)
pkvPairKey = phoistAcyclic $ plam $ \kv -> pfromData (pfstBuiltin # kv)

{- | Get the value of a key-value pair.

@since 2.1.1
-}
pkvPairValue ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsData v =>
  Term s (PBuiltinPair (PAsData k) (PAsData v) :--> v)
pkvPairValue = phoistAcyclic $ plam $ \kv -> pfromData (psndBuiltin # kv)

-- | @since 2.1.1
punsortedMapFromFoldable ::
  forall (k :: S -> Type) (v :: S -> Type) (f :: Type -> Type) (s :: S).
  ( Foldable f
  , PIsData k
  , PIsData v
  ) =>
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

-- | @since 2.1.1
psortedMapFromFoldable ::
  forall (k :: S -> Type) (v :: S -> Type) (f :: Type -> Type) (s :: S).
  ( Foldable f
  , POrd k
  , PIsData k
  , PIsData v
  ) =>
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

{- | Given an \'updater\' and a key, if the key exists in the 'PMap', apply the
 \'updater\' to it, otherwise do nothing. If the \'updater\' produces
 'PNothing', the value is deleted; otherwise, it is modified to the result.

 Performance will be equivalent to a lookup followed by an insert (or delete),
 as well as the cost of calling the \'updater\'.

 @since 2.1.1
-}
pupdate ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData v
  , POrd k
  ) =>
  Term s ((v :--> PMaybe v) :--> k :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pupdate = phoistAcyclic $
  plam $ \updater key kvs -> pmatch kvs $ \(PMap kvs') ->
    pcon . PMap $
      ( precList
          ( \self x xs ->
              plet (pfromData $ pfstBuiltin # x) $ \k ->
                pif
                  (k #== key)
                  ( pmatch (updater # pfromData (psndBuiltin # x)) $ \case
                      PNothing -> self # xs
                      PJust v -> pcons # (ppairDataBuiltin # pdata k # pdata v) #$ self # xs
                  )
                  (pif (key #<= k) (pcons # x # xs) (pcons # x #$ self # xs))
          )
          (const pnil)
          # kvs'
      )

{- | If a value exists at the specified key, apply the function argument to it;
 otherwise, do nothing.

 @since 2.1.1
-}
padjust ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PEq k
  , PIsData v
  ) =>
  Term s ((v :--> v) :--> k :--> PMap 'Unsorted k v :--> PMap 'Unsorted k v)
padjust = phoistAcyclic $
  plam $ \f key kvs ->
    pmapWithKey # plam (\k' a -> pif (k' #== key) (f # a) a) # kvs

{- | Left-associative fold of a 'PMap' with keys. Keys and values will be
 presented in key order.

 @since 2.1.1
-}
pfoldlWithKey ::
  forall (a :: S -> Type) (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s ((a :--> k :--> v :--> a) :--> a :--> PMap 'Sorted k v :--> a)
pfoldlWithKey = phoistAcyclic $
  plam $ \f x kvs -> pmatch kvs $ \case
    PMap kvs' ->
      pfoldl # plam (\acc kv -> f # acc # (pkvPairKey # kv) # (pkvPairValue # kv)) # x # kvs'

{- | Project all key-value pairs into a 'Monoid', then combine. Keys and values
 will be presented in key order.

 @since 2.1.1
-}
pfoldMapWithKey ::
  forall (m :: S -> Type) (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData v
  , forall (s' :: S). Monoid (Term s' m)
  ) =>
  Term s ((k :--> v :--> m) :--> PMap 'Sorted k v :--> m)
pfoldMapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    pfoldlWithKey # plam (\acc k v -> acc <> (f # k # v)) # mempty # kvs

{- | Get a list-like structure full of the keys of the argument 'PMap'. If the
 'PMap' is 'Sorted', the keys will maintain that order, and will be unique;
 otherwise, the order is unspecified, and duplicates may exist.

 = Note

 You will need to specify what manner of list-like structure you want; we have
 arranged the type signature to make specifying this easy with
 @TypeApplications@.

 @since 2.1.1
-}
pkeys ::
  forall
    (ell :: (S -> Type) -> S -> Type)
    (k :: S -> Type)
    (v :: S -> Type)
    (keys :: KeyGuarantees)
    (s :: S).
  ( PListLike ell
  , PElemConstraint ell (PAsData k)
  ) =>
  Term s (PMap keys k v :--> ell (PAsData k))
pkeys = phoistAcyclic $
  plam $ \kvs -> pmatch kvs $ \(PMap kvs') ->
    precList go (const pnil) # kvs'
  where
    go ::
      forall (s' :: S).
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v)) :--> ell (PAsData k)) ->
      Term s' (PBuiltinPair (PAsData k) (PAsData v)) ->
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))) ->
      Term s' (ell (PAsData k))
    go self kv acc = pcons # (pfstBuiltin # kv) # (self # acc)

{- | Compare two key-value pairs by their keys. Gives 'PTrue' if the key of the
 first argument pair is less than the key of the second argument pair.

 @since 2.1.1
-}
pkvPairLt ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  (PIsData k, POrd k) =>
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBool
    )
pkvPairLt = phoistAcyclic $
  plam $ \kv kv' ->
    (pkvPairKey # kv) #< (pkvPairKey # kv')

-- Helpers

-- | Rebuild the map at the given key.
rebuildAtKey ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
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

-- We have to clone this in here or we get a dependency cycle
passertPJust :: forall (a :: S -> Type) (s :: S). Term s (PString :--> PMaybe a :--> a)
passertPJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceInfoError emsg
