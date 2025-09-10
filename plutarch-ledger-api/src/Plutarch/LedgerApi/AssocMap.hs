{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Plutarch.LedgerApi.AssocMap (
  -- * Types
  PSortedMap,
  PUnsortedMap (..),
  MergeHandler (..),
  BothPresentHandler (..),
  OnePresentHandler (..),

  -- * Classes
  PIsAssocMap (..),

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
  pmapMaybeWithKey,
  pmapMaybeDataWithKey,

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
  punionWith,
  punionWithData,
  pleftBiasedUnion,
  pdifference,
  pdifferenceWith,
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

----------------------------------------------------------------------
-- Classes

class PIsAssocMap t where
  ptoUnsortedMap :: Term s (t k v) -> Term s (PUnsortedMap k v)
  punsafeFromUnsortedMap :: Term s (PUnsortedMap k v) -> Term s (t k v)

instance PIsAssocMap PUnsortedMap where
  ptoUnsortedMap = id
  punsafeFromUnsortedMap = id

instance PIsAssocMap PSortedMap where
  ptoUnsortedMap = pto
  punsafeFromUnsortedMap = punsafeDowncast

----------------------------------------------------------------------
-- Sorted Map

-- | @since 3.5.0
newtype PUnsortedMap (k :: S -> Type) (v :: S -> Type) (s :: S)
  = PUnsortedMap (Term s (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))))
  deriving stock
    ( -- | @since 3.5.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.5.0
      SOP.Generic
    , -- | @since 3.5.0
      PShow
    )
  deriving
    ( -- | @since 3.5.0
      PlutusType
    )
    via (DeriveNewtypePlutusType (PUnsortedMap k v))

-- | @since 3.5.0
instance PIsData (PUnsortedMap k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 3.5.0
instance
  ( PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PUnsortedMap k v))
  where
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasMap # opq
    unwrapped <- tcont . plet $ PPrelude.pmap # ptryFromPair # opq'
    pure (pdata . pcon . PUnsortedMap $ unwrapped, ())
    where
      ptryFromPair ::
        forall (s :: S).
        Term s (PBuiltinPair PData PData :--> PBuiltinPair (PAsData k) (PAsData v))
      ptryFromPair = plam $ \p ->
        ppairDataBuiltin
          # ptryFrom (pfstBuiltin # p) fst
          # ptryFrom (psndBuiltin # p) fst

-- | @since 3.5.0
instance
  ( Plutus.ToData (AsHaskell k)
  , Plutus.ToData (AsHaskell v)
  , Plutus.FromData (AsHaskell k)
  , Plutus.FromData (AsHaskell v)
  ) =>
  PLiftable (PUnsortedMap k v)
  where
  type AsHaskell (PUnsortedMap k v) = PlutusMap.Map (AsHaskell k) (AsHaskell v)
  type PlutusRepr (PUnsortedMap k v) = [(Plutus.Data, Plutus.Data)]
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

----------------------------------------------------------------------
-- Unsorted Map

newtype PSortedMap (k :: S -> Type) (v :: S -> Type) (s :: S)
  = PSortedMap (Term s (PUnsortedMap k v))
  deriving stock
    ( -- | @since 3.5.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.5.0
      SOP.Generic
    , -- | @since 3.5.0
      PShow
    )
  deriving
    ( -- | @since 3.5.0
      PlutusType
    )
    via (DeriveNewtypePlutusType (PSortedMap k v))

-- | @since 3.5.0
instance PIsData (PSortedMap k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 3.5.0
instance PEq (PSortedMap k v) where
  x #== y = peqViaData # x # y
    where
      peqViaData ::
        forall (s :: S).
        Term s (PSortedMap k v :--> PSortedMap k v :--> PBool)
      peqViaData = phoistAcyclic $ plam $ \m0 m1 -> pdata m0 #== pdata m1

-- | @since 3.5.0
instance
  ( POrd k
  , PIsData k
  , PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PSortedMap k v))
  where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PUnsortedMap k v)) opq
    unwrapped <- tcont $ plet . papp passertSorted . pfromData $ opq'
    pure (pdata unwrapped, ())

----------------------------------------------------------------------
-- Creation

{- | Construct an empty map.

@since 2.0.0
-}
pempty ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s (t k v)
pempty = punsafeFromUnsortedMap $ punsafeDowncast pnil

{- | Construct a singleton map with the given key and value.

@since 2.1.1
-}
psingleton ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData v
  ) =>
  Term s (k :--> v :--> t k v)
psingleton =
  phoistAcyclic $
    plam $ \key value ->
      psingletonData # pdata key # pdata value

{- | Construct a singleton map with the given data-encoded key and value.

@since 2.1.1
-}
psingletonData ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s (PAsData k :--> PAsData v :--> t k v)
psingletonData =
  phoistAcyclic $
    plam $ \key value ->
      punsafeFromUnsortedMap $
        punsafeDowncast $
          pcons
            # (ppairDataBuiltin # key # value)
            # pnil

-- | @since 2.1.1
punsortedMapFromFoldable ::
  forall (k :: S -> Type) (v :: S -> Type) (f :: Type -> Type) (s :: S).
  ( Foldable f
  , PIsData k
  , PIsData v
  ) =>
  f (Term s k, Term s v) ->
  Term s (PUnsortedMap k v)
punsortedMapFromFoldable = pcon . PUnsortedMap . foldl' go (pcon PNil)
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
  Term s (PSortedMap k v)
psortedMapFromFoldable = foldl' go pempty
  where
    go ::
      forall (s' :: S).
      Term s' (PSortedMap k v) ->
      (Term s' k, Term s' v) ->
      Term s' (PSortedMap k v)
    go acc (key, val) = pinsert # key # val # acc

----------------------------------------------------------------------
-- Transformation

-- TODO: Rename this, because the name is confusing.

{- | Attempt to promote `PUnsortedMap` to `PSortedMap`. This function checks
that the keys in the input map are in ascending order and fails with an error if
they are not.

@since 2.0.0
-}
passertSorted ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , POrd k
  , PIsData k
  ) =>
  Term s (t k v :--> PSortedMap k v)
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
            (const . plam . const . punsafeDowncast $ ptoUnsortedMap m)
            # pto (ptoUnsortedMap m)
            # plam (const $ pcon PFalse)

{- | Forget the knowledge that keys were sorted.

@since 2.1.1
-}
pforgetSorted ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  Term s (PSortedMap k v) ->
  Term s (PUnsortedMap k v)
pforgetSorted = pto -- TODO: should we deprecate this function?

{- | Applies a function to every value in the map, much like 'Data.PPrelude.map'.

@since 2.0.0
-}
pmap ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData a
  , PIsData b
  ) =>
  Term s ((a :--> b) :--> t k a :--> t k b)
pmap = phoistAcyclic $
  plam $
    \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

{- | As 'pmap', but over Data representations.

@since 2.0.0
-}
pmapData ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s ((PAsData a :--> PAsData b) :--> t k a :--> t k b)
pmapData = phoistAcyclic $
  plam $ \f m ->
    punsafeFromUnsortedMap $
      pcon . PUnsortedMap $
        precList
          ( \self x xs ->
              pcons
                # (ppairDataBuiltin # (pfstBuiltin # x) # (f #$ psndBuiltin # x))
                # (self # xs)
          )
          (const pnil)
          # pto (ptoUnsortedMap m)

{- | As 'pmap', but gives key access as well.

@since 2.1.1
-}
pmapWithKey ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term s ((k :--> a :--> b) :--> t k a :--> t k b)
pmapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    punsafeFromUnsortedMap $
      punsafeDowncast $
        PPrelude.pmap
          # plam
            ( \x ->
                plet (pkvPairKey # x) $ \key ->
                  ppairDataBuiltin
                    # pdata key
                    #$ pdata
                    $ f # key # (pkvPairValue # x)
            )
          # pto (ptoUnsortedMap kvs)

{- | Maps and filters the map, much like 'Data.PPrelude.mapMaybe'.

@since 2.0.0
-}
pmapMaybe ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData a
  , PIsData b
  ) =>
  Term s ((a :--> PMaybe b) :--> t k a :--> t k b)
pmapMaybe = phoistAcyclic $
  plam $ \f -> pmapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

{- | As 'pmapMaybe', but over Data representation.

@since 2.0.0
-}
pmapMaybeData ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s ((PAsData a :--> PMaybe (PAsData b)) :--> t k a :--> t k b)
pmapMaybeData = phoistAcyclic $
  plam $ \f m ->
    punsafeFromUnsortedMap $
      pcon . PUnsortedMap $
        precList
          ( \self x xs ->
              plet (self # xs) $ \xs' ->
                pmatch (f #$ psndBuiltin # x) $ \case
                  PNothing -> xs'
                  PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
          )
          (const pnil)
          # pto (ptoUnsortedMap m)

{- | As 'pmapMaybe', but gives key access as well.

@since 2.0.0
-}
pmapMaybeWithKey ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term s ((k :--> a :--> PMaybe b) :--> t k a :--> t k b)
pmapMaybeWithKey = phoistAcyclic $
  plam $ \f ->
    pmapMaybeDataWithKey #$ plam $ \k v -> pmatch (f # pfromData k # pfromData v) $ \case
      PNothing -> pcon PNothing
      PJust v' -> pcon $ PJust (pdata v')

{- | As 'pmapMaybeData', but gives key access as well.

@since 2.0.0
-}
pmapMaybeDataWithKey ::
  forall t (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s ((PAsData k :--> PAsData a :--> PMaybe (PAsData b)) :--> t k a :--> t k b)
pmapMaybeDataWithKey = phoistAcyclic $
  plam $ \f m ->
    punsafeFromUnsortedMap $
      pcon . PUnsortedMap $
        precList
          ( \self x xs ->
              plet (self # xs) $ \xs' ->
                pmatch (f # (pfstBuiltin # x) # (psndBuiltin # x)) $ \case
                  PNothing -> xs'
                  PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
          )
          (const pnil)
          # pto (ptoUnsortedMap m)

----------------------------------------------------------------------
-- Relational lift

{- | Given a comparison function and a "zero" value, check whether a binary
relation holds over two 'PSortedMap's.

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
        :--> PSortedMap k v
        :--> PSortedMap k v
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
     in inner # pto (pto m1) # pto (pto m2)

----------------------------------------------------------------------
-- Comparison

{- | Gives 'PTrue' if both argument 'PSortedMap's contain mappings for exactly
the same set of keys. Requires a number of equality comparisons between keys
proportional to the length of the shorter argument.

@since 2.1.1
-}
pkeysEqual ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PEq k
  , PIsData k
  ) =>
  Term s (PSortedMap k a :--> PSortedMap k b :--> PBool)
pkeysEqual = phoistAcyclic $
  plam $ \kvs kvs' ->
    go # pto (pto kvs) # pto (pto kvs')
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
works for 'PUnsortedMap's. This requires a number of equality comparisons
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
  Term s (PUnsortedMap k a :--> PUnsortedMap k b :--> PBool)
pkeysEqualUnsorted = phoistAcyclic $
  plam $ \kvs kvs' ->
    pmatch kvs $ \(PUnsortedMap ell) ->
      pmatch kvs' $ \(PUnsortedMap ell') ->
        go # kvs # kvs' # ell # ell'
  where
    go ::
      forall (s' :: S).
      Term
        s'
        ( PUnsortedMap k a
            :--> PUnsortedMap k b
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

----------------------------------------------------------------------
-- Fold

{- | Verifies all values in the map satisfy the given predicate.

@since 2.0.0
-}
pall ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData v
  ) =>
  Term s ((v :--> PBool) :--> t k v :--> PBool)
pall = phoistAcyclic $
  plam $ \pred m ->
    PPrelude.pall
      # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair)
      # pto (ptoUnsortedMap m)

{- | Tests if any value in the map satisfies the given predicate.

@since 2.1.1
-}
pany ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData v
  ) =>
  Term s ((v :--> PBool) :--> t k v :--> PBool)
pany = phoistAcyclic $
  plam $ \pred m ->
    PPrelude.pany
      # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair)
      # pto (ptoUnsortedMap m)

-- TODO: make `pfoldMapWithKey` and `pfoldlWithKey` more generic?

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
  Term s ((k :--> v :--> m) :--> PSortedMap k v :--> m)
pfoldMapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    pfoldlWithKey # plam (\acc k v -> acc <> (f # k # v)) # mempty # kvs

{- | Left-associative fold of a 'PSortedMap' with keys. Keys and values will be
presented in key order.

@since 2.1.1
-}
pfoldlWithKey ::
  forall (a :: S -> Type) (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsData k
  , PIsData v
  ) =>
  Term s ((a :--> k :--> v :--> a) :--> a :--> PSortedMap k v :--> a)
pfoldlWithKey = phoistAcyclic $
  plam $ \f x kvs ->
    pfoldl
      # plam (\acc kv -> f # acc # (pkvPairKey # kv) # (pkvPairValue # kv))
      # x
      # pto (pto kvs)

----------------------------------------------------------------------
-- Combination

{- | Build a function that zips two 'PSortedMap's together using a custom 'MergeHandler'.

The provided 'MergeHandler' determines how to merge entries based on whether a
key is present in the left map, the right map, or both.

= NOTE

This function itself cannot be hoisted with 'phoistAcyclic' because it
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
    ( PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k c
    )
zipWithBuilder mergeHandler =
  zipWithDataBuilder (Zip.mergeHandlerOnData mergeHandler)

{- | Build a function that zips two 'PSortedMap's together using a custom 'MergeHandler'.

The provided 'MergeHandler' determines how to merge entries based on whether a
key is present in the left map, the right map, or both.

Unlike 'zipWithBuilder', 'zipWithDataBuilder' operates on values wrapped in
'PAsData' (typed BuiltinData).

= NOTE

This function itself cannot be hoisted with 'phoistAcyclic' because it
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
    ( PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k c
    )
zipWithDataBuilder mergeHandler =
  plam $ \mapL mapR ->
    pcon $
      PSortedMap $
        pcon $
          PUnsortedMap $
            Zip.zipWorker mergeHandler # pto (pto mapL) # pto (pto mapR)

{- | Build the union of two 'PSortedMap's, merging values that share the same
key using the given function.

@since 3.5.0
-}
punionWith ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term
    s
    ( (v :--> v :--> v)
        :--> PSortedMap k v
        :--> PSortedMap k v
        :--> PSortedMap k v
    )
punionWith =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.unionMergeHandler

{- | Build the union of two 'PSortedMap's, merging values that share the same
key using the given function.

@since 3.5.0
-}
punionWithData ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  ) =>
  Term
    s
    ( (PAsData v :--> PAsData v :--> PAsData v)
        :--> PSortedMap k v
        :--> PSortedMap k v
        :--> PSortedMap k v
    )
punionWithData =
  phoistAcyclic $
    plam $
      zipWithDataBuilder . Zip.unionMergeHandler

{- | Build the union of two 'PSortedMap's. Take the value from the left argument
for colliding keys.

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
    ( PSortedMap k v
        :--> PSortedMap k v
        :--> PSortedMap k v
    )
pleftBiasedUnion =
  phoistAcyclic $
    zipWithBuilder Zip.leftBiasedUnionMergeHandler

{- Difference of two 'PSortedMap's. Return elements of the first map not
existing in the second map.

@since 2.1.1
-}
pdifference ::
  forall (b :: S -> Type) (a :: S -> Type) (k :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term
    s
    ( PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k a
    )
pdifference =
  phoistAcyclic $
    zipWithBuilder Zip.differenceMergeHandler

{- Difference with a combining function. When two equal keys are encountered,
the combining function is applied to the values of these keys. If it returns
'PNothing', the element is discarded.

@since 3.5.0
-}
pdifferenceWith ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData a
  , PIsData b
  ) =>
  Term
    s
    ( (a :--> b :--> PMaybe a)
        :--> PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k a
    )
pdifferenceWith =
  phoistAcyclic $
    plam $ \combine ->
      zipWithBuilder $
        Zip.differenceMergeHandler
          { mhBothPresent = HandleOrDropBoth $ plam (\_ valL valR -> combine # valL # valR)
          }

{- | Zip two 'PSortedMap's, using the given value merge function for key
collisions, and different values for the sides.

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
        :--> PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k c
    )
pzipWithDefaults defLeft defRight =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.zipMergeHandler defLeft defRight

{- | Build the intersection of two 'PSortedMap's, merging values that share the
same key using the given function.

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
        :--> PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k c
    )
pintersectionWith =
  phoistAcyclic $
    plam $
      zipWithBuilder . Zip.intersectionMergeHandler

{- | Build the intersection of two 'PSortedMap's, merging data-encoded values
that share the same key using the given function.

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
        :--> PSortedMap k a
        :--> PSortedMap k b
        :--> PSortedMap k c
    )
pintersectionWithData =
  phoistAcyclic $
    plam $
      zipWithDataBuilder . Zip.intersectionMergeHandler

----------------------------------------------------------------------
-- Query

{- | Tests whether the map is empty.

@since 2.0.0
-}
pnull ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s (t k v :--> PBool)
pnull = plam (\m -> PPrelude.pnull # pto (ptoUnsortedMap m))

{- | Look up the given key in a 'PMap'.

@since 2.1.1
-}
plookup ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData v
  ) =>
  Term s (k :--> t k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \key ->
    plookupDataWith
      # phoistAcyclic (plam $ \pair -> pcon $ PJust $ pfromData $ psndBuiltin # pair)
      # pdata key

{- | As 'plookup', except over Data representation.

@since 2.1.1
-}
plookupData ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s (PAsData k :--> t k v :--> PMaybe (PAsData v))
plookupData =
  plookupDataWith # phoistAcyclic (plam $ \pair -> pcon $ PJust $ psndBuiltin # pair)

{- | Look up the given key data in a 'PMap', applying the given function to the
found key-value pair.

@since 2.1.1
-}
plookupDataWith ::
  forall t (k :: S -> Type) (v :: S -> Type) (x :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term
    s
    ( (PBuiltinPair (PAsData k) (PAsData v) :--> PMaybe x)
        :--> PAsData k
        :--> t k v
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
      # pto (ptoUnsortedMap m)

{- | Look up the given key in a 'PMap', returning the default value if the key
is absent.

@since 2.1.1
-}
pfindWithDefault ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData v
  ) =>
  Term s (v :--> k :--> t k v :--> v)
pfindWithDefault =
  phoistAcyclic $
    plam $ \def key ->
      pfoldAtData # pdata key # def # plam pfromData

{- | Look up the given key in a 'PMap'; return the default if the key is
absent or apply the argument function to the value data if present.

@since 2.1.1
-}
pfoldAt ::
  forall t (k :: S -> Type) (v :: S -> Type) (r :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  ) =>
  Term s (k :--> r :--> (PAsData v :--> r) :--> t k v :--> r)
pfoldAt = phoistAcyclic $
  plam $
    \key -> pfoldAtData # pdata key

{- | Look up the given key data in a 'PMap'; return the default if the key is
absent or apply the argument function to the value data if present.

@since 2.1.1
-}
pfoldAtData ::
  forall t (k :: S -> Type) (v :: S -> Type) (r :: S -> Type) (s :: S).
  PIsAssocMap t =>
  Term s (PAsData k :--> r :--> (PAsData v :--> r) :--> t k v :--> r)
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
      # pto (ptoUnsortedMap m)

{- | As 'plookup', but errors when the key is missing.

@since 2.1.1
-}
ptryLookup ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PIsData v
  ) =>
  Term s (k :--> t k v :--> v)
ptryLookup = phoistAcyclic $
  plam $ \k kvs ->
    passertPJust
      # "plookupPartial: No value found for key."
      # (plookup # k # kvs)

----------------------------------------------------------------------
-- Modification

{- | Insert a new key/value pair into the map, overriding the previous if any.

@since 2.1.1
-}
pinsert ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( POrd k
  , PIsData k
  , PIsData v
  ) =>
  Term s (k :--> v :--> PSortedMap k v :--> PSortedMap k v)
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
  Term s (k :--> PSortedMap k v :--> PSortedMap k v)
pdelete = rebuildAtKey # plam id

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
  Term s ((v :--> PMaybe v) :--> k :--> PSortedMap k v :--> PSortedMap k v)
pupdate = phoistAcyclic $
  plam $ \updater key kvs -> pmatch kvs $ \(PSortedMap kvs') ->
    pcon . PSortedMap . pcon . PUnsortedMap $
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
          # pto kvs'
      )

{- | If a value exists at the specified key, apply the function argument to it;
 otherwise, do nothing.

 @since 2.1.1
-}
padjust ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , PIsData k
  , PEq k
  , PIsData v
  ) =>
  Term s ((v :--> v) :--> k :--> t k v :--> t k v)
padjust = phoistAcyclic $
  plam $ \f key kvs ->
    pmapWithKey # plam (\k' a -> pif (k' #== key) (f # a) a) # kvs

----------------------------------------------------------------------
-- Key-value pair manipulation

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

----------------------------------------------------------------------
-- Conversion

{- | Extract the keys from the given 'PIsAssocMap' instance as a list-like
structure. If the provided Map is 'PSortedMap', the keys will maintain that
order, and will be unique; otherwise, the order is unspecified, and duplicates
may exist.

= Note

You will need to specify what manner of list-like structure you want; we have
arranged the type signature to make specifying this easy with
@TypeApplications@.

@since 2.1.1
-}
pkeys ::
  forall
    (ell :: (S -> Type) -> S -> Type)
    (t :: (S -> Type) -> (S -> Type) -> S -> Type)
    (k :: S -> Type)
    (v :: S -> Type)
    (s :: S).
  ( PIsAssocMap t
  , PListLike ell
  , PElemConstraint ell (PAsData k)
  ) =>
  Term s (t k v :--> ell (PAsData k))
pkeys = phoistAcyclic $
  plam $ \kvs ->
    precList go (const pnil) # pto (ptoUnsortedMap kvs)
  where
    go ::
      forall (s' :: S).
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v)) :--> ell (PAsData k)) ->
      Term s' (PBuiltinPair (PAsData k) (PAsData v)) ->
      Term s' (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))) ->
      Term s' (ell (PAsData k))
    go self kv acc = pcons # (pfstBuiltin # kv) # (self # acc)

----------------------------------------------------------------------
-- Internal

-- | Rebuild the map at the given key.
rebuildAtKey ::
  forall t (k :: S -> Type) (v :: S -> Type) (s :: S).
  ( PIsAssocMap t
  , POrd k
  , PIsData k
  ) =>
  Term
    s
    ( ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
          :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
      )
        :--> k
        :--> t k v
        :--> t k v
    )
rebuildAtKey = phoistAcyclic $
  plam $ \handler key m ->
    punsafeFromUnsortedMap $
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
          # pto (ptoUnsortedMap m)
          # plam id

-- We have to clone this in here or we get a dependency cycle
passertPJust :: forall (a :: S -> Type) (s :: S). Term s (PString :--> PMaybe a :--> a)
passertPJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceInfoError emsg
