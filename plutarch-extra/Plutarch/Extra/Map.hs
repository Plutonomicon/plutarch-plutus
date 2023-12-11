{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Map (
  -- * Lookup
  ptryLookup,

  -- * Comparisons
  pkeysEqual,
  pkeysEqualUnsorted,

  -- * Modification
  pupdate,
  padjust,
  pmapWithKey,

  -- * Folds
  pfoldMapWithKey,
  pfoldlWithKey,

  -- * Conversion
  pkeys,

  -- * Key-value pair manipulation
  pkvPairKey,
  pkvPairValue,
  pkvPairLt,
) where

import Plutarch.Api.V1.AssocMap (
  KeyGuarantees (Sorted, Unsorted),
  PMap (PMap),
  plookup,
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.Maybe (passertPJust)
import qualified Plutarch.List as PList
import Plutarch.Prelude

{- | Like regular 'fmap' but it provides key of each element that is being
     modified.
-}
pmapWithKey ::
  forall (k :: PType) (a :: PType) (b :: PType) (keysort :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData a, PIsData b) =>
  Term s ((k :--> a :--> b) :--> PMap keysort k a :--> PMap 'Unsorted k b)
pmapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    pmatch kvs $ \(PMap kvs') ->
      pcon . PMap $
        PList.pmap
          # plam
            ( \x ->
                plet (pkvPairKey # x) $ \key ->
                  ppairDataBuiltin
                    # pdata key
                    #$ pdata
                    $ f # key # (pkvPairValue # x)
            )
          # kvs'

{- | If a value exists at the specified key, apply the function argument to it;
 otherwise, do nothing.
-}
padjust ::
  forall (k :: PType) (v :: PType) (s :: S).
  (PIsData k, PEq k, PIsData v) =>
  Term s ((v :--> v) :--> k :--> PMap 'Unsorted k v :--> PMap 'Unsorted k v)
padjust = phoistAcyclic $
  plam $ \f key kvs ->
    pmapWithKey # plam (\k' a -> pif (k' #== key) (f # a) a) # kvs

{- | As 'pkeysEqual', but requires only 'PEq' constraints for the keys, and
 works for 'Unsorted' 'PMap's. This requires a number of equality comparisons
 between keys proportional to the product of the lengths of both arguments:
 that is, this function is quadratic.
-}
pkeysEqualUnsorted ::
  forall (k :: PType) (a :: PType) (b :: PType) (s :: S).
  (PIsData k, PIsData a, PIsData b) =>
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
        pmatch (PList.puncons # ell) $ \case
          PNothing -> pmatch (PList.puncons # ell') $ \case
            -- We reached the end, so we match
            PNothing -> pcon PTrue
            PJust ht' -> pmatch ht' $ \(PPair h' t') ->
              pmatch (plookup # (pkvPairKey # h') # kvs) $ \case
                -- We mismatch, so fail
                PNothing -> pcon PFalse
                -- We match, so continue
                PJust _ -> self # kvs # kvs' # ell # t'
          PJust ht -> pmatch ht $ \(PPair h t) ->
            pmatch (PList.puncons # ell') $ \case
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

{- | Gives 'PTrue' if both argument 'PMap's contain mappings for exactly the
 same set of keys. Requires a number of equality comparisons between keys
 proportional to the length of the shorter argument.
-}
pkeysEqual ::
  forall (k :: PType) (a :: PType) (b :: PType) (s :: S).
  (PIsData k, PEq k) =>
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
        pmatch (PList.puncons # ell) $ \case
          PNothing -> pmatch (PList.puncons # ell') $ \case
            PNothing -> pcon PTrue -- no mismatches found
            PJust _ -> pcon PFalse -- one argument too long
          PJust kv -> pmatch (PList.puncons # ell') $ \case
            PNothing -> pcon PFalse -- one argument too long
            PJust kv' -> pmatch kv $ \(PPair h t) ->
              pmatch kv' $ \(PPair h' t') ->
                pif
                  ((pkvPairKey # h) #== (pkvPairKey # h'))
                  (self # t # t') -- continue
                  (pcon PFalse) -- key mismatch

-- | Get the key of a key-value pair.
pkvPairKey ::
  forall (k :: PType) (v :: PType) (s :: S).
  PIsData k =>
  Term s (PBuiltinPair (PAsData k) (PAsData v) :--> k)
pkvPairKey = phoistAcyclic $ plam $ \kv -> pfromData (pfstBuiltin # kv)

-- | Get the value of a key-value pair.
pkvPairValue ::
  forall (k :: PType) (v :: PType) (s :: S).
  PIsData v =>
  Term s (PBuiltinPair (PAsData k) (PAsData v) :--> v)
pkvPairValue = phoistAcyclic $ plam $ \kv -> pfromData (psndBuiltin # kv)

{- | Compare two key-value pairs by their keys. Gives 'PTrue' if the key of the
 first argument pair is less than the key of the second argument pair.
-}
pkvPairLt ::
  forall (k :: PType) (v :: PType) (s :: S).
  (PIsData k, PPartialOrd k) =>
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBool
    )
pkvPairLt = phoistAcyclic $
  plam $ \kv kv' ->
    (pkvPairKey # kv) #< (pkvPairKey # kv')

-- | As 'plookup', but errors when the key is missing.
ptryLookup ::
  forall (k :: PType) (v :: PType) (keys :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData v) =>
  Term s (k :--> PMap keys k v :--> v)
ptryLookup = phoistAcyclic $
  plam $ \k kvs ->
    passertPJust
      # "plookupPartial: No value found for key."
      # (plookup # k # kvs)

{- | Get a list-like structure full of the keys of the argument 'PMap'. If the
 'PMap' is 'Sorted', the keys will maintain that order, and will be unique;
 otherwise, the order is unspecified, and duplicates may exist.

 = Note

 You will need to specify what manner of list-like structure you want; we have
 arranged the type signature to make specifying this easy with
 @TypeApplications@.
-}
pkeys ::
  forall
    (ell :: PType -> PType)
    (k :: PType)
    (v :: PType)
    (keys :: KeyGuarantees)
    (s :: S).
  (PListLike ell, PElemConstraint ell (PAsData k)) =>
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

{- | Given an \'updater\' and a key, if the key exists in the 'PMap', apply the
 \'updater\' to it, otherwise do nothing. If the \'updater\' produces
 'PNothing', the value is deleted; otherwise, it is modified to the result.

 Performance will be equivalent to a lookup followed by an insert (or delete),
 as well as the cost of calling the \'updater\'.
-}
pupdate ::
  forall (k :: PType) (v :: PType) (s :: S).
  (PIsData k, PIsData v, POrd k) =>
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

{- | Left-associative fold of a 'PMap' with keys. Keys and values will be
 presented in key order.
-}
pfoldlWithKey ::
  forall (a :: PType) (k :: PType) (v :: PType) (s :: S).
  (PIsData k, PIsData v) =>
  Term s ((a :--> k :--> v :--> a) :--> a :--> PMap 'Sorted k v :--> a)
pfoldlWithKey = phoistAcyclic $
  plam $ \f x kvs -> pmatch kvs $ \case
    PMap kvs' ->
      pfoldl # plam (\acc kv -> f # acc # (pkvPairKey # kv) # (pkvPairValue # kv)) # x # kvs'

{- | Project all key-value pairs into a 'Monoid', then combine. Keys and values
 will be presented in key order.
-}
pfoldMapWithKey ::
  forall (m :: PType) (k :: PType) (v :: PType) (s :: S).
  (PIsData k, PIsData v, forall (s' :: S). Monoid (Term s' m)) =>
  Term s ((k :--> v :--> m) :--> PMap 'Sorted k v :--> m)
pfoldMapWithKey = phoistAcyclic $
  plam $ \f kvs ->
    pfoldlWithKey # plam (\acc k v -> acc <> (f # k # v)) # mempty # kvs
