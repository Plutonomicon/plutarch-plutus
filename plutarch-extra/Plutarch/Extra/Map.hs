{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Map (
  plookup,
  plookup',
  pmapFromList,
  pkeys,
  pupdate,
  Plutarch.Extra.Map.pmap,
  pkvPairKey,
  pkvPairLt,
  pfoldlWithKey,
) where

import Data.Foldable (foldl')
import Plutarch.Api.V1.AssocMap (KeyGuarantees, PMap (PMap))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.List (pmapMaybe)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import qualified Plutarch.List
import Plutarch.Prelude

-- | @since 1.0.0
plookup ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData v, PIsData k, PEq k) =>
  Term s (k :--> PMap keys k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \x m -> unTermCont $ do
    PMap kvs <- pmatchC m
    res <- pmatchC (pfind # (go # x) # kvs)
    pure $ case res of
      PNothing -> pcon PNothing
      PJust y -> pcon . PJust . pfromData $ psndBuiltin # y
  where
    go ::
      forall (s' :: S).
      Term s' (k :--> PBuiltinPair (PAsData k) (PAsData v) :--> PBool)
    go = phoistAcyclic $
      plam $ \x p -> unTermCont $ do
        k' <- pletC (pfromData $ pfstBuiltin # p)
        pure $ k' #== x

-- | @since 1.0.0
plookup' ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData v, PIsData k, PEq k) =>
  Term s (k :--> PMap keys k v :--> v)
plookup' = phoistAcyclic $
  plam $ \x m -> unTermCont $ do
    res <- pmatchC (plookup # x # m)
    pure $ case res of
      PNothing -> ptraceError "plookup': Could not find key."
      PJust v -> v

-- | @since 1.0.0
pmapFromList ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData v) =>
  [(Term s k, Term s v)] ->
  Term s (PMap keys k v)
pmapFromList = pcon . PMap . foldl' go (pcon PNil)
  where
    go acc (k, v) = unTermCont $ do
      k' <- pletC (pdata k)
      v' <- pletC (pdata v)
      p <- pletC (ppairDataBuiltin # k' # v')
      pure . pcon . PCons p $ acc

{- | Get the keys of a given map, the order of the keys is preserved.

      @since 1.1.0
-}
pkeys ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  Term s (PMap keys k v :--> PBuiltinList (PAsData k))
pkeys = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    PMap kvs <- pmatchC m
    pure $ Plutarch.Prelude.pmap # pfstBuiltin # kvs

{- | / O(n) /. Update the value at a given key in a `PMap`, have the same functionalities as 'Data.Map.update'.

     @since 1.1.0
-}
pupdate ::
  forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData v) =>
  Term s ((v :--> PMaybe v) :--> k :--> PMap keys k v :--> PMap keys k v)
pupdate = phoistAcyclic $
  plam $ \f (pdata -> tk) (pto -> (ps :: Term _ (PBuiltinList _))) ->
    pcon $
      PMap $
        pmapMaybe
          # plam
            ( \kv ->
                let k = pfstBuiltin # kv
                    v = pfromData $ psndBuiltin # kv
                 in pif
                      (k #== tk)
                      ( pmatch (f # v) $
                          \case
                            PJust uv -> pcon $ PJust $ ppairDataBuiltin # k # pdata uv
                            _ -> pcon PNothing
                      )
                      (pcon $ PJust kv)
            )
          # ps

{- | / O(n) /. Map a function over all values in a 'PMap'.

     @since 1.1.0
-}
pmap ::
  forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData a, PIsData b) =>
  Term s ((a :--> b) :--> PMap keys k a :--> PMap keys k b)
pmap = phoistAcyclic $
  plam $ \f (pto -> (ps :: Term _ (PBuiltinList _))) ->
    pcon $
      PMap $
        Plutarch.List.pmap
          # plam
            ( \kv ->
                let k = pfstBuiltin # kv
                    v = psndBuiltin # kv

                    nv = pdata $ f # pfromData v
                 in ppairDataBuiltin # k # nv
            )
          # ps

{- | Get the key of a key-value pair.

     @since 1.1.0
-}
pkvPairKey :: (PIsData k) => Term s (PBuiltinPair (PAsData k) (PAsData v) :--> k)
pkvPairKey = phoistAcyclic $ plam $ \(pfromData . (pfstBuiltin #) -> key) -> key

{- | Compare two key-value pairs by their keys, return true if the first key is less than the second one.

      @since 1.1.0
-}
pkvPairLt ::
  (PIsData k, POrd k) =>
  Term s (PBuiltinPair (PAsData k) (PAsData v) :--> PBuiltinPair (PAsData k) (PAsData v) :--> PBool)
pkvPairLt = phoistAcyclic $
  plam $ \((pkvPairKey #) -> keyA) ((pkvPairKey #) -> keyB) -> keyA #< keyB

-- | @since 1.3.0
pfoldlWithKey ::
  forall (a :: S -> Type) (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
  (PIsData k, PIsData v) =>
  Term s ((a :--> k :--> v :--> a) :--> a :--> PMap keys k v :--> a)
pfoldlWithKey = phoistAcyclic $
  plam $ \f a (pto -> l :: Term _ (PBuiltinList _)) ->
    pfoldl
      # plam
        ( \x p ->
            let k = pfromData $ pfstBuiltin # p
                v = pfromData $ psndBuiltin # p
             in f # x # k # v
        )
      # a
      # l
