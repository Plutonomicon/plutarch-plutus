{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Plutarch.LedgerApi.AssocMap.Zip (
  MergeHandler (..),
  defaultMergeHandler,
  differenceMergeHandler,
  intersectionMergeHandler,
  leftBiasedUnionMergeHandler,
  mergeHandlerOnData,
  unionMergeHandler,
  zipWorker,
) where

import Data.Kind (Type)
import Plutarch.Maybe (pjust, pmapMaybe, pmaybe, pnothing)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude hiding (pmap, pzipWith)

pmapDropNothing ::
  forall (s :: S) (list :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type).
  ( PListLike list
  , PElemConstraint list a
  , PElemConstraint list b
  ) =>
  Term s ((a :--> PMaybe b) :--> list a :--> list b)
pmapDropNothing =
  phoistAcyclic $
    plam $ \f ->
      precList
        ( \self x xs -> P.do
            xs' <- plet $ self # xs
            pmaybe # xs' # plam (\x' -> pcons # x' # xs') #$ f # x
        )
        (const pnil)

data MergeHandler (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) = MergeHandler
  { mhBothPresent :: Term s (k :--> a :--> b :--> PMaybe c)
  , mhLeftPresent :: Term s (k :--> a :--> PMaybe c)
  , mhRightPresent :: Term s (k :--> b :--> PMaybe c)
  }

{- TODO: Should we really care about commutativity here?
data MergeHandlerCommutative (s :: S) (k :: S -> Type) (v :: S -> Type) = MergeHandlerCommutative
  { mhBothPresent :: Term s (k :--> v :--> v :--> PMaybe v)
  , mhOnePresent :: Term s (k :--> v :--> PMaybe v)
  }
-}

mergeHandlerOnData ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  ( PIsData k
  , PIsData a
  , PIsData b
  , PIsData c
  ) =>
  MergeHandler s k a b c ->
  MergeHandler s (PAsData k) (PAsData a) (PAsData b) (PAsData c)
mergeHandlerOnData mergeHandler =
  MergeHandler
    { mhBothPresent =
        plam $ \k valL valR ->
          pmapMaybe
            # plam pdata
            # (mergeHandler.mhBothPresent # pfromData k # pfromData valL # pfromData valR)
    , mhLeftPresent =
        plam $ \k valL ->
          pmapMaybe
            # plam pdata
            # (mergeHandler.mhLeftPresent # pfromData k # pfromData valL)
    , mhRightPresent =
        plam $ \k valR ->
          pmapMaybe
            # plam pdata
            # (mergeHandler.mhRightPresent # pfromData k # pfromData valR)
    }

defaultMergeHandler ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  Term s a ->
  Term s b ->
  Term s (a :--> b :--> c) ->
  MergeHandler s k a b c
defaultMergeHandler defL defR combine =
  MergeHandler
    { mhBothPresent = plam (\_ valL valR -> pjust #$ combine # valL # valR)
    , mhLeftPresent = plam (\_ valL -> pjust #$ combine # valL # defR)
    , mhRightPresent = plam (\_ valR -> pjust #$ combine # defL # valR)
    }

intersectionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  Term s (a :--> b :--> c) ->
  MergeHandler s k a b c
intersectionMergeHandler merge =
  MergeHandler
    { mhBothPresent = plam (\_ valL valR -> pjust #$ merge # valL # valR)
    , mhLeftPresent = plam (\_ _ -> pnothing)
    , mhRightPresent = plam (\_ _ -> pnothing)
    }

differenceMergeHandler ::
  forall (s :: S) (k :: S -> Type) (v :: S -> Type).
  MergeHandler s k v v v
differenceMergeHandler =
  MergeHandler
    { mhBothPresent = plam (\_ _ _ -> pnothing)
    , mhLeftPresent = plam (\_ valL -> pjust # valL)
    , mhRightPresent = plam (\_ _ -> pnothing)
    }

unionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (v :: S -> Type).
  Term s (v :--> v :--> v) ->
  MergeHandler s k v v v
unionMergeHandler merge =
  MergeHandler
    { mhBothPresent = plam (\_ valL valR -> pjust #$ merge # valL # valR)
    , mhLeftPresent = plam (\_ valL -> pjust # valL)
    , mhRightPresent = plam (\_ valR -> pjust # valR)
    }

leftBiasedUnionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (v :: S -> Type).
  MergeHandler s k v v v
leftBiasedUnionMergeHandler = unionMergeHandler $ plam const

zipWorker ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  ( POrd k
  , PIsData k
  ) =>
  MergeHandler s (PAsData k) (PAsData a) (PAsData b) (PAsData c) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData a))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData b))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData c))
    )
zipWorker mergeHandler =
  pfix #$ plam $ \self mapL mapR ->
    pmatch mapL $ \case
      PNil ->
        pmapDropNothing
          # plam
            ( \entry -> P.do
                k <- plet $ pfstBuiltin # entry
                v <- plet $ psndBuiltin # entry
                pmapMaybe
                  # plam (\x -> ppairDataBuiltin # k # x)
                  # (mergeHandler.mhRightPresent # k # v)
            )
          # mapR
      PCons entryL restL ->
        pmatch mapR $ \case
          PNil ->
            pmapDropNothing
              # plam
                ( \entry -> P.do
                    k <- plet $ pfstBuiltin # entry
                    v <- plet $ psndBuiltin # entry
                    pmapMaybe
                      # plam (\x -> ppairDataBuiltin # k # x)
                      # (mergeHandler.mhLeftPresent # k # v)
                )
              # mapL
          PCons entryR restR -> P.do
            keyL <- plet $ pfstBuiltin # entryL
            keyR <- plet $ pfstBuiltin # entryR
            valL <- plet $ psndBuiltin # entryL
            valR <- plet $ psndBuiltin # entryR
            pif
              (keyL #== keyR)
              ( P.do
                  zipped <- plet $ self # restL # restR
                  pmaybe
                    # zipped
                    # plam (\v -> pcons # (ppairDataBuiltin # keyL # v) # zipped)
                    # (mergeHandler.mhBothPresent # keyL # valL # valR)
              )
              ( pif
                  (pfromData keyL #< pfromData keyR)
                  ( P.do
                      zipped <- plet $ self # restL # mapR
                      pmaybe
                        # zipped
                        # plam (\v -> pcons # (ppairDataBuiltin # keyL # v) # zipped)
                        # (mergeHandler.mhLeftPresent # keyL # valL)
                  )
                  ( P.do
                      zipped <- plet $ self # mapL # restR
                      pmaybe
                        # zipped
                        # plam (\v -> pcons # (ppairDataBuiltin # keyR # v) # zipped)
                        # (mergeHandler.mhRightPresent # keyR # valR)
                  )
              )
