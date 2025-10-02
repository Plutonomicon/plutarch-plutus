{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Plutarch.LedgerApi.AssocMap.Zip (
  BothPresentHandler (..),
  MergeHandler (..),
  OnePresentHandler (..),
  differenceMergeHandler,
  intersectionMergeHandler,
  leftBiasedUnionMergeHandler,
  mergeHandlerOnData,
  unionMergeHandler,
  zipMergeHandler,
  zipWorker,
) where

import Data.Kind (Type)
import Plutarch.Maybe (pmapDropNothing, pmapMaybe, pmaybe)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude hiding (pmap)
import Plutarch.Prelude qualified as PPrelude (pmap)

--------------------------------------------------------------------------------
-- Types

data MergeHandler (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) = MergeHandler
  { mhBothPresent :: BothPresentHandler s k a b c
  , mhLeftPresent :: OnePresentHandler s k a c
  , mhRightPresent :: OnePresentHandler s k b c
  }

data BothPresentHandler (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type)
  = DropBoth
  | HandleBoth (Term s (k :--> a :--> b :--> c))
  | HandleOrDropBoth (Term s (k :--> a :--> b :--> PMaybe c))

data OnePresentHandler (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type)
  = DropOne
  | HandleOne (Term s (k :--> a :--> b))
  | HandleOrDropOne (Term s (k :--> a :--> PMaybe b))

--------------------------------------------------------------------------------
-- Handler Conversion

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
    { mhBothPresent = bothPresentHandlerOnData mergeHandler.mhBothPresent
    , mhLeftPresent = onePresentHandlerOnData mergeHandler.mhLeftPresent
    , mhRightPresent = onePresentHandlerOnData mergeHandler.mhRightPresent
    }

bothPresentHandlerOnData ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  ( PIsData k
  , PIsData a
  , PIsData b
  , PIsData c
  ) =>
  BothPresentHandler s k a b c ->
  BothPresentHandler s (PAsData k) (PAsData a) (PAsData b) (PAsData c)
bothPresentHandlerOnData = \case
  DropBoth -> DropBoth
  HandleBoth f ->
    HandleBoth $ plam \k valL valR ->
      pdata $ f # pfromData k # pfromData valL # pfromData valR
  HandleOrDropBoth f ->
    HandleOrDropBoth $ plam \k valL valR ->
      pmapMaybe
        # plam pdata
        # (f # pfromData k # pfromData valL # pfromData valR)

onePresentHandlerOnData ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type).
  ( PIsData k
  , PIsData a
  , PIsData b
  ) =>
  OnePresentHandler s k a b ->
  OnePresentHandler s (PAsData k) (PAsData a) (PAsData b)
onePresentHandlerOnData = \case
  DropOne -> DropOne
  HandleOne f ->
    HandleOne $ plam \k v ->
      pdata $ f # pfromData k # pfromData v
  HandleOrDropOne f ->
    HandleOrDropOne $ plam \k v ->
      pmapMaybe # plam pdata # (f # pfromData k # pfromData v)

--------------------------------------------------------------------------------
-- Handlers

zipMergeHandler ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  Term s a ->
  Term s b ->
  Term s (a :--> b :--> c) ->
  MergeHandler s k a b c
zipMergeHandler defL defR combine =
  MergeHandler
    { mhBothPresent = HandleBoth $ plam (\_ valL valR -> combine # valL # valR)
    , mhLeftPresent = HandleOne $ plam (\_ valL -> combine # valL # defR)
    , mhRightPresent = HandleOne $ plam (\_ valR -> combine # defL # valR)
    }

intersectionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  Term s (a :--> b :--> c) ->
  MergeHandler s k a b c
intersectionMergeHandler merge =
  MergeHandler
    { mhBothPresent = HandleBoth $ plam (\_ valL valR -> merge # valL # valR)
    , mhLeftPresent = DropOne
    , mhRightPresent = DropOne
    }

differenceMergeHandler ::
  forall (s :: S) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type).
  MergeHandler s k a b a
differenceMergeHandler =
  MergeHandler
    { mhBothPresent = DropBoth
    , mhLeftPresent = HandleOne $ plam (\_ valL -> valL)
    , mhRightPresent = DropOne
    }

unionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (v :: S -> Type).
  Term s (v :--> v :--> v) ->
  MergeHandler s k v v v
unionMergeHandler merge =
  MergeHandler
    { mhBothPresent = HandleBoth $ plam (\_ valL valR -> merge # valL # valR)
    , mhLeftPresent = HandleOne $ plam (\_ valL -> valL)
    , mhRightPresent = HandleOne $ plam (\_ valR -> valR)
    }

leftBiasedUnionMergeHandler ::
  forall (s :: S) (k :: S -> Type) (v :: S -> Type).
  MergeHandler s k v v v
leftBiasedUnionMergeHandler = unionMergeHandler $ plam const

--------------------------------------------------------------------------------
-- Logic

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
  pfixHoisted #$ plam $ \self mapL mapR ->
    pmatch mapL $ \case
      PNil ->
        case mergeHandler.mhRightPresent of
          DropOne -> pnil
          HandleOne handler ->
            PPrelude.pmap
              # plam
                ( \entry -> P.do
                    k <- plet $ pfstBuiltin # entry
                    ppairDataBuiltin # k #$ handler # k #$ psndBuiltin # entry
                )
              # mapR
          HandleOrDropOne handler ->
            pmapDropNothing
              # plam
                ( \entry -> P.do
                    k <- plet $ pfstBuiltin # entry
                    v <- plet $ psndBuiltin # entry
                    pmapMaybe # plam (\x -> ppairDataBuiltin # k # x) # (handler # k # v)
                )
              # mapR
      PCons entryL restL ->
        pmatch mapR $ \case
          PNil ->
            case mergeHandler.mhLeftPresent of
              DropOne -> pnil
              HandleOne handler ->
                PPrelude.pmap
                  # plam
                    ( \entry -> P.do
                        k <- plet $ pfstBuiltin # entry
                        ppairDataBuiltin # k #$ handler # k #$ psndBuiltin # entry
                    )
                  # mapL
              HandleOrDropOne handler ->
                pmapDropNothing
                  # plam
                    ( \entry -> P.do
                        k <- plet $ pfstBuiltin # entry
                        v <- plet $ psndBuiltin # entry
                        pmapMaybe # plam (\x -> ppairDataBuiltin # k # x) # (handler # k # v)
                    )
                  # mapL
          PCons entryR restR -> P.do
            keyDataL <- plet $ pfstBuiltin # entryL
            keyDataR <- plet $ pfstBuiltin # entryR
            keyL <- plet $ pfromData keyDataL
            keyR <- plet $ pfromData keyDataR
            valL <- plet $ psndBuiltin # entryL
            valR <- plet $ psndBuiltin # entryR
            pif
              (keyL #== keyR)
              ( let
                  xs = self # restL # restR
                  k = keyDataL
                 in
                  case mergeHandler.mhBothPresent of
                    DropBoth ->
                      xs
                    HandleBoth handler ->
                      pcons # (ppairDataBuiltin # k #$ handler # k # valL # valR) # xs
                    HandleOrDropBoth handler -> P.do
                      zipped <- plet xs
                      pmaybe
                        # zipped
                        # plam (\v -> pcons # (ppairDataBuiltin # k # v) # zipped)
                        # (handler # k # valL # valR)
              )
              ( pif
                  (keyL #< keyR)
                  ( let
                      xs = self # restL # mapR
                     in
                      case mergeHandler.mhLeftPresent of
                        DropOne -> xs
                        HandleOne handler ->
                          pcons
                            # (ppairDataBuiltin # keyDataL #$ handler # keyDataL # valL)
                            # xs
                        HandleOrDropOne handler -> P.do
                          zipped <- plet xs
                          pmaybe
                            # zipped
                            # plam (\v -> pcons # (ppairDataBuiltin # keyDataL # v) # zipped)
                            # (handler # keyDataL # valL)
                  )
                  ( let
                      xs = self # mapL # restR
                     in
                      case mergeHandler.mhRightPresent of
                        DropOne -> xs
                        HandleOne handler ->
                          pcons
                            # (ppairDataBuiltin # keyDataR #$ handler # keyDataR # valR)
                            # xs
                        HandleOrDropOne handler -> P.do
                          zipped <- plet xs
                          pmaybe
                            # zipped
                            # plam (\v -> pcons # (ppairDataBuiltin # keyDataR # v) # zipped)
                            # (handler # keyDataR # valR)
                  )
              )
