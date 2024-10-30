{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- This needs to go later
{-# OPTIONS_GHC -Wwarn #-}

module Plutarch.Rework where

import Generics.SOP

import GHC.TypeLits

import Data.Functor.Compose

import Control.Monad.Reader
import Data.Kind (Type)
import Data.List (groupBy, sortBy)
import Data.Proxy
import Data.Text qualified as Text
import Plutarch
import Plutarch.Builtin
import Plutarch.DataRepr.Internal
import Plutarch.Internal
import Plutarch.Prelude
import Plutarch.Unsafe

newtype PStruct (struct :: [[S -> Type]]) (s :: S) = PStruct (SOP (Term s) struct)
newtype PRec (struct :: [S -> Type]) (s :: S) = PRec (NP (Term s) struct)

----------------------------------------------------------------------Smart plet
pdataStructureAsData :: Term s (PDataStruct struct) -> Term s PData
pdataStructureAsData = punsafeCoerce

getRawTerm :: Term s a -> TermCont s RawTerm
getRawTerm t = TermCont $ \f ->
  Term $ \i ->
    case runReaderT (runTermMonad (asRawTerm t i)) mempty of
      Right rt -> asRawTerm (f $ getTerm rt) i
      Left e -> asRawTerm (pthrow e) i

findOccurrence :: Dig -> Term s a -> TermCont s Int
findOccurrence targetHash t = TermCont $ \f -> unTermCont $ do
  rt <- getRawTerm t
  let
    go (RLamAbs _ rt) = go rt
    go (RApply rt rts) = go rt + sum (go <$> rts)
    go (RForce rt) = go rt
    go (RDelay rt) = go rt
    go (RHoisted (HoistedTerm hash _)) = if hash == targetHash then 1 else 0
    go _ = 0

  pure $ f (go rt)

createPlaceholder :: Term s a -> TermCont s (Dig, Term s a)
createPlaceholder t = do
  (hash :: Dig) <- hashOpenTerm t
  pure (hash, Term $ const $ pure $ TermResult (RHoisted (HoistedTerm hash RError)) [])

pletSmart :: Term s a -> (Term s a -> Term s b) -> Term s b
pletSmart t f = unTermCont $ do
  (hash, placeholder) <- createPlaceholder t

  occurrences <-
    findOccurrence hash $ f placeholder

  pure $ case occurrences of
    0 -> f perror
    1 -> f t
    _ -> plet t f

pletSmartC :: Term s a -> TermCont s (Term s a)
pletSmartC = tcont . pletSmart

----------------------------------------------------------------------PDataStruct
data PDataStruct (struct :: [[S -> Type]]) (s :: S) where
  PDataStruct :: (All2 PDataRepresentable struct, All2 PIsData struct) => PStruct struct s -> PDataStruct struct s

data PDataRec (struct :: [S -> Type]) (s :: S) where
  PDataRec :: (All PDataRepresentable struct, All PIsData struct) => PRec struct s -> PDataRec struct s

class PDataRepresentable (a :: S -> Type)
instance PDataRepresentable (PDataStruct struct)
instance PDataRepresentable PInteger
instance PDataRepresentable PByteString

pconDataRec ::
  forall (struct :: [S -> Type]) (s :: S).
  All PIsData struct =>
  PRec struct s ->
  Term s (PDataRec struct)
pconDataRec (PRec xs) =
  let
    collapesdData = hcollapse $ hcmap (Proxy @PIsData) (K . pforgetData . pdata) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
   in
    punsafeCoerce builtinList

pconDataStruct ::
  forall (struct :: [[S -> Type]]) (s :: S).
  (SListI2 struct, All2 PIsData struct) =>
  PStruct struct s ->
  Term s (PDataStruct struct)
pconDataStruct (PStruct xs) =
  let
    collapesdData = hcollapse $ hcmap (Proxy @PIsData) (K . pforgetData . pdata) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
    idx = pconstant $ toInteger $ hindex xs
   in
    punsafeCoerce $ pforgetData $ pconstrBuiltin # idx #$ builtinList

newtype H s struct = H {unH :: forall r. Term s (PBuiltinList PData) -> (PRec struct s -> Term s r) -> Term s r}

pmatchDataRec ::
  forall (struct :: [S -> Type]) b s.
  All PIsData struct =>
  Term s (PDataRec struct) ->
  (PRec struct s -> Term s b) ->
  Term s b
pmatchDataRec (punsafeCoerce -> x) f =
  let
    go :: forall y ys. PIsData y => H s ys -> H s (y ': ys)
    go (H rest) = H $ \ds cps ->
      let
        tail = ptail # ds
        parsed = pfromData @y $ punsafeCoerce $ phead # ds
       in
        rest tail $ \(PRec rest') ->
          pletSmart parsed $ \parsed' -> cps $ PRec $ parsed' :* rest'
    record = unH $ cpara_SList (Proxy @PIsData) (H $ \_ cps -> cps $ PRec Nil) go
   in
    pletSmart x (`record` f)

newtype StructureHandler s r struct = StructureHandler
  { unSBR ::
      Integer ->
      (PStruct struct s -> Term s r) ->
      NP (K (Integer, Term s r)) struct
  }

-- This is probably general enough to be used for non-Data encoded types
pmatchDataStruct ::
  forall (struct :: [[S -> Type]]) b s.
  All2 PIsData struct =>
  Term s (PDataStruct struct) ->
  (PStruct struct s -> Term s b) ->
  Term s b
pmatchDataStruct (punsafeCoerce -> x) f = unTermCont $ do
  x' <- pletSmartC $ pasConstr # x
  idx <- pletSmartC $ pfstBuiltin # x'
  ds <- pletSmartC $ psndBuiltin # x'

  let
    go :: forall y ys. All PIsData y => StructureHandler s b ys -> StructureHandler s b (y ': ys)
    go (StructureHandler rest) = StructureHandler $ \i cps ->
      let
        handler = pmatchDataRec @y (punsafeCoerce ds) $ \(PRec r) -> cps $ PStruct $ SOP $ Z r
        restHandlers = rest (i + 1) (\(PStruct (SOP sop)) -> cps $ PStruct $ SOP $ S sop)
       in
        K (i, handler) :* restHandlers

    -- This builds "handlers"--that is each cases of SOP data
    -- By building this we can figure out which cases share same computation, hence which branches to group
    handlers' :: StructureHandler s b struct
    handlers' = cpara_SList (Proxy @(All PIsData)) (StructureHandler $ \_ _ -> Nil) go

    handlers :: [(Integer, Term s b)]
    handlers = hcollapse $ unSBR handlers' 0 f

  handlersWithHash :: [(Integer, (Term s b, Dig))] <-
    traverse (\(i, t) -> (\hash -> (i, (t, hash))) <$> hashOpenTerm t) handlers

  let
    -- My deepest apology to whoever is reading this. This basically groups all the handler that yields same results
    -- and sort each group so that smallest group comes first
    groupedHandlers :: [([Integer], Term s b)]
    groupedHandlers =
      sortBy (\g1 g2 -> length (fst g1) `compare` length (fst g2)) $
        (\g -> (fst <$> g, fst $ snd $ head g))
          <$> groupBy (\x1 x2 -> snd (snd x1) == snd (snd x2)) handlersWithHash

  pure $
    let
      -- This one builds chain of #&& condition, making if one per groups
      pgo' :: [([Integer], Term s b)] -> Term s b
      pgo' [(_, t)] = t
      pgo' [(is, t), (_, t')] =
        pif (foldl1 (#&&) $ (\i -> fromInteger i #== idx) <$> is) t t'
      pgo' ((is, t) : rest) =
        pif (foldl1 (#&&) $ (\i -> fromInteger i #== idx) <$> is) t (pgo' rest)
      pgo' [] = error "impossible"

      -- This one builds if one per every entry
      buildIfs :: [Integer] -> Term s b -> (Term s b -> Term s b)
      buildIfs [] _ = id
      buildIfs [_] t = const t
      buildIfs (i : is) t =
        buildIfs is t . pif (fromInteger i #== idx) t

      pgo :: [([Integer], Term s b)] -> Term s b
      pgo [(is, t), (_, t')] = buildIfs is t t'
      pgo ((is, t) : rest) = buildIfs is t $ pgo rest
      pgo [] = error "impossible"

      -- So that GHC doesn't bitch
      _a = pgo'
      _b = pgo
     in
      -- first one seems to be faster
      pgo' groupedHandlers

----------------------------------------------------------------------

data PScottStruct (struct :: [[S -> Type]]) (s :: S) where
  PScottStruct :: All2 PScottRepresentable struct => PStruct struct s -> PScottStruct struct s

data PScottRec (struct :: [S -> Type]) (s :: S) where
  PScottRec :: All PScottRepresentable struct => PRec struct s -> PScottRec struct s

class PScottRepresentable (a :: S -> Type)

data PSOPStruct (struct :: [[S -> Type]]) (s :: S) where
  PSOPStruct :: All2 PSOPRepresentable struct => PStruct struct s -> PSOPStruct struct s

data PSOPRec (struct :: [S -> Type]) (s :: S) where
  PSOPRec :: All PSOPRepresentable struct => PRec struct s -> PSOPRec struct s

class PSOPRepresentable (a :: S -> Type)

----------------------------------------------------------------------

{- $>
import Plutarch.Internal
import Plutarch.Pretty
import Plutarch.Rework
import Prelude
import Generics.SOP
import Plutarch.Prelude
import Debug.Trace
<$
-}

-- $> prettyTermAndCost mempty $ hello

-- $> prettyTermAndCost mempty $ world

-- $> prettyTermAndCost mempty $ pmatchDataRec @FAZ faz (\(PRec (x :* y :* _z :* _)) ->  x + x + y)

-- $> prettyTermAndCost mempty $ pmatchDataRec @FAZ faz (\(PRec (_x :* _y :* _z :* _)) ->  pconstant (5 :: Integer))

type MyRepr = '[ '[PInteger], '[PByteString], '[PInteger, PInteger], '[PInteger, PInteger]]
type MyNestedRepr = '[ '[PStruct MyRepr], '[PByteString], '[PInteger, PInteger]]

test :: PStruct MyRepr s
test = PStruct (SOP (S $ S $ Z ((pconstant 10 :: Term s PInteger) :* (pconstant 12 :: Term s PInteger) :* Nil)))

world :: forall s. Term s PInteger
world =
  let
    a :: Term s (PDataSum '[ '["foo" ':= PInteger], '["foo" ':= PByteString], '["foo" ':= PInteger, "pgo" ':= PInteger]])
    a = punsafeCoerce $ pdataStructureAsData $ pconDataStruct test
    handler :: PDataSum _ s -> Term s PInteger
    handler (PDataSum (Z _x)) = pconstant (1 :: Integer)
    handler (PDataSum (S (Z _x))) = pconstant (3 :: Integer)
    handler _ = pconstant (2 :: Integer)
   in
    pmatch a handler

hello :: forall s. Term s PInteger
hello =
  let
    handler :: PStruct MyRepr s -> Term s PInteger
    handler (PStruct (SOP (Z _x))) = pconstant (1 :: Integer)
    handler (PStruct (SOP (S (Z _x)))) = pconstant (1 :: Integer)
    handler (PStruct (SOP (S (S (Z (_x :* _y :* Nil)))))) = 1
    handler (PStruct (SOP (S (S (S (Z (_x :* _y :* Nil))))))) = 1
    handler _ = undefined
   in
    pmatchDataStruct @MyRepr (pconDataStruct test) handler

type FAZ = '[PInteger, PInteger, PInteger]

faz :: Term s (PDataRec FAZ)
faz = pconDataRec $ PRec (pconstant (1 :: Integer) :* pconstant (2 :: Integer) :* pconstant (4 :: Integer) :* Nil)
