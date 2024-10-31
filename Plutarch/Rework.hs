{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
-- This needs to go later
{-# OPTIONS_GHC -Wwarn #-}

module Plutarch.Rework where

import Generics.SOP

import Generics.SOP qualified as SOP

import GHC.TypeLits

import Data.Functor.Compose

import Control.Arrow
import Control.Monad.Reader
import Data.Coerce
import Data.Kind (Type)
import Data.List (groupBy, sortBy)
import Data.Proxy
import Data.Text qualified as Text
import Data.Void
import GHC.Generics qualified as GHC
import GHC.Word (Word64)
import Plutarch
import Plutarch.Builtin
import Plutarch.DataRepr.Internal
import Plutarch.Internal
import Plutarch.Prelude
import Plutarch.Unsafe

newtype PStruct (struct :: [[S -> Type]]) (s :: S) = PStruct {unPStruct :: SOP (Term s) struct}
newtype PRec (struct :: [S -> Type]) (s :: S) = PRec {unPRec :: NP (Term s) struct}

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
    go (RPlaceHolder hash) = if hash == targetHash then 1 else 0
    go _ = 0

  pure $ f (go rt)

hasErrorTerm :: Term s a -> TermCont s Bool
hasErrorTerm t = TermCont $ \f -> unTermCont $ do
  rt <- getRawTerm t
  let
    go (RLamAbs _ rt) = go rt
    go (RApply rt rts) = go rt || any go rts
    go (RForce rt) = go rt
    go (RDelay rt) = go rt
    go (RHoisted (HoistedTerm _ rt)) = go rt
    go RError = True
    go _ = False

  pure $ f (go rt)

createPlaceholder :: Term s a -> TermCont s (Dig, Term s a)
createPlaceholder t = do
  (hash :: Dig) <- hashOpenTerm t
  pure (hash, Term $ const $ pure $ TermResult (RPlaceHolder hash) [])

-- This needs more thoughts
pletSmart :: Term s a -> (Term s a -> Term s b) -> Term s b
pletSmart = plet

-- unTermCont $ do
-- hasError <- hasErrorTerm t
-- (hash, placeholder) <- createPlaceholder t

-- occurrences <-
--   findOccurrence hash $ f placeholder

-- pure $ case occurrences of
--   0 -> if hasError then plet t f else f t
--   1 -> f t
--   _ -> plet t f

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
        dataRecAsBuiltinList :: Term s (PBuiltinList PData) -> Term s (PDataRec y)
        dataRecAsBuiltinList = punsafeCoerce

        handler = pmatchDataRec @y (dataRecAsBuiltinList ds) $ \(PRec r) -> cps $ PStruct $ SOP $ Z r
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
          <$> groupBy
            (\x1 x2 -> snd (snd x1) == snd (snd x2))
            (sortBy (\(_, (_, h1)) (_, (_, h2)) -> h1 `compare` h2) handlersWithHash)

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
instance PScottRepresentable a

--------------------------------------------------------------------------------------

type ScottFn' :: [PType] -> PType -> PType
type family ScottFn' xs r where
  ScottFn' '[] r = r
  ScottFn' (x ': xs) r = x :--> ScottFn' xs r

type ScottFn :: [PType] -> PType -> PType
type family ScottFn xs r where
  ScottFn '[] r = PDelayed r
  ScottFn xs r = ScottFn' xs r

-- scottList l r = map (flip scottFn r) l
type ScottList :: [[PType]] -> PType -> [PType]
type family ScottList code r where
  ScottList '[] _ = '[]
  ScottList (xs ': xss) r = ScottFn xs r ': ScottList xss r

newtype PLamL' s b as = PLamL' {unPLamL' :: (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)}

-- Explicitly variadic `plam`.
plamL' :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)
plamL' = unPLamL' $ para_SList (PLamL' \f -> f Nil) (\(PLamL' prev) -> PLamL' \f -> plam' \a -> prev \as -> f (a :* as))

newtype PLamL s b as = PLamL {unPLamL :: (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)}

-- `pdelay`s the 0-arity case.
plamL :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)
plamL = unPLamL $ case_SList (PLamL \f -> pdelay $ f Nil) (PLamL plamL')

newtype PAppL' s r as = PAppL' {unPAppL' :: Term s (ScottFn' as r) -> NP (Term s) as -> Term s r}

pappL' :: SListI as => Term s (ScottFn' as c) -> NP (Term s) as -> Term s c
pappL' = unPAppL' $ para_SList (PAppL' \f Nil -> f) (\(PAppL' prev) -> PAppL' \f (x :* xs) -> prev (f # x) xs)

newtype PAppL s r as = PAppL {unPAppL :: Term s (ScottFn as r) -> NP (Term s) as -> Term s r}

pappL :: forall as r s. SListI as => Term s (ScottFn as r) -> NP (Term s) as -> Term s r
pappL = unPAppL $ case_SList (PAppL \f Nil -> pforce f) (PAppL pappL')

newtype PLetL s r as = PLetL {unPLetL :: NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r}

pletL' :: SListI as => NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r
pletL' = unPLetL $ para_SList
  (PLetL \Nil f -> f Nil)
  \(PLetL prev) -> PLetL \(x :* xs) f -> pletSmart x \x' ->
    -- TODO: pletSmart
    prev xs (\xs' -> f (x' :* xs'))

pletL :: All SListI as => SOP (Term s) as -> (SOP (Term s) as -> Term s r) -> Term s r
pletL (SOP (Z x)) f = pletL' x \x' -> f (SOP $ Z x')
pletL (SOP (S xs)) f = pletL (SOP xs) \(SOP xs') -> f (SOP $ S xs')

--------------------------------------------------------------------------------------

-- PScottRec struct <~> Term s (ScottFn' struct r :--> Term s r)

-- Note, we don't have to use delay unit here because when value is unit, that will be the only branch that will get run.
pconScottRec ::
  forall (struct :: [S -> Type]) (s :: S).
  All PScottRepresentable struct =>
  PRec struct s ->
  Term s (PScottRec struct)
pconScottRec (PRec xs) = punsafeCoerce $ plam $ flip pappL' xs

pmatchScottRec ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S).
  All PScottRepresentable struct =>
  Term s (PScottRec struct) ->
  (PRec struct s -> Term s r) ->
  Term s r
pmatchScottRec xs f = punsafeCoerce xs # plamL' (f . PRec)

---------------------------

-- PScottStruct struct <~> Term s (ScottFn (ScottList struct r)) :--> Term s r)

newtype GPCon' s r as = GPCon' {unGPCon' :: NP (Term s) (ScottList as r) -> PStruct as s -> Term s r}

gpcon' :: SListI2 as => NP (Term s) (ScottList as r) -> PStruct as s -> Term s r
gpcon' = unGPCon' $
  cpara_SList
    (Proxy @SListI)
    (GPCon' \Nil -> \case {})
    \(GPCon' prev) -> GPCon' \(arg :* args) -> \case
      (PStruct (SOP (Z x))) -> pappL arg x
      (PStruct (SOP (S xs))) -> prev args (PStruct $ SOP xs)

pconScottStruct ::
  forall (struct :: [[S -> Type]]) (r :: S -> Type) (s :: S).
  ( SListI (ScottList struct r)
  , SListI2 struct
  ) =>
  PStruct struct s ->
  Term s (PScottStruct struct)
pconScottStruct (PStruct xs) =
  pletL xs \(SOP fields) ->
    punsafeCoerce $ plamL \args -> (gpcon' args (PStruct $ SOP fields) :: Term s r)

newtype GPMatch' s r as = GPMatch' {unGPMatch' :: (PStruct as s -> Term s r) -> NP (Term s) (ScottList as r)}

gpmatch' ::
  forall as r s.
  SListI2 as =>
  (PStruct as s -> Term s r) ->
  NP (Term s) (ScottList as r)
gpmatch' = unGPMatch' $ cpara_SList (Proxy @SListI) (GPMatch' (const Nil)) \(GPMatch' prev) -> GPMatch' \f ->
  plamL (\args -> f (PStruct $ SOP $ Z args)) :* prev (\(PStruct (SOP x)) -> f (PStruct $ SOP (S x)))

pmatchScottStruct :: forall struct r s. (SListI (ScottList struct r), SListI2 struct) => Term s (PScottStruct struct) -> (PStruct struct s -> Term s r) -> Term s r
pmatchScottStruct xs f = pappL (punsafeCoerce xs) (gpmatch' f)

----------------------------------------------------------------------

data PSOPStruct (struct :: [[S -> Type]]) (s :: S) where
  PSOPStruct :: All2 PSOPRepresentable struct => PStruct struct s -> PSOPStruct struct s

data PSOPRec (struct :: [S -> Type]) (s :: S) where
  PSOPRec :: All PSOPRepresentable struct => PRec struct s -> PSOPRec struct s

class PSOPRepresentable (a :: S -> Type)

type family PHandlerTy r (struct :: [S -> Type]) :: S -> Type where
  PHandlerTy r '[] = r
  PHandlerTy r (x ': xs) = x :--> PHandlerTy r xs

type family PCaseTy r (struct :: [[S -> Type]]) :: [S -> Type] where
  PCaseTy _ '[] = '[]
  PCaseTy r (x ': xs) = PHandlerTy r x ': PCaseTy r xs

pconSOPRec ::
  forall (struct :: [S -> Type]) (s :: S). SListI struct => PRec struct s -> Term s (PSOPRec struct)
pconSOPRec (PRec xs) = Term $ \i -> do
  ts <- hcollapse <$> htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) xs
  let
    term = RConstr 0 $ fst <$> ts
    deps = mconcat $ snd <$> ts
  pure $ TermResult term deps

newtype MSR s r struct = MSR {unMSR :: (PRec struct s -> Term s r) -> Term s (PHandlerTy r struct)}

sopHandler ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S). SListI struct => (PRec struct s -> Term s r) -> Term s (PHandlerTy r struct)
sopHandler f =
  let
    go :: MSR s r ys -> MSR s r (y ': ys)
    go (MSR rest) = MSR $ \f ->
      plam $ \x -> rest $ \(PRec rest') -> f $ PRec (x :* rest')

    handler :: Term s (PHandlerTy r struct)
    handler = unMSR (para_SList (MSR $ \f -> f $ PRec Nil) go) f
   in
    handler

pmatchSOPRec ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S). SListI struct => Term s (PSOPRec struct) -> (PRec struct s -> Term s r) -> Term s r
pmatchSOPRec xs f = Term $ \i -> do
  (term, deps) <- (getTerm &&& getDeps) <$> asRawTerm xs i
  (handlerTerm, handlerDeps) <- (getTerm &&& getDeps) <$> asRawTerm (sopHandler f) i
  pure $ TermResult (RCase term (pure handlerTerm)) (deps <> handlerDeps)

pconSOPStruct ::
  forall (struct :: [[S -> Type]]) (s :: S). SListI2 struct => PStruct struct s -> Term s (PSOPStruct struct)
pconSOPStruct (PStruct xs) = Term $ \i -> do
  ts <- hcollapse <$> htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) xs
  let
    idx = hindex xs
    term = RConstr (fromIntegral idx) $ fst <$> ts
    deps = mconcat $ snd <$> ts
  pure $ TermResult term deps

newtype MSS s r struct = MSS {unMSS :: (PStruct struct s -> Term s r) -> NP (Term s) (PCaseTy r struct)}

pmatchSOPStruct ::
  forall (struct :: [[S -> Type]]) (r :: S -> Type) (s :: S).
  (SListI2 struct, SListI (PCaseTy r struct)) =>
  Term s (PSOPStruct struct) ->
  (PStruct struct s -> Term s r) ->
  Term s r
pmatchSOPStruct xs h = Term $ \i -> do
  (term, deps) <- (getTerm &&& getDeps) <$> asRawTerm xs i

  let
    go :: forall y ys r s. SListI y => MSS s r ys -> MSS s r (y ': ys)
    go (MSS rest) = MSS $ \f ->
      let
        handler = sopHandler (\(PRec x) -> f $ PStruct $ SOP $ Z x)
        f' (PStruct (SOP x)) = f $ PStruct $ SOP $ S x
       in
        handler :* rest f'

    handlers' :: NP (Term s) (PCaseTy r struct)
    handlers' = unMSS (cpara_SList (Proxy @SListI) (MSS $ const Nil) go) h

  handlers <- hcollapse <$> htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) handlers'
  let
    handlerTerms = fst <$> handlers
    handlerDeps = mconcat $ snd <$> handlers

  pure $ TermResult (RCase term handlerTerms) (deps <> handlerDeps)

-- $> prettyTermAndCost mempty $ matchTestSOP

-- $> plift $ matchTestSOP

----------------------------------------------------------------------

{- $>
import Plutarch.Internal
import Plutarch.Pretty
import Plutarch.Rework
import Prelude
import Generics.SOP
import Plutarch.Prelude
import Debug.Trace
import Plutarch.Maybe
import Plutarch.Internal.Generic
<$
-}

-- -- $> plift $ bazMatch
--
-- -- $> prettyTermAndCost mempty $ bazMatch

-- $> prettyTermAndCost mempty $ fun

-- $> plift $ fun

-- $> prettyTermAndCost mempty $ matchTestScott

-- $> plift $ matchTestData

-- -- $> prettyTermAndCost mempty $ world

-- -- $> prettyTermAndCost mempty $ pmatchDataRec @FAZ faz (\(PRec (x :* y :* _z :* _)) ->  x + x + y)

-- -- $> prettyTermAndCost mempty $ pmatchDataRec @FAZ faz (\(PRec (_x :* _y :* _z :* _)) ->  pconstant (5 :: Integer))

type family UnTermRec (struct :: [Type]) :: [S -> Type] where
  UnTermRec '[] = '[]
  UnTermRec (Term _ a ': rest) = a ': UnTermRec rest

type family UnTermStruct' (struct :: [[Type]]) :: [[S -> Type]] where
  UnTermStruct' '[] = '[]
  UnTermStruct' (x ': rest) = UnTermRec x ': UnTermStruct' rest

type UnTermStruct x = UnTermStruct' (Code x)

data MyPMaybe s a
  = MyJust (Term s a)
  | MyNothing
  | C (Term s PInteger) (Term s PInteger)
  | D (Term s PInteger) (Term s PInteger)
  | E (Term s PInteger) (Term s PBool)
  | F
  deriving stock (GHC.Generic)
instance SOP.Generic (MyPMaybe s a)

type S' = forall s. s

type MyPMaybeRepr a = UnTermStruct (MyPMaybe S' a)

test :: PStruct (MyPMaybeRepr PInteger) s
test = PStruct $ hcoerce $ from $ MyJust (10 :: Term s PInteger) -- (pconstant False)

testData :: Term s (PDataStruct (MyPMaybeRepr PInteger))
testData = pconDataStruct test

testScott :: Term s (PScottStruct (MyPMaybeRepr PInteger))
testScott = pconScottStruct test

testSOP :: Term s (PSOPStruct (MyPMaybeRepr PInteger))
testSOP = pconSOPStruct test

handler :: MyPMaybe s PInteger -> Term s PInteger
handler (MyJust x) = 1 + x
handler MyNothing = 123
handler (C x y) = x + y + x
handler _ = perror

matchTestData :: Term s PInteger
matchTestData = pmatchDataStruct @(MyPMaybeRepr PInteger) testData (handler . to . hcoerce . unPStruct)

matchTestScott :: Term s PInteger
matchTestScott = pmatchScottStruct @(MyPMaybeRepr PInteger) testScott (handler . to . hcoerce . unPStruct)

matchTestSOP :: Term s PInteger
matchTestSOP = pmatchSOPStruct @(MyPMaybeRepr PInteger) testSOP (handler . to . hcoerce . unPStruct)

conversion ::
  forall {struct :: [[S -> Type]]} (s :: S).
  (SListI (ScottList struct (PDataStruct struct)), SListI2 struct, All2 PIsData struct) =>
  Term s (PScottStruct struct) ->
  Term s (PDataStruct struct)
conversion x = pmatchScottStruct x pconDataStruct

fun :: Term s PInteger
fun = pmatchDataStruct @(MyPMaybeRepr PInteger) (conversion testScott) (handler . to . hcoerce . unPStruct)

world :: forall s. Term s PInteger
world =
  let
    a :: Term s (PDataSum '[ '["foo" ':= PInteger], '["foo" ':= PByteString], '["foo" ':= PInteger, "pgo" ':= PInteger]])
    a = punsafeCoerce $ pdataStructureAsData $ pconDataStruct test
    handler :: PDataSum _ s -> Term s PInteger
    handler (PDataSum (Z _x)) = pconstant (2 :: Integer)
    handler (PDataSum (S (Z _x))) = pconstant (1 :: Integer)
    handler (PDataSum (S (S (Z _x)))) = pconstant (1 :: Integer)
    handler _ = pconstant (2 :: Integer)
   in
    pmatch a handler

type FAZ = '[PInteger, PInteger, PInteger]

faz :: Term s (PDataRec FAZ)
faz = pconDataRec $ PRec (pconstant (1 :: Integer) :* pconstant (2 :: Integer) :* pconstant (4 :: Integer) :* Nil)

baz' :: Term s (PScottRec '[])
baz' = pconScottRec $ PRec Nil

baz :: Term s (PScottRec FAZ)
baz = pconScottRec $ PRec (pconstant (1 :: Integer) :* pconstant (2 :: Integer) :* pconstant (4 :: Integer) :* Nil)

sopRec :: Term s (PSOPRec FAZ)
sopRec = pconSOPRec $ PRec (pconstant (1 :: Integer) :* pconstant (2 :: Integer) :* pconstant (4 :: Integer) :* Nil)

sopMatch :: Term s PInteger
sopMatch = pmatchSOPRec sopRec (\(PRec (x :* y :* z :* Nil)) -> x + x + y + z)

bazMatch' :: Term s PInteger
bazMatch' = pmatchScottRec baz' $ const 1

bazMatch :: Term s PInteger
bazMatch = pmatchScottRec baz (\(PRec (x :* y :* z :* Nil)) -> pmatchScottRec baz (\_ -> x + x + y + z))
