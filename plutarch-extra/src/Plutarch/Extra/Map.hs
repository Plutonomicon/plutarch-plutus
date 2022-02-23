{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Map (
  Map (..),
  plookup,
  pinsert,
  filterWithKey,
  findWithDefault,
  adjust,
  mapWithKey,
  mall,
  msingleton,
  unionWith,
  mapFromBuiltin,
  mapToBuiltin,
  mapLTE,
  mapJoin,
  mapSplit,
  mmap,
  mapSub,
  numEq,
) where

import Plutarch.Prelude

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)

import Plutarch.Builtin (PBuiltinMap)
import Plutarch.List (pconvertLists)
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.Extra.Monadic (tcon, tlet, tmatch)
import Plutarch.Extra.Num (PNum)
import Plutarch.Extra.Pair (pairFromBuiltin, pairToBuiltin, pfirst, psecond, psnd)

newtype Map (a :: PType) (b :: PType) (s :: S)
  = Map (Term s (PList (PPair a b)))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving (PlutusType) via (DerivePNewtype (Map a b) (Map' a b))

type Map' a b = PList (PPair a b)

instance (PIsData a, PIsData b) => PIsData (Map a b) where
  pfromData m' = phoistAcyclic (plam $ \m -> mapFromBuiltin #$ pfromData (punsafeCoerce m)) # m'
  pdata m' = phoistAcyclic (plam $ \m -> punsafeCoerce $ pdata (mapToBuiltin # m)) # m'

instance (PEq a, PEq b) => PEq (Map a b) where
  l'' #== r'' =
    phoistAcyclic
      ( plam $ \l r -> unTermCont $ do
          Map l' <- tmatch l
          Map r' <- tmatch r
          pure $ l' #== r'
      )
      # l''
      # r''

mapFromBuiltin :: (PIsData a, PIsData b) => Term s (PBuiltinMap a b :--> Map a b)
mapFromBuiltin = phoistAcyclic $
  plam $ \m ->
    pcon $ Map $ pmap # pairFromBuiltin #$ pconvertLists # m

mapToBuiltin :: (PIsData a, PIsData b) => Term s (Map a b :--> PBuiltinMap a b)
mapToBuiltin = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    Map m' <- tmatch m
    pure $ pconvertLists #$ (pmap # pairToBuiltin # m')

plookup :: (POrd k, PEq k) => Term s (k :--> Map k a :--> PMaybe a)
plookup = phoistAcyclic $
  plam $ \k m -> unTermCont $ do
    Map m' <- tmatch m
    pure $ plookup' # k # m'

plookup' :: (POrd k, PEq k) => Term s (k :--> Map' k a :--> PMaybe a)
plookup' = phoistAcyclic $
  pfix #$ plam $ \f k m ->
    pmatch m $ \case
      PSNil -> pcon PNothing
      PSCons x xs -> pmatch x $ \(PPair k' v) ->
        pif
          (k #== k')
          (pcon $ PJust v)
          ( pif
              (k #< k')
              (pcon PNothing)
              (f # k # xs)
          )

pinsert :: (POrd k, PEq k) => Term s (k :--> a :--> Map k a :--> Map k a)
pinsert = phoistAcyclic $
  pfix #$ plam $ \f k v m ->
    pmatch m $ \(Map m') -> pmatch m' $ \case
      PSNil -> pcon $ Map $ pcons # pcon (PPair k v) # pnil
      PSCons x xs -> pmatch x $ \(PPair k' _v') ->
        pif
          (k #== k')
          (pcon $ Map $ pcons # pcon (PPair k v) # xs)
          ( pif
              (k #< k')
              (pcon $ Map $ pcons # pcon (PPair k v) #$ pcons # x # xs)
              ( unTermCont $ do
                  Map m'' <- tmatch $ f # k # v # pcon (Map xs)
                  tcon $ Map $ pcons # x #$ m''
              )
          )

filterWithKey :: Term s ((k :--> a :--> PBool) :--> Map k a :--> Map k a)
filterWithKey = phoistAcyclic $
  plam $ \f m -> unTermCont $ do
    Map m' <- tmatch m
    let f' = plam (\p -> pmatch p $ \(PPair k v) -> f # k # v)
    tcon $ Map $ pfilter # f' # m'

findWithDefault :: (POrd k, PEq k) => Term s (a :--> k :--> Map k a :--> a)
findWithDefault = phoistAcyclic $
  plam $ \a k m ->
    pmatch (plookup # k # m) $ \case
      PNothing -> a
      PJust v -> v

adjust :: PEq k => Term s ((a :--> a) :--> k :--> Map k a :--> Map k a)
adjust = phoistAcyclic $
  plam $ \f k ->
    mapWithKey # plam (\k' v -> pif (k #== k') (f # v) v)

mapWithKey :: Term s ((k :--> a :--> a) :--> Map k a :--> Map k a)
mapWithKey = phoistAcyclic $
  plam $ \f m -> unTermCont $ do
    Map m' <- tmatch m
    let f' = plam (\p -> pmatch p $ \(PPair k v) -> pcon $ PPair k (f # k # v))
    tcon $ Map $ pmap # f' # m'

-- a PFoldable class would probably be better
-- but afaict (Map a) can't implement PIsListLike
-- so this can't use Plutarch.List all for now

mall :: Term s ((a :--> PBool) :--> Map k a :--> PBool)
mall = phoistAcyclic $
  plam $ \p m -> unTermCont $ do
    Map m' <- tmatch m
    pure $ pall # p #$ pmap # psnd # m'

msingleton :: Term s (k :--> a :--> Map k a)
msingleton = phoistAcyclic $ plam $ \k a -> pcon $ Map $ psingleton # pcon (PPair k a)

unionWith :: (PEq k, POrd k) => Term s ((a :--> a :--> a) :--> Map k a :--> Map k a :--> Map k a)
unionWith = phoistAcyclic $
  plam $ \f l r -> unTermCont $ do
    Map l' <- tmatch l
    Map r' <- tmatch r
    tcon $ Map $ unionWith' # f # l' # r'

unionWith' ::
  (PEq k, POrd k) =>
  Term
    s
    ( (a :--> a :--> a)
        :--> Map' k a
        :--> Map' k a
        :--> Map' k a
    )
unionWith' = phoistAcyclic $
  pfix #$ plam $ \self f l r -> unTermCont $ do
    (x, xs) <- nonEmpty r =<< tmatch l
    (y, ys) <- nonEmpty l =<< tmatch r
    PPair xk xv <- tmatch x
    PPair yk yv <- tmatch y
    pure $
      pif
        (xk #== yk)
        (pcons # pcon (PPair xk (f # xv # yv)) #$ self # f # xs # ys)
        ( pif
            (xk #< yk)
            (pcons # x #$ self # f # xs # r)
            (pcons # y #$ self # f # l # ys)
        )

-- | Terminate with given value on empty list, otherwise continue.
nonEmpty :: Term s r -> PList a s -> TermCont @r s (Term s a, Term s (PList a))
nonEmpty x0 list = TermCont $ \k ->
  case list of
    PSCons x xs -> k (x, xs)
    PSNil -> x0

-- TODO would all (>) (mapSub a b) be better?
mapLTE :: (POrd a, POrd b) => Term s (Map a b :--> Map a b :--> PBool)
mapLTE = phoistAcyclic $
  plam $ \l r -> unTermCont $ do
    Map l' <- tmatch l
    Map r' <- tmatch r
    pure $ mapLTE' # l' # r'

mapLTE' :: (POrd a, POrd b) => Term s (Map' a b :--> Map' a b :--> PBool)
mapLTE' =
  phoistAcyclic $
    pfix #$ plam $ \self l r ->
      pmatch l $ \case
        PSNil -> pcon PTrue
        PSCons x xs ->
          pmatch r $ \case
            PSNil -> pcon PFalse
            PSCons y ys -> unTermCont $ do
              PPair xk xv <- tmatch x
              PPair yk yv <- tmatch y
              pure $
                pif
                  (xk #< yk)
                  (pcon PFalse)
                  $ pif
                    (yk #< xk)
                    (self # xs # r)
                    (xv #<= yv #&& self # xs # ys)

unMap :: Term s (Map a b :--> Map' a b)
unMap = phoistAcyclic $ plam $ \m -> pmatch m $ \(Map m') -> m'

mkMap :: Term s (Map' a b :--> Map a b)
mkMap = phoistAcyclic $
  plam $ \m -> pcon $ Map m

mapJoin :: Term s (Map a (Map b c) :--> Map (PPair a b) c)
mapJoin = phoistAcyclic $ plam $ \m -> pcon $ Map $ mapJoin' # (pmap # (psecond # unMap) # (unMap # m))

mapJoin' :: Term s (Map' a (Map' b c) :--> Map' (PPair a b) c)
mapJoin' =
  phoistAcyclic $
    pfix #$ plam $ \self m ->
      pmatch m $ \case
        PSNil -> pcon PSNil
        PSCons x xs -> unTermCont $ do
          PPair k km <- tmatch x
          km' <- tlet $ pmap # (pfirst # (mkPair # k)) # km
          pure $ pconcat # km' # (self # xs)

mapSplit :: PEq a => Term s (Map (PPair a b) c :--> Map a (Map b c))
mapSplit = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    Map m' <- tmatch m
    pure $ mmap # mkMap #$ mkMap #$ mapSplit' # pcon PNothing # m'

mapSplit' :: PEq a => Term s (PMaybe (PPair a (Map' b c)) :--> Map' (PPair a b) c :--> Map' a (Map' b c))
mapSplit' = phoistAcyclic $
  pfix #$ plam $ \self state m ->
    pmatch m $ \case
      PSNil -> pmatch state $ \case
        PNothing -> pnil
        PJust state' -> psingleton #$ psecond # preverse # state'
      PSCons x xs -> unTermCont $ do
        PPair ab c <- tmatch x
        PPair a b <- tmatch ab
        pure $
          pmatch state $ \case
            PNothing -> unTermCont $ do
              state' <- tlet $ pcon $ PJust $ pcon $ PPair a (psingleton #$ pcon (PPair b c))
              pure $ self # state' # xs
            PJust state' -> unTermCont $ do
              PPair k m' <- tmatch state'
              pure $
                pif
                  (k #== a)
                  ( unTermCont $ do
                      state'' <- tlet $ pcon $ PJust $ pcon $ PPair k (pcons # pcon (PPair b c) # m')
                      pure $ self # state'' # xs
                  )
                  $ pcons # (psecond # preverse # state') #$ self # pcon PNothing # m

mkPair :: Term s (a :--> b :--> PPair a b)
mkPair = phoistAcyclic $ plam $ \a b -> pcon $ PPair a b

mmap :: Term s ((b :--> b') :--> Map a b :--> Map a b')
mmap = phoistAcyclic $
  plam $ \f m -> unTermCont $ do
    Map m' <- tmatch m
    tcon $ Map $ pmap # (psecond # f) # m'

-- TODO would `unionWith (+) a (map negate b)`  be better?
mapSub :: (PEq a, POrd a, PNum b) => Term s (Map a b :--> Map a b :--> Map a b)
mapSub = phoistAcyclic $
  plam $ \l r -> unTermCont $ do
    Map l' <- tmatch l
    Map r' <- tmatch r
    tcon $ Map $ mapSub' # l' # r'

mapSub' :: (PEq a, POrd a, PNum b) => Term s (Map' a b :--> Map' a b :--> Map' a b)
mapSub' = phoistAcyclic $
  pfix #$ plam $ \self l r ->
    pmatch l $ \case
      PSNil -> pmap # (psecond # plam negate) # r
      PSCons x xs ->
        pmatch r $ \case
          PSNil -> l
          PSCons y ys -> unTermCont $ do
            PPair lk lv <- tmatch x
            PPair rk rv <- tmatch y
            pure $
              pif
                (lk #< rk)
                (pcons # x # (self # xs # r))
                $ pif
                  (lk #== rk)
                  (pcons # pcon (PPair lk (lv - rv)) # (self # xs # ys))
                  (pcons # pcon (PPair rk (negate rv)) # (self # l # ys))

numEq :: (PEq a, PEq b, PNum b) => Term s (Map a b :--> Map a b :--> PBool)
numEq = phoistAcyclic $
  plam $ \l r -> unTermCont $ do
    Map l' <- tmatch l
    Map r' <- tmatch r
    pure $
      numEq' # l' # r'

numEq' :: (PEq a, PEq b, PNum b) => Term s (Map' a b :--> Map' a b :--> PBool)
numEq' = phoistAcyclic $
  pfix #$ plam $ \self l r -> unTermCont $ do
    (x, xs) <- nonEmpty (zeros # r) =<< tmatch l
    (y, ys) <- nonEmpty (zeros # l) =<< tmatch r
    PPair xk xv <- tmatch x
    PPair yk yv <- tmatch y
    pure $
      pif
        (xk #== yk)
        ((xv #== yv) #&& (self # xs # ys))
        ( pif
            (xv #== 0)
            (self # xs # r)
            ((yv #== 0) #&& (self # l # ys))
        )

zeros :: (PEq b, PNum b) => Term s (Map' a b :--> PBool)
zeros =
  phoistAcyclic $
    pall # plam (\x -> psnd # x #== 0)
