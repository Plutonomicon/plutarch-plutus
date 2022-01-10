{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.DataRepr 
  (PDataRepr, punDataRepr, pindexDataRepr, pmatchDataRepr, DataReprHandlers (..), PDataList, pdhead, pdtail, PIsDataRepr (..), PIsDataReprInstances (..), punsafeIndex, pindexDataList, PLiftVia (..)) where

import GHC.TypeLits (Nat, KnownNat, natVal, type (-))
import Data.Proxy (Proxy)
import Data.Coerce (Coercible, coerce)
import Data.List (groupBy, maximumBy, sortOn)
import Plutarch (Dig, PMatch, TermCont, hashOpenTerm, punsafeBuiltin, punsafeCoerce, runTermCont, PlutusType (..), ClosedTerm)
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin 
  (PAsData, PBuiltinList, PData, PIsData, pasConstr, pdata, pfromData, pfstBuiltin, psndBuiltin, ptailBuiltin, pheadBuiltin)
import Plutarch.Integer (PInteger)
import Plutarch.Lift
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Ledger
import qualified PlutusCore as PLC


data PDataList (as :: [k -> Type]) (s :: k)

pdhead :: Term s (PDataList (a : as) :--> PAsData a)
pdhead = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

pdtail :: Term s (PDataList (a : as) :--> PDataList as)
pdtail = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

type PDataRepr :: [[k -> Type]] -> k -> Type
data PDataRepr (defs :: [[k -> Type]]) (s :: k)

pasData :: Term s (PDataRepr _) -> Term s PData
pasData = punsafeCoerce

type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

punDataRepr :: Term s (PDataRepr '[def] :--> PDataList def)
punDataRepr = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataList def))

pindexDataRepr :: (KnownNat n) => Proxy n -> Term s (PDataRepr (def : defs) :--> PDataList (IndexList n (def : defs)))
pindexDataRepr n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin # d
       in pif
            (i #== (fromInteger $ toInteger $ natVal $ n))
            (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataList _))
            perror

-- | Safely index a DataList
pindexDataList :: (KnownNat n) => Proxy n -> Term s (PDataList xs :--> PAsData (IndexList n xs))
pindexDataList n = 
  phoistAcyclic $ punsafeCoerce $ 
    punsafeIndex # ind
  where
    ind :: Term s PInteger
    ind = fromInteger $ toInteger $ natVal n

{- | 
  Unsafely index a BuiltinList, failing if
  the index is out of bounds.
-}
punsafeIndex :: Term s (PInteger :--> PBuiltinList a :--> a)
punsafeIndex = phoistAcyclic $
  pfix #$ plam
    \self n xs ->
      pif
        (n #== 0)
        (pheadBuiltin # xs)
        (self # (n - 1) #$ ptailBuiltin # xs)

--type family LengthList (l :: [k]) :: Nat
--type instance LengthList '[] = 'N
--type instance LengthList (x : xs) = 'S (LengthList xs)

data DataReprHandlers (out :: k -> Type) (def :: [[k -> Type]]) (s :: k) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataList def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

pmatchDataRepr :: Term s (PDataRepr (def : defs)) -> DataReprHandlers out (def : defs) s -> Term s out
pmatchDataRepr d handlers =
  plet (pasConstr #$ pasData d) $ \d' ->
    plet (pfstBuiltin # d') $ \constr ->
      plet (psndBuiltin # d') $ \args ->
        let handlers' = applyHandlers args handlers
         in runTermCont (findCommon handlers') $ \common ->
              go
                common
                0
                handlers'
                constr
  where
    hashHandlers :: [Term s out] -> TermCont s [(Dig, Term s out)]
    hashHandlers [] = pure []
    hashHandlers (handler : rest) = do
      hash <- hashOpenTerm handler
      hashes <- hashHandlers rest
      pure $ (hash, handler) : hashes

    findCommon :: [Term s out] -> TermCont s (Dig, Term s out)
    findCommon handlers = do
      l <- hashHandlers handlers
      pure $ head . maximumBy (\x y -> length x `compare` length y) . groupBy (\x y -> fst x == fst y) . sortOn fst $ l

    applyHandlers :: Term s (PBuiltinList PData) -> DataReprHandlers out defs s -> [Term s out]
    applyHandlers _ DRHNil = []
    applyHandlers args (DRHCons handler rest) = (handler $ punsafeCoerce args) : applyHandlers args rest

    go ::
      (Dig, Term s out) ->
      Integer ->
      [Term s out] ->
      Term s PInteger ->
      Term s out
    go common _ [] _ = snd common
    go common idx (handler : rest) constr =
      runTermCont (hashOpenTerm handler) $ \hhash ->
        if hhash == fst common
          then go common (idx + 1) rest constr
          else
            pif
              (fromInteger idx #== constr)
              handler
              $ go common (idx + 1) rest constr

newtype PIsDataReprInstances a h s = PIsDataReprInstances (a s)

class (PMatch a, PIsData a) => PIsDataRepr (a :: k -> Type) where
  type PIsDataReprRepr a :: [[k -> Type]]
  pmatchRepr :: forall s b. Term s (PDataRepr (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b

instance PIsDataRepr a => PIsData (PIsDataReprInstances a h) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

instance PIsDataRepr a => PMatch (PIsDataReprInstances a h) where
  pmatch x f = pmatchRepr (punsafeCoerce x) (f . PIsDataReprInstances)

instance (Ledger.FromData h, Ledger.ToData h, PIsData p) => PLift (PIsDataReprInstances p h) where
  type PHaskellType (PIsDataReprInstances p h) = h
  pconstant' =
    punsafeCoerce . pconstant @PData . Ledger.toData
  plift' t = do
    h <- plift' @_ @PData (punsafeCoerce t)
    maybeToRight "Failed to decode data" $ Ledger.fromData h
    where
      maybeToRight e = maybe (Left e) Right

{- | 
  DerivingVia wrapper for deriving `PLift` instances
  via the wrapped type, while lifting to a coercible Haskell type.

-}
newtype PLiftVia (p :: k -> Type) (h :: Type) s = PLiftVia (p s)

instance 
  ( PLift p
  , Coercible (PHaskellType p) h
  ) => PLift (PLiftVia p h) where
  type PHaskellType (PLiftVia p h) = h

  pconstant' :: h -> Term s (PLiftVia p h)
  pconstant' x = punsafeCoerce $ pconstant @p (coerce x)

  plift' :: ClosedTerm (PLiftVia p h) -> Either LiftError h
  plift' t = coerce $ plift' t'
    where 
      t' :: ClosedTerm p 
      t' = punsafeCoerce t

instance (Coercible (p s) (Term s p)) => PlutusType (PLiftVia p h) where
  type PInner (PLiftVia p h) = p
  pcon' (PLiftVia x) = coerce x  
  pmatch' t f = f $ PLiftVia $ coerce t