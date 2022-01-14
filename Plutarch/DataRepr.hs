{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Plutarch.DataRepr (PDataRepr, punDataRepr, pindexDataRepr, pmatchDataRepr, DataReprHandlers (..), PDataList, pdhead, pdtail, PIsDataRepr (..), PIsDataReprInstances (..), punsafeIndex, pindexDataList) where

import Data.List (groupBy, maximumBy, sortOn)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownNat, Nat, natVal, type (-))
import Plutarch (Dig, PMatch, TermCont, hashOpenTerm, punsafeBuiltin, punsafeCoerce, runTermCont)
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData,
  pasConstr,
  pdata,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Integer (PInteger)
import Plutarch.Lift (PLifted, PLiftedRepr, PUnsafeLiftDecl, pliftFromRepr, pliftToRepr)
import Plutarch.List (punsafeIndex)
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
  phoistAcyclic $
    punsafeCoerce $
      punsafeIndex @PBuiltinList @PData # ind
  where
    ind :: Term s PInteger
    ind = fromInteger $ toInteger $ natVal n

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
    applyHandlers args (DRHCons handler rest) = handler (punsafeCoerce args) : applyHandlers args rest

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

newtype PIsDataReprInstances (a :: k -> Type) (h :: Type) (s :: k) = PIsDataReprInstances (a s)

class (PMatch a, PIsData a) => PIsDataRepr (a :: k -> Type) where
  type PIsDataReprRepr a :: [[k -> Type]]
  pmatchRepr :: forall s b. Term s (PDataRepr (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b

instance PIsDataRepr a => PIsData (PIsDataReprInstances a h) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

instance PIsDataRepr a => PMatch (PIsDataReprInstances a h) where
  pmatch x f = pmatchRepr (punsafeCoerce x) (f . PIsDataReprInstances)

instance {-# OVERLAPPABLE #-} (Ledger.FromData h, Ledger.ToData h, PIsData p) => PUnsafeLiftDecl h (PIsDataReprInstances p h) where
  type PLifted (PIsDataReprInstances p h) = h
  type PLiftedRepr (PIsDataReprInstances p h) = Ledger.Data
  pliftToRepr = Ledger.toData
  pliftFromRepr = Ledger.fromData
