{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Plutarch.DataRepr (PDataRepr, SNat (..), punDataRepr, pindexDataRepr, pmatchDataRepr, DataReprHandlers (..), PDataList, pdhead, pdtail, PIsDataRepr (..), PIsDataReprInstances (..)) where

import Plutarch (PMatch, punsafeBuiltin, punsafeCoerce)
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (PBuiltinList, PData, PIsData, pasConstr, pdata, pfromData, pfstBuiltin, psndBuiltin)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PDataList (as :: [k -> Type]) (s :: k)

pdhead :: Term s (PDataList (a : as) :--> a)
pdhead = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

pdtail :: Term s (PDataList (a : as) :--> PDataList as)
pdtail = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

type PDataRepr :: [[k -> Type]] -> k -> Type
data PDataRepr (defs :: [[k -> Type]]) (s :: k)

pasData :: Term s (PDataRepr _) -> Term s PData
pasData = punsafeCoerce

data Nat = N | S Nat

data SNat :: Nat -> Type where
  SN :: SNat 'N
  SS :: SNat n -> SNat ( 'S n)

unSingleton :: SNat n -> Nat
unSingleton SN = N
unSingleton (SS n) = S $ unSingleton n

natToInteger :: Nat -> Integer
natToInteger N = 0
natToInteger (S n) = 1 + natToInteger n

type family IndexList (n :: Nat) (l :: [k]) :: k
type instance IndexList 'N '[x] = x
type instance IndexList ( 'S n) (x : xs) = IndexList n xs

punDataRepr :: Term s (PDataRepr '[def] :--> PDataList def)
punDataRepr = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataList def))

pindexDataRepr :: SNat n -> Term s (PDataRepr (def : defs) :--> PDataList (IndexList n (def : defs)))
pindexDataRepr n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin # d
       in pif
            (i #== (fromInteger . natToInteger . unSingleton $ n))
            (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataList _))
            perror

type family LengthList (l :: [k]) :: Nat
type instance LengthList '[] = 'N
type instance LengthList (x : xs) = 'S (LengthList xs)

data DataReprHandlers (out :: k -> Type) (def :: [[k -> Type]]) (s :: k) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataList def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

punsafeMatchDataRepr' ::
  Integer ->
  DataReprHandlers out (def : defs) s ->
  Term s PInteger ->
  Term s (PBuiltinList PData) ->
  Term s out
punsafeMatchDataRepr' _ (DRHCons handler DRHNil) _ args = handler $ punsafeCoerce args
punsafeMatchDataRepr' idx (DRHCons handler rest@DRHCons {}) constr args =
  pif
    (fromInteger idx #== constr)
    (handler $ punsafeCoerce args)
    $ punsafeMatchDataRepr' (idx + 1) rest constr args

-- FIXME: Collapse most common case
pmatchDataRepr :: Term s (PDataRepr (def : defs)) -> DataReprHandlers out (def : defs) s -> Term s out
pmatchDataRepr d handlers =
  plet (pasConstr #$ pasData d) $ \d' ->
    punsafeMatchDataRepr'
      0
      handlers
      (pfstBuiltin # d')
      (psndBuiltin # d')

newtype PIsDataReprInstances a s = PIsDataReprInstances (a s)

class (PMatch a, PIsData a) => PIsDataRepr (a :: k -> Type) where
  type PIsDataReprRepr a :: [[k -> Type]]
  pmatchRepr :: forall s b. Term s (PDataRepr (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b

instance PIsDataRepr a => PIsData (PIsDataReprInstances a) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

instance PIsDataRepr a => PMatch (PIsDataReprInstances a) where
  pmatch x f = pmatchRepr (punsafeCoerce x) (f . PIsDataReprInstances)
