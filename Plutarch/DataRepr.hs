{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Plutarch.DataRepr (PDataRepr, SNat (..), punDataRepr, pindexDataRepr, pmatchDataRepr) where

import Plutarch (punsafeCoerce)
import Plutarch.Bool (pif, (£==))
import Plutarch.Builtin (PBuiltinList, PData, pasConstr, pfstBuiltin, psndBuiltin)
import Plutarch.BuiltinHList (PBuiltinHList)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude

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

punDataRepr :: Term s (PDataRepr '[def] :--> PBuiltinHList def)
punDataRepr = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr £$ pasData t) $ \d ->
      (punsafeCoerce $ psndBuiltin £ d :: Term _ (PBuiltinHList def))

pindexDataRepr :: SNat n -> Term s (PDataRepr (def : defs) :--> PBuiltinHList (IndexList n (def : defs)))
pindexDataRepr n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr £$ pasData t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin £ d
       in pif
            (i £== (fromInteger . natToInteger . unSingleton $ n))
            (punsafeCoerce $ psndBuiltin £ d :: Term _ (PBuiltinHList _))
            perror

type family LengthList (l :: [k]) :: Nat
type instance LengthList '[] = 'N
type instance LengthList (x : xs) = 'S (LengthList xs)

data DataReprHandlers (out :: k -> Type) (def :: [[k -> Type]]) (s :: k) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: Maybe (Term s (PBuiltinHList def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

-- FIXME: remove unnecessary final perror if all cases are matched
punsafeMatchDataRepr' :: Integer -> DataReprHandlers out defs s -> Term s PInteger -> Term s (PBuiltinList PData) -> Term s out
punsafeMatchDataRepr' _ DRHNil _ _ = perror
punsafeMatchDataRepr' idx (DRHCons Nothing rest) constr args = punsafeMatchDataRepr' (idx + 1) rest constr args
punsafeMatchDataRepr' idx (DRHCons (Just handler) rest) constr args =
  pif
    (fromInteger idx £== constr)
    (handler $ punsafeCoerce args)
    $ punsafeMatchDataRepr' (idx + 1) rest constr args

pmatchDataRepr :: DataReprHandlers out defs s -> Term s (PDataRepr defs) -> Term s out
pmatchDataRepr handlers d =
  let d' = pasConstr £$ pasData d
   in punsafeMatchDataRepr'
        0
        handlers
        (pfstBuiltin £ d')
        (psndBuiltin £ d')
