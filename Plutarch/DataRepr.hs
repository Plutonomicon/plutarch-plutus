{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Plutarch.DataRepr (
  PDataRepr,
  punDataRepr,
  pindexDataRepr,
  pmatchDataRepr,
  DataReprHandlers (..),
  PDataRecord,
  PLabeled (..),
  type PTypes,
  type PNames,
  pdhead,
  pdtail,
  PIsDataRepr (..),
  PIsDataReprInstances (..),
  pindexDataRecord,
  pindexDataRecord',
  pdropDataRecord,
  DerivePConstantViaData (..),
) where

import Data.List (groupBy, maximumBy, sortOn)
import Data.Proxy (Proxy)
import GHC.TypeLits (ErrorMessage (Text), KnownNat, Symbol, TypeError, natVal)
import Generics.SOP (Code)
import Numeric.Natural (Natural)
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
import Plutarch.Field.HList (type Drop, type IndexList)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (S (SI))
import Plutarch.Lift (PConstant, PConstantRepr, PConstanted, PLift, pconstantFromRepr, pconstantToRepr)
import Plutarch.List (pdrop, punsafeIndex, punsafeIndex')
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Ledger
import qualified PlutusCore as PLC

data PDataRecord (as :: [PLabeled]) (s :: S)

data PLabeled = Symbol := PType

pdhead :: Term s (PDataRecord ((l ':= a) : as) :--> PAsData a)
pdhead = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

pdtail :: Term s (PDataRecord (a ': as) :--> PDataRecord as)
pdtail = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

type family PTypes (as :: [PLabeled]) :: [PType] where
  PTypes '[] = '[]
  PTypes ((l ':= t) ': as) = t ': (PTypes as)

type family PNames (as :: [PLabeled]) :: [Symbol] where
  PNames '[] = '[]
  PNames ((l ':= t) ': as) = l ': (PNames as)

type family GetPDataRecordArgs (a :: [[Type]]) :: [[PLabeled]] where
  GetPDataRecordArgs xs = ToPLabeled2 xs

type ToPLabeled :: [Type] -> [PLabeled]
type family ToPLabeled as where
  ToPLabeled '[] = '[]
  ToPLabeled '[Term s (PDataRecord fs)] = fs
  ToPLabeled '[_] = TypeError ( 'Text "Expected PDataRecord")
  ToPLabeled _ = TypeError ( 'Text "Must have 0 or 1 argument in sum constructor")

-- Unfortunately we can't write a generic FMap due to ghc's arity limitations.
type ToPLabeled2 :: [[Type]] -> [[PLabeled]]
type family ToPLabeled2 as where
  ToPLabeled2 '[] = '[]
  ToPLabeled2 (a ': as) = ToPLabeled a ': ToPLabeled2 as

type PDataRepr :: [[PLabeled]] -> PType
data PDataRepr (defs :: [[PLabeled]]) (s :: S)

pasData :: Term s (PDataRepr _) -> Term s PData
pasData = punsafeCoerce

punDataRepr :: Term s (PDataRepr '[def] :--> PDataRecord def)
punDataRepr = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataRecord def))

pindexDataRepr :: (KnownNat n) => Proxy n -> Term s (PDataRepr (def : defs) :--> PDataRecord (IndexList n (def : defs)))
pindexDataRepr n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pasData t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin # d
       in pif
            (i #== fromInteger (natVal n))
            (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataRecord _))
            perror

-- | Safely index a DataRecord
pindexDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs :--> PAsData (IndexList n (PTypes xs)))
pindexDataRecord n =
  phoistAcyclic $
    punsafeCoerce $
      punsafeIndex @PBuiltinList @PData # ind
  where
    ind :: Term s PInteger
    ind = fromInteger $ natVal n

-- | Version of 'pindexDataRecord' using repeated applications of 'ptail'.
pindexDataRecord' :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs) -> Term s (PAsData (IndexList n (PTypes xs)))
pindexDataRecord' n xs =
  punsafeCoerce $
    punsafeIndex' @PBuiltinList @PData ind (punsafeCoerce xs)
  where
    ind :: Natural
    ind = fromInteger $ natVal n

-- | Safely drop the first n items of a PDataRecord.
pdropDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs) -> Term s (PDataRecord (Drop n xs))
pdropDataRecord n xs =
  punsafeCoerce $
    pdrop @PBuiltinList @PData ind (punsafeCoerce xs)
  where
    ind :: Natural
    ind = fromInteger $ natVal n

data DataReprHandlers (out :: PType) (def :: [[PLabeled]]) (s :: S) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataRecord def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

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

newtype PIsDataReprInstances (a :: PType) (s :: S) = PIsDataReprInstances (a s)

class (PMatch a, PIsData a) => PIsDataRepr (a :: PType) where
  type PIsDataReprRepr a :: [[PLabeled]]
  type PIsDataReprRepr a = GetPDataRecordArgs (Code (a 'SI))
  pmatchRepr :: forall s b. Term s (PDataRepr (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b

instance PIsDataRepr a => PIsData (PIsDataReprInstances a) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

instance PIsDataRepr a => PMatch (PIsDataReprInstances a) where
  pmatch x f = pmatchRepr (punsafeCoerce x) (f . PIsDataReprInstances)

newtype DerivePConstantViaData (h :: Type) (p :: PType) = DerivePConstantViaData h

instance (PIsDataRepr p, PLift p, Ledger.FromData h, Ledger.ToData h) => PConstant (DerivePConstantViaData h p) where
  type PConstantRepr (DerivePConstantViaData h p) = Ledger.Data
  type PConstanted (DerivePConstantViaData h p) = p
  pconstantToRepr (DerivePConstantViaData x) = Ledger.toData x
  pconstantFromRepr x = DerivePConstantViaData <$> Ledger.fromData x
