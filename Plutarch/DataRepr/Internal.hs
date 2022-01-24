{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Plutarch.DataRepr.Internal (
  PDataSum,
  punDataSum,
  ptryIndexDataSum,
  pmatchDataSum,
  DataReprHandlers (..),
  PDataRecord,
  PLabeledType (..),
  type PUnLabel,
  type PLabel,
  PIsDataRepr (..),
  PIsDataReprInstances (..),
  pindexDataRecord,
  pdropDataRecord,
  DerivePConstantViaData (..),
  pasDataSum,
) where

import Data.List (groupBy, maximumBy, sortOn)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownNat, Symbol, natVal)
import Plutarch (Dig, PMatch, TermCont, hashOpenTerm, punsafeCoerce, runTermCont)
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData,
  pasConstr,
  pdata,
  pforgetData,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.DataRepr.Internal.HList (type Drop, type IndexList)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (PConstant, PConstantRepr, PConstanted, PLift, pconstantFromRepr, pconstantToRepr)
import Plutarch.List (pdrop, ptryIndex)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Ledger

{- | A "record" of `exists a. PAsData a`. The underlying representation is
 `PBuiltinList PData`.
-}
data PDataRecord (as :: [PLabeledType]) (s :: S)

data PLabeledType = Symbol := PType

type family PUnLabel (as :: [PLabeledType]) :: [PType] where
  PUnLabel '[] = '[]
  PUnLabel ((l ':= t) ': as) = t ': (PUnLabel as)

type family PLabel (as :: [PLabeledType]) :: [Symbol] where
  PLabel '[] = '[]
  PLabel ((l ':= t) ': as) = l ': (PLabel as)

{- | A sum of 'PDataRecord's. The underlying representation is the `PDataConstr` constructor,
 where the integer is the index of the variant and the list is the record.

 This is how most data structures are stored on-chain.
-}
type PDataSum :: [[PLabeledType]] -> PType
data PDataSum (defs :: [[PLabeledType]]) (s :: S)

instance PIsData (PDataSum defs) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

-- | If there is only a single variant, then we can safely extract it.
punDataSum :: Term s (PDataSum '[def] :--> PDataRecord def)
punDataSum = phoistAcyclic $
  plam $ \t ->
    (punsafeCoerce $ psndBuiltin # (pasConstr #$ pforgetData $ pdata t) :: Term _ (PDataRecord def))

-- | Try getting the nth variant. Errs if it's another variant.
ptryIndexDataSum :: (KnownNat n) => Proxy n -> Term s (PDataSum (def : defs) :--> PDataRecord (IndexList n (def : defs)))
ptryIndexDataSum n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pforgetData $ pdata t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin # d
       in pif
            (i #== fromInteger (natVal n))
            (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataRecord _))
            perror

-- | Safely index a 'PDataRecord'.
pindexDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs) -> Term s (PAsData (IndexList n (PUnLabel xs)))
pindexDataRecord n xs =
  punsafeCoerce $
    ptryIndex @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

-- | Safely drop the first n items of a 'PDataRecord'.
pdropDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs) -> Term s (PDataRecord (Drop n xs))
pdropDataRecord n xs =
  punsafeCoerce $
    pdrop @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

-- | This is used to define the handlers for 'pmatchDataSum'.
data DataReprHandlers (out :: PType) (def :: [[PLabeledType]]) (s :: S) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataRecord def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

-- | Pattern match on a 'PDataSum' manually. The common case only appears once in the generated code.
pmatchDataSum :: Term s (PDataSum (def : defs)) -> DataReprHandlers out (def : defs) s -> Term s out
pmatchDataSum d handlers =
  plet (pasConstr #$ pforgetData $ pdata d) $ \d' ->
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

{- | Use this for implementing the necessary instances for getting the `Data` representation.
 You must implement 'PIsDataRepr' to use this.
-}
newtype PIsDataReprInstances (a :: PType) (s :: S) = PIsDataReprInstances (a s)

class (PMatch a, PIsData a) => PIsDataRepr (a :: PType) where
  type PIsDataReprRepr a :: [[PLabeledType]]
  pmatchRepr :: forall s b. Term s (PDataSum (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b

pasDataSum :: PIsDataRepr a => Term s a -> Term s (PDataSum (PIsDataReprRepr a))
pasDataSum = punsafeCoerce

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
