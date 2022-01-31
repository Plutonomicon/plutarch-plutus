{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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
  type PLabelIndex,
  type PUnLabel,
  PIsDataRepr (..),
  PIsDataReprInstances (..),
  pindexDataRecord,
  pdropDataRecord,
  DerivePConstantViaData (..),
  pasDataSum,
) where

import Data.Kind (Type)
import Data.List (groupBy, maximumBy, sortOn)
import Data.Proxy (Proxy)
import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:<>:)),
  KnownNat,
  Nat,
  Symbol,
  TypeError,
  natVal,
  type (+),
 )
import Generics.SOP (Code, Generic, I (I), NP (Nil, (:*)), SOP (SOP), to)
import Plutarch (
  Dig,
  PMatch,
  PType,
  S,
  Term,
  perror,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (#$),
  type (:-->),
 )
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
import Plutarch.DataRepr.Internal.Generic (MkSum (mkSum))
import Plutarch.DataRepr.Internal.HList (type Drop, type IndexList)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (S (SI))
import Plutarch.Internal.TermCont (TermCont (runTermCont), hashOpenTerm)
import Plutarch.Lift (PConstant, PConstantRepr, PConstanted, PLift, pconstantFromRepr, pconstantToRepr)
import Plutarch.List (pdrop, ptryIndex)
import Plutarch.Unsafe (punsafeCoerce)
import qualified Plutus.V1.Ledger.Api as Ledger

{- | A "record" of `exists a. PAsData a`. The underlying representation is
 `PBuiltinList PData`.
-}
data PDataRecord (as :: [PLabeledType]) (s :: S)

data PLabeledType = Symbol := PType

{- Get the product types of a data record sum constructor
-}
type PDataRecordFields :: [Type] -> [PLabeledType]
type family PDataRecordFields as where
  PDataRecordFields '[] = '[]
  PDataRecordFields '[Term s (PDataRecord fs)] = fs
  PDataRecordFields '[t] = TypeError ( 'Text "Expected PDataRecord" ':<>: 'Text "but got" ':<>: 'ShowType t)
  PDataRecordFields ts = TypeError ( 'Text "Expected none or PDataRecord" ':<>: 'Text "but got" ':<>: 'ShowType ts)

{- Return the table of data records for a sum type.

NOTE: Unfortunately we can't write a generic FMap due to ghc's arity limitations.
-}
type PDataRecordFields2 :: [[Type]] -> [[PLabeledType]]
type family PDataRecordFields2 as where
  PDataRecordFields2 '[] = '[]
  PDataRecordFields2 (a ': as) = PDataRecordFields a ': PDataRecordFields2 as

type family PLabelIndex (name :: Symbol) (as :: [PLabeledType]) :: Nat where
  PLabelIndex name ((name ':= a) ': as) = 0
  PLabelIndex name (_' : as) = (PLabelIndex name as) + 1

type family PUnLabel (a :: PLabeledType) :: PType where
  PUnLabel (name ':= a) = a

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
pindexDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord as) -> Term s (PAsData (PUnLabel (IndexList n as)))
pindexDataRecord n xs =
  punsafeCoerce $
    ptryIndex @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

-- | Safely drop the first n items of a 'PDataRecord'.
pdropDataRecord :: (KnownNat n) => Proxy n -> Term s (PDataRecord xs) -> Term s (PDataRecord (Drop n xs))
pdropDataRecord n xs =
  punsafeCoerce $
    pdrop @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

-- | This is used to define the handlers for 'pmatchDataSum'.
data DataReprHandlers (out :: PType) (defs :: [[PLabeledType]]) (s :: S) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataRecord def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def : defs) s

-- | Pattern match on a 'PDataSum' manually. The common case only appears once in the generated code.
pmatchDataSum :: Term s (PDataSum defs) -> DataReprHandlers out defs s -> Term s out
pmatchDataSum d (DRHCons handler DRHNil) = handler $ punDataSum # d
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
  type PIsDataReprRepr a = PDataRecordFields2 (Code (a 'SI))

  pmatchRepr :: forall s b. Term s (PDataSum (PIsDataReprRepr a)) -> (a s -> Term s b) -> Term s b
  default pmatchRepr ::
    forall s b code.
    ( code ~ Code (a s)
    , PDataRecordFields2 code ~ PIsDataReprRepr a
    , MkDataReprHandler s a 0 code
    ) =>
    Term s (PDataSum (PIsDataReprRepr a)) ->
    (a s -> Term s b) ->
    Term s b
  pmatchRepr dat =
    pmatchDataSum dat . mkDataReprHandler @s @a @0 @code

-- | Create a `DataReprhandlers` starting from `n`th sum constructor
class MkDataReprHandler (s :: S) (a :: PType) (n :: Nat) (rest :: [[Type]]) where
  mkDataReprHandler :: forall out. (a s -> Term s out) -> DataReprHandlers out (PDataRecordFields2 rest) s

instance MkDataReprHandler s a n '[] where
  mkDataReprHandler _ = DRHNil

instance
  ( Generic (a s)
  , code ~ Code (a s)
  , r ~ IndexList n code
  , r ~ '[Term s (PDataRecord fs)]
  , MkSum n code
  , MkDataReprHandler s a (n + 1) rs
  ) =>
  MkDataReprHandler s a n (r ': rs)
  where
  mkDataReprHandler f =
    DRHCons (f . to . mkSOP . mkProduct) $
      mkDataReprHandler @s @a @(n + 1) @rs f
    where
      mkProduct :: Term s (PDataRecord fs) -> NP I r
      mkProduct x = I x :* Nil
      mkSOP :: NP I r -> SOP I (Code (a s))
      mkSOP = SOP . mkSum @n @code

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
