{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.DataRepr.Internal (
  PDataSum (..),
  punDataSum,
  ptryIndexDataSum,
  pdcons,
  pdnil,
  DataReprHandlers (..),
  PDataRecord (..),
  PLabeledType (..),
  type PLabelIndex,
  type PUnLabel,
  type PLookupLabel,
  pindexDataRecord,
  pdropDataRecord,
  DualReprHandler (..),
  PlutusTypeData,
) where

import Data.Coerce (coerce)
import Data.Functor.Compose qualified as F
import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.List (groupBy, maximumBy, sortOn)
import Data.Proxy (Proxy (Proxy))
import Data.SOP.NP (cana_NP)
import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.TypeLits (
  KnownNat,
  KnownSymbol,
  Nat,
  Symbol,
  natVal,
  symbolVal,
  type (+),
 )
import Generics.SOP (
  All,
  Compose,
  K (K),
  NP (Nil, (:*)),
  NS (S, Z),
  SListI,
  SOP (SOP),
  Top,
  case_SList,
  hcollapse,
  hindex,
  hmap,
  para_SList,
 )
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PData,
  pasConstr,
  pchooseListBuiltin,
  pconstrBuiltin,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque, popaque)
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit (PUnit))
import Plutarch.DataRepr.Internal.HList (
  HRec (HCons, HNil),
  HRecGeneric (HRecGeneric),
  Labeled (Labeled),
  type Drop,
  type IndexList,
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
import Plutarch.Internal.IsData (PIsData, pdata, pdataImpl, pforgetData, pfromData, pfromDataImpl)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.ListLike (PListLike (pnil), pcons, pdrop, phead, ptail, ptryIndex)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd (pmax, pmin, (#<), (#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  DerivedPInner,
  PlutusType (PInner, pcon', pmatch'),
  PlutusTypeStrat,
  PlutusTypeStratConstraint,
  derivedPCon,
  derivedPMatch,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Show (PShow (pshow'))
import Plutarch.Internal.Term (
  Dig,
  Term,
  pdelay,
  perror,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.Term qualified as P
import Plutarch.Internal.TermCont (
  TermCont,
  hashOpenTerm,
  runTermCont,
  tcont,
  unTermCont,
 )
import Plutarch.Internal.TryFrom (
  PSubtype',
  PSubtypeRelation (PNoSubtypeRelation, PSubtypeRelation),
  PTryFrom,
  PTryFromExcess,
  ptryFrom,
  ptryFrom',
  pupcast,
 )
import Plutarch.Reducible (NoReduce, Reduce)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.Unsafe (punsafeCoerce)

{- | A "record" of `exists a. PAsData a`. The underlying representation is
 `PBuiltinList PData`.
-}
data PDataRecord (as :: [PLabeledType]) (s :: P.S) where
  PDCons ::
    forall name_x x xs s.
    PUnLabel name_x ~ x =>
    Term s (PAsData x) ->
    (Term s (PDataRecord xs)) ->
    -- GHC bug prevents `name ':= x` from working well
    PDataRecord (name_x ': xs) s
  PDNil :: PDataRecord '[] s

newtype H s (l :: [PLabeledType]) = H {unH :: forall r. (PDataRecord l s -> Term s r) -> Term s r}

instance SListI l => PlutusType (PDataRecord l) where
  type PInner (PDataRecord l) = PBuiltinList PData
  pcon' :: PDataRecord l s -> Term s (PBuiltinList PData)
  pcon' (PDCons x xs) = pcons # pforgetData x # pto xs
  pcon' PDNil = pcon' PDNil
  pmatch' :: Term s (PBuiltinList PData) -> (PDataRecord l s -> Term s b) -> Term s b
  pmatch' l' = unH
    $ case_SList
      (H $ \f -> f PDNil)
    $ H
    $ \f ->
      plet l' \l ->
        let x :: Term _ (PAsData x)
            x = punsafeCoerce $ phead # l
            xs :: Term _ (PDataRecord xs)
            xs = punsafeCoerce $ ptail # l
         in f $ PDCons x xs

-- | This uses data equality. 'PEq' instances of elements don't make any difference.
instance PEq (PDataRecord xs) where
  x #== y = pto x #== pto y

-- Lexicographic ordering based 'Ord' instances for 'PDataRecord'.

instance POrd (PDataRecord '[]) where
  {-# INLINEABLE (#<=) #-}
  _ #<= _ = pconstant True
  {-# INLINEABLE (#<) #-}
  _ #< _ = pconstant False
  {-# INLINEABLE pmin #-}
  pmin t _ = t
  {-# INLINEABLE pmax #-}
  pmax = pmin

instance PShow (PDataRecord '[]) where
  pshow' _ _ = "[]"

instance
  (All Top xs, KnownSymbol label, PIsData x, PShow x, PShow (PDataRecordShowHelper xs)) =>
  PShow (PDataRecord ((label ':= x) ': xs))
  where
  pshow' b xs = "[" <> pmatch xs go
    where
      go :: PDataRecord ((label ':= x) ': xs) s -> Term s PString
      go (PDCons y ys) =
        showWithLabel (Proxy @label) b y
          <> pshow' b (pcon $ PDataRecordShowHelper ys)

{- | This type exists because we need different show strategies depending
 on if the original list was non-empty. The idea is to implement the
 following at the type-level:

 @
 showList' :: Show a => [a] -> String
 showList' [] = "[]"
 showList' (x:xs) = '[' : show x ++ go xs
 where
   go [] = "]"
   go (y:ys) = ',' : show y ++ go ys
 @
-}
newtype PDataRecordShowHelper as s = PDataRecordShowHelper (Term s (PDataRecord as))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PDataRecordShowHelper as) where type DPTStrat _ = PlutusTypeNewtype

instance PShow (PDataRecordShowHelper '[]) where
  pshow' _ _ = "]"

instance
  (All Top xs, KnownSymbol label, PIsData x, PShow x, PShow (PDataRecordShowHelper xs)) =>
  PShow (PDataRecordShowHelper ((label ':= x) ': xs))
  where
  pshow' b xs = ", " <> pmatch (pto xs) go
    where
      go (PDCons y ys) =
        showWithLabel (Proxy @label) b y
          <> pshow' b (pcon $ PDataRecordShowHelper ys)

instance (POrd x, PIsData x) => POrd (PDataRecord '[label ':= x]) where
  {-# INLINEABLE (#<) #-}
  l1 #< l2 = unTermCont $ do
    PDCons x _ <- tcont $ pmatch l1
    PDCons y _ <- tcont $ pmatch l2
    pure $ pfromData x #< pfromData y
  {-# INLINEABLE (#<=) #-}
  l1 #<= l2 = unTermCont $ do
    PDCons x _ <- tcont $ pmatch l1
    PDCons y _ <- tcont $ pmatch l2
    pure $ pfromData x #<= pfromData y

instance
  (SListI xs, POrd x, PIsData x, POrd (PDataRecord (x' ': xs))) =>
  POrd (PDataRecord ((label ':= x) ': x' ': xs))
  where
  {-# INLINEABLE (#<) #-}
  l1 #< l2 = unTermCont $ do
    PDCons x xs <- tcont $ pmatch l1
    PDCons y ys <- tcont $ pmatch l2
    a <- tcont . plet $ pfromData x
    b <- tcont . plet $ pfromData y
    pure $ pif (a #< b) (pconstant True) $ pif (a #== b) (xs #< ys) $ pconstant False
  {-# INLINEABLE (#<=) #-}
  l1 #<= l2 = unTermCont $ do
    PDCons x xs <- tcont $ pmatch l1
    PDCons y ys <- tcont $ pmatch l2
    a <- tcont . plet $ pfromData x
    b <- tcont . plet $ pfromData y
    pure $ pif (a #< b) (pconstant True) $ pif (a #== b) (xs #<= ys) $ pconstant False

{- | Cons a field to a data record.

You can specify the label to associate with the field using type applications-

@

foo :: Term s (PDataRecord '[ "fooField" ':= PByteString ])
foo = pdcons @"fooField" # pdata (phexByteStr "ab") # pdnil

@
-}
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))
pdcons = punsafeCoerce $ pcons @PBuiltinList @PData

-- | An empty 'PDataRecord'.
pdnil :: Term s (PDataRecord '[])
pdnil = punsafeCoerce $ pnil @PBuiltinList @PData

data PLabeledType = Symbol := (P.S -> Type)

type family PLabelIndex (name :: Symbol) (as :: [PLabeledType]) :: Nat where
  PLabelIndex name ((name ':= _) ': _) = 0
  PLabelIndex name (_ ': as) = PLabelIndex name as + 1

type PLookupLabel :: Symbol -> [PLabeledType] -> P.S -> Type
type family PLookupLabel name as where
  PLookupLabel name ((name ':= a) ': _) = a
  PLookupLabel name (_ ': as) = PLookupLabel name as

type family PUnLabel (a :: PLabeledType) :: P.S -> Type where
  PUnLabel (_ ':= a) = a

instance PIsData (PDataRecord xs) where
  pfromDataImpl x = punsafeCoerce (pfromData (punsafeCoerce x) :: Term _ (PBuiltinList PData))
  pdataImpl x = pupcast $ pdata (pupcast x :: Term _ (PBuiltinList PData))

{- | A sum of 'PDataRecord's. The underlying representation is the `Constr` constructor,
where the integer is the index of the variant and the list is the record.
-}
type PDataSum :: [[PLabeledType]] -> P.S -> Type
newtype PDataSum defs s = PDataSum (NS (F.Compose (Term s) PDataRecord) defs)

instance (All Top defs, All (Compose PShow PDataRecord) defs) => PShow (PDataSum defs) where
  pshow' b dsum = pmatch dsum showSum
    where
      showSum :: All (Compose PShow PDataRecord) xs => PDataSum xs s -> Term s PString
      showSum (PDataSum (Z x)) = pshow' b (F.getCompose x)
      showSum (PDataSum (S x)) = showSum (PDataSum x)

class IsPDataSum (a :: [[P.S -> Type]]) where
  type IsPDataSumDefs a :: [[PLabeledType]]
  toSum :: SOP (Term s) a -> PDataSum (IsPDataSumDefs a) s
  fromSum :: PDataSum (IsPDataSumDefs a) s -> SOP (Term s) a

instance IsPDataSum '[] where
  type IsPDataSumDefs '[] = '[]
  toSum (SOP x) = case x of {}
  fromSum (PDataSum x) = case x of {}

instance IsPDataSum xs => IsPDataSum ('[PDataRecord l] ': xs) where
  type IsPDataSumDefs ('[PDataRecord l] ': xs) = (l ': IsPDataSumDefs xs)
  toSum (SOP (Z (x :* Nil))) = PDataSum $ Z $ coerce x
  toSum (SOP (S x)) = case toSum (SOP x) of
    PDataSum y -> PDataSum $ S y
  fromSum (PDataSum (Z x)) = SOP $ Z $ coerce x :* Nil
  fromSum (PDataSum (S x)) = case fromSum (PDataSum x) of
    SOP y -> SOP $ S y

data DataReprHandlers (out :: P.S -> Type) (defs :: [[PLabeledType]]) (s :: P.S) where
  DRHNil :: DataReprHandlers out '[] s
  DRHCons :: (Term s (PDataRecord def) -> Term s out) -> DataReprHandlers out defs s -> DataReprHandlers out (def ': defs) s

newtype A s out defs = A {unA :: (PDataSum defs s -> Term s out) -> DataReprHandlers out defs s}

instance
  SListI defs =>
  PlutusType (PDataSum defs)
  where
  type PInner (PDataSum defs) = PData
  pcon' (PDataSum xss) =
    let constrIx = fromIntegral $ hindex xss
        datRec = hcollapse $ hmap (K . pto . F.getCompose) xss
     in pforgetData $ pconstrBuiltin # pconstant constrIx # datRec
  pmatch' d f =
    let handlers = conv f
     in case handlers of
          DRHCons handler DRHNil -> handler $ punDataSum # (punsafeCoerce d :: Term _ (PDataSum defs))
          _ -> plet (pasConstr #$ d) $ \d' ->
            plet (pfstBuiltin # d') $ \constr ->
              plet (psndBuiltin # d') $ \args ->
                let handlers' = applyHandlers args handlers
                 in runTermCont (findCommon handlers') $ \common ->
                      reprHandlersGo
                        common
                        0
                        handlers'
                        constr
    where
      applyHandlers :: forall out s defs. Term s (PBuiltinList PData) -> DataReprHandlers out defs s -> [Term s out]
      applyHandlers _ DRHNil = []
      applyHandlers args (DRHCons handler rest) = handler (punsafeCoerce args) : applyHandlers args rest

      conv :: forall out s defs. SListI defs => (PDataSum defs s -> Term s out) -> DataReprHandlers out defs s
      conv =
        unA $
          para_SList
            (A $ const DRHNil)
            ( \(A prev) -> A \f ->
                DRHCons
                  (\x -> f (PDataSum (Z $ coerce x)))
                  $ prev (\(PDataSum x) -> f (PDataSum (S x)))
            )

instance PIsData (PDataSum defs) where
  pfromDataImpl = punsafeCoerce
  pdataImpl = punsafeCoerce

instance PEq (PDataSum defs) where
  x #== y = pdata x #== pdata y

instance All (Compose POrd PDataRecord) defs => POrd (PDataSum defs) where
  {-# INLINEABLE (#<) #-}
  x' #< y' = f # x' # y'
    where
      f :: Term s (PDataSum defs :--> PDataSum defs :--> PBool)
      f = phoistAcyclic $ plam $ \x y -> pmatchLT x y mkLTHandler
  {-# INLINEABLE (#<=) #-}
  x' #<= y' = f # x' # y'
    where
      f :: Term s (PDataSum defs :--> PDataSum defs :--> PBool)
      f = phoistAcyclic $ plam $ \x y -> pmatchLT x y mkLTEHandler

-- | If there is only a single variant, then we can safely extract it.
punDataSum :: Term s (PDataSum '[def] :--> PDataRecord def)
punDataSum = phoistAcyclic $
  plam $ \t ->
    (punsafeCoerce $ psndBuiltin # (pasConstr #$ pforgetData $ pdata t) :: Term _ (PDataRecord def))

-- | Try getting the nth variant. Errs if it's another variant.
ptryIndexDataSum :: KnownNat n => Proxy n -> Term s (PDataSum (def ': defs) :--> PDataRecord (IndexList n (def ': defs)))
ptryIndexDataSum n = phoistAcyclic $
  plam $ \t ->
    plet (pasConstr #$ pforgetData $ pdata t) $ \d ->
      let i :: Term _ PInteger = pfstBuiltin # d
       in pif
            (i #== fromInteger (natVal n))
            (punsafeCoerce $ psndBuiltin # d :: Term _ (PDataRecord _))
            perror

-- | Safely index a 'PDataRecord'.
pindexDataRecord :: KnownNat n => Proxy n -> Term s (PDataRecord as) -> Term s (PAsData (PUnLabel (IndexList n as)))
pindexDataRecord n xs =
  punsafeCoerce $
    ptryIndex @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

-- | Safely drop the first n items of a 'PDataRecord'.
pdropDataRecord :: KnownNat n => Proxy n -> Term s (PDataRecord xs) -> Term s (PDataRecord (Drop n xs))
pdropDataRecord n xs =
  punsafeCoerce $
    pdrop @PBuiltinList @PData (fromInteger $ natVal n) (punsafeCoerce xs)

data PlutusTypeData

class
  ( IsPDataSum (PCode a)
  , SListI (IsPDataSumDefs (PCode a))
  , PGeneric a
  ) =>
  PlutusTypeDataConstraint a
instance
  ( IsPDataSum (PCode a)
  , SListI (IsPDataSumDefs (PCode a))
  , PGeneric a
  ) =>
  PlutusTypeDataConstraint a

instance PlutusTypeStrat PlutusTypeData where
  type PlutusTypeStratConstraint PlutusTypeData = PlutusTypeDataConstraint
  type DerivedPInner PlutusTypeData a = PDataSum (IsPDataSumDefs (PCode a))
  derivedPCon x = pcon $ toSum $ gpfrom x
  derivedPMatch x f = pmatch x (f . gpto . fromSum)

newtype DualReprHandler s out def = DualRepr (Term s (PDataRecord def) -> Term s (PDataRecord def) -> Term s out)

-- | Optimized dual pmatch specialized for lexicographic '#<' and '#<=' implementations.
pmatchLT :: Term s (PDataSum defs) -> Term s (PDataSum defs) -> NP (DualReprHandler s PBool) defs -> Term s PBool
pmatchLT d1 d2 (DualRepr handler :* Nil) = handler (punDataSum # d1) (punDataSum # d2)
pmatchLT d1 d2 handlers = unTermCont $ do
  a <- tcont . plet $ pasConstr #$ pforgetData $ pdata d1
  b <- tcont . plet $ pasConstr #$ pforgetData $ pdata d2

  cid1 <- tcont . plet $ pfstBuiltin # a
  cid2 <- tcont . plet $ pfstBuiltin # b

  pure
    $ pif
      (cid1 #< cid2)
      -- Left arg's constructor id is less, no need to continue.
      (pconstant True)
    $ pif
      (cid1 #== cid2)
      -- Matching constructors, compare fields now.
      ( unTermCont $ do
          flds1 <- tcont . plet $ psndBuiltin # a
          flds2 <- tcont . plet $ psndBuiltin # b
          let handlers' = applyHandlers flds1 flds2 handlers
          common <- findCommon handlers'
          pure $ reprHandlersGo common 0 (applyHandlers flds1 flds2 handlers) cid1
      )
    -- Left arg's constructor id is greater, no need to continue.
    $ pconstant False
  where
    applyHandlers ::
      Term s (PBuiltinList PData) ->
      Term s (PBuiltinList PData) ->
      NP (DualReprHandler s PBool) defs ->
      [Term s PBool]
    applyHandlers _ _ Nil = []
    applyHandlers args1 args2 (DualRepr handler :* rest) =
      handler (punsafeCoerce args1) (punsafeCoerce args2)
        : applyHandlers args1 args2 rest

reprHandlersGo ::
  (Dig, Term s out) ->
  Integer ->
  [Term s out] ->
  Term s PInteger ->
  Term s out
reprHandlersGo common _ [] _ = snd common
reprHandlersGo common idx (handler : rest) c =
  runTermCont (hashOpenTerm handler) $ \hhash ->
    if hhash == fst common
      then reprHandlersGo common (idx + 1) rest c
      else
        pif
          (fromInteger idx #== c)
          handler
          $ reprHandlersGo common (idx + 1) rest c

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

mkLTHandler :: forall def s. All (Compose POrd PDataRecord) def => NP (DualReprHandler s PBool) def
mkLTHandler = cana_NP (Proxy @(Compose POrd PDataRecord)) rer $ Const ()
  where
    rer ::
      forall (y :: [PLabeledType]) (ys :: [[PLabeledType]]).
      Compose POrd PDataRecord y =>
      Const () (y ': ys) ->
      (DualReprHandler s PBool y, Const () ys)
    rer _ = (DualRepr (#<), Const ())

mkLTEHandler :: forall def s. All (Compose POrd PDataRecord) def => NP (DualReprHandler s PBool) def
mkLTEHandler = cana_NP (Proxy @(Compose POrd PDataRecord)) rer $ Const ()
  where
    rer ::
      forall (y :: [PLabeledType]) (ys :: [[PLabeledType]]).
      Compose POrd PDataRecord y =>
      Const () (y ': ys) ->
      (DualReprHandler s PBool y, Const () ys)
    rer _ = (DualRepr (#<=), Const ())

----------------------- HRecP and friends -----------------------------------------------

type HRecPApply :: [(Symbol, P.S -> Type)] -> P.S -> [(Symbol, Type)]
type family HRecPApply as s where
  HRecPApply ('(name, ty) ': rest) s = '(name, Reduce (ty s)) ': HRecPApply rest s
  HRecPApply '[] _ = '[]

newtype HRecP (as :: [(Symbol, P.S -> Type)]) (s :: P.S)
  = HRecP (NoReduce (HRecGeneric (HRecPApply as s)))
  deriving stock (Generic)

newtype Flip f a b = Flip (f b a)
  deriving stock (Generic)

class Helper2 (b :: PSubtypeRelation) a where
  type Helper2Excess b a :: P.S -> Type
  ptryFromData' :: forall s r. Proxy b -> Term s PData -> ((Term s (PAsData a), Reduce (Helper2Excess b a s)) -> Term s r) -> Term s r

instance PTryFrom PData (PAsData a) => Helper2 'PNoSubtypeRelation a where
  type Helper2Excess 'PNoSubtypeRelation a = PTryFromExcess PData (PAsData a)
  ptryFromData' _ = ptryFrom'

instance PTryFrom PData a => Helper2 'PSubtypeRelation a where
  type Helper2Excess 'PSubtypeRelation a = PTryFromExcess PData a
  ptryFromData' _ x = runTermCont $ do
    (y, exc) <- tcont $ ptryFrom @a @PData x
    pure (punsafeCoerce y, exc)

-- We could have a more advanced instance but it's not needed really.
newtype ExcessForField (b :: PSubtypeRelation) (a :: P.S -> Type) (s :: P.S)
  = ExcessForField (Term s (PAsData a), Reduce (Helper2Excess b a s))
  deriving stock (Generic)

instance PTryFrom (PBuiltinList PData) (PDataRecord '[]) where
  type PTryFromExcess (PBuiltinList PData) (PDataRecord '[]) = HRecP '[]
  ptryFrom' opq = runTermCont $ do
    _ <-
      tcont . plet . pforce $
        pchooseListBuiltin # opq # pdelay (pcon PUnit) # pdelay (ptraceInfoError "ptryFrom(PDataRecord[]): list is longer than zero")
    pure (pdnil, HRecGeneric HNil)

type family UnHRecP (x :: P.S -> Type) :: [(Symbol, P.S -> Type)] where
  UnHRecP (HRecP as) = as

instance
  ( Helper2 (PSubtype' PData pty) pty
  , PTryFrom (PBuiltinList PData) (PDataRecord as)
  , PTryFromExcess (PBuiltinList PData) (PDataRecord as) ~ HRecP ase
  ) =>
  PTryFrom (PBuiltinList PData) (PDataRecord ((name ':= pty) ': as))
  where
  type
    PTryFromExcess (PBuiltinList PData) (PDataRecord ((name ':= pty) ': as)) =
      HRecP
        ( '(name, ExcessForField (PSubtype' PData pty) pty)
            ': UnHRecP (PTryFromExcess (PBuiltinList PData) (PDataRecord as))
        )
  ptryFrom' opq = runTermCont $ do
    h <- tcont $ plet $ phead # opq
    hv <- tcont $ ptryFromData' (Proxy @(PSubtype' PData pty)) h
    t <- tcont $ plet $ ptail # opq
    tv <- tcont $ ptryFrom @(PDataRecord as) @(PBuiltinList PData) t
    pure (punsafeCoerce opq, HRecGeneric (HCons (Labeled hv) (coerce $ snd tv)))

newtype Helper a b s = Helper (Reduce (a s), Reduce (b s)) deriving stock (Generic)

instance
  ( PTryFrom (PBuiltinList PData) (PDataRecord as)
  , PTryFromExcess (PBuiltinList PData) (PDataRecord as) ~ HRecP ase
  ) =>
  PTryFrom PData (PAsData (PDataRecord as))
  where
  type
    PTryFromExcess PData (PAsData (PDataRecord as)) =
      Helper (Flip Term (PDataRecord as)) (PTryFromExcess (PBuiltinList PData) (PDataRecord as))
  ptryFrom' opq = runTermCont $ do
    l <- snd <$> tcont (ptryFrom @(PAsData (PBuiltinList PData)) opq)
    r <- tcont $ ptryFrom @(PDataRecord as) l
    pure (punsafeCoerce opq, r)

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Proxy n -> Proxy sum -> Term s PInteger -> Term s (PBuiltinList PData) -> Term s POpaque

instance
  forall (n :: Nat) (x :: [PLabeledType]) (xs :: [[PLabeledType]]).
  ( PTryFrom (PBuiltinList PData) (PDataRecord x)
  , SumValidation (n + 1) xs
  , KnownNat n
  ) =>
  SumValidation n (x ': xs)
  where
  validateSum _ _ constr fields =
    pif
      (fromInteger (natVal $ Proxy @n) #== constr)
      ( unTermCont $ do
          _ <- tcont $ ptryFrom @(PDataRecord x) fields
          pure $ popaque $ pcon PUnit
      )
      (validateSum (Proxy @(n + 1)) (Proxy @xs) constr fields)

instance SumValidation n '[] where
  validateSum _ _ _ _ = ptraceInfoError "reached end of sum while still not having found the constructor"

instance SumValidation 0 ys => PTryFrom PData (PDataSum ys) where
  type PTryFromExcess _ _ = Const ()
  ptryFrom' opq = runTermCont $ do
    x <- tcont $ plet $ pasConstr # opq
    constr <- tcont $ plet $ pfstBuiltin # x
    fields <- tcont $ plet $ psndBuiltin # x
    _ <- tcont $ plet $ validateSum (Proxy @0) (Proxy @ys) constr fields
    pure (punsafeCoerce opq, ())

instance PTryFrom PData (PDataSum ys) => PTryFrom PData (PAsData (PDataSum ys)) where
  type PTryFromExcess _ _ = Const ()
  ptryFrom' x = runTermCont $ do
    (y, exc) <- tcont $ ptryFrom x
    pure (pdata y, exc)

-- | Annotates a shown field with a label.
showWithLabel ::
  forall label t s.
  (KnownSymbol label, PShow t) =>
  Proxy label ->
  Bool ->
  Term s t ->
  Term s PString
showWithLabel proxy b x = lblStr <> " = " <> pshow' b x
  where
    lblStr = fromString $ symbolVal proxy
