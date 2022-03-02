{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Plutarch.DataRepr.Internal (
  PDataSum,
  punDataSum,
  ptryIndexDataSum,
  pmatchDataSum,
  pdcons,
  pdnil,
  DataReprHandlers (..),
  PDataRecord (..),
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
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:<>:)),
  KnownNat,
  Nat,
  Symbol,
  TypeError,
  natVal,
  type (+), 
  Natural
 )
import Generics.SOP (
  All,
  All2,
  AllZipN,
  Code,
  Generic,
  I (I),
  K (K),
  LiftedCoercible,
  NP (Nil, (:*)),
  POP,
  SListI,
  SOP (SOP),
  from,
  hcmap,
  hcollapse,
  hfromI,
  hindex,
  to,
 )
import Plutarch (
  Dig,
  PInner,
  PMatch,
  PType,
  PlutusType,
  S,
  Term,
  pcon',
  perror,
  phoistAcyclic,
  plam,
  plet,
  pmatch',
  pto,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
  PIsData,
  pasConstr,
  pconstrBuiltin,
  pdata,
  pforgetData,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.DataRepr.Internal.Generic (MkSum (mkSum))
import Plutarch.DataRepr.Internal.HList (type Drop, type IndexList)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (S (SI), punsafeBuiltin)
import Plutarch.Internal.TypeFamily (ToPType2)
import Plutarch.Lift (PConstant, PConstantRepr, PConstanted, PLift, pconstant, pconstantFromRepr, pconstantToRepr)
import Plutarch.List (PListLike (pnil), pcons, pdrop, phead, ptail, ptryIndex)
import Plutarch.TermCont (TermCont, hashOpenTerm, runTermCont)
import Plutarch.Unsafe (punsafeCoerce)
import qualified Plutus.V1.Ledger.Api as Ledger
import qualified PlutusCore as PLC
import Data.Constraint
import Data.Constraint.Nat (plusNat)
import Data.Type.Equality
import qualified Unsafe.Coerce as UNSAFE

{- | A "record" of `exists a. PAsData a`. The underlying representation is
 `PBuiltinList PData`.
-}
data PDataRecord (as :: [PLabeledType]) (s :: S) where
  PDCons ::
    forall name x xs s.
    Term s (PAsData x) ->
    (Term s (PDataRecord xs)) ->
    PDataRecord ((name ':= x) ': xs) s
  PDNil :: PDataRecord '[] s

instance PlutusType (PDataRecord ((name ':= x) ': xs)) where
  type PInner (PDataRecord ((name ':= x) ': xs)) _ = PBuiltinList PData
  pcon' (PDCons x xs) = pto result
    where
      result :: Term _ (PDataRecord ((name ':= x) ': xs))
      result = pdcons # x # xs
  pmatch' l' f = plet l' $ \l ->
    let x :: Term _ (PAsData x)
        x = punsafeCoerce $ phead # l
        xs :: Term _ (PDataRecord xs)
        xs = punsafeCoerce $ ptail # l
     in f $ PDCons x xs

instance PlutusType (PDataRecord '[]) where
  type PInner (PDataRecord '[]) _ = PBuiltinList PData
  pcon' PDNil = pnil
  pmatch' _ f = f PDNil

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
data PDataSum (defs :: [[PLabeledType]]) (s :: S) where 
  MkPDataSum :: PDataSum' n 'True defs s -> PDataSum defs s 

{- Overview of how this works: 

1) We need constructors for PDataSum in order to define a PlutusType instance. 

2) Ideally, we want the PDataSum data type to be structured so as to enforce the invariant that each sum contains 
   *exactly one* of its choices

3) We can't do that with PDataSum alone. To enforce those invariants at the type level, we need an index type which encodes 
   the fact that the sum contains exactly one choice. 

4) PDataSum' is the helper type which allows us to express those invariants. The structure of the type has to be inductive, but 
   we need four constructors (two Zero / two Suc) to cover each of the possible cases. The Boolean type argument indicates whether or not the 
   sum contains an element, while the Natural type argument represents the position of the element in the list of choices. 

      a) 'SumZE' represents a one element sum that *does not* contain its element.  

      b) 'SumZF' represents a one element sum that *does* contain its element.

      c) 'InjE' injects an element into a sum which does not contain any elements such that the sum will contain the injected element. 

      d) 'InjF' injects an element into a sum which may or may not contain any elements, and preserves the containment status of 
         the sum it was injected into. 

      Suggestions for better constructor names are welcome. 

5) PDataSum is a wrapper around a PDataSum' that contains exactly one of its elements. The structure of PDataSum' ensures that any 'PDataSum n 'True' 
   will contain exactly one of its elements. An empty sum cannot be represented, and there is no way to inject a second element into a sum which already 
   contains one element such that the resulting sum contains more than one of its elements. 

6) We use a universally quantified continuation to indicate containment. I *think* we have to do it this way. 

7) We need some helper utilities to reconstruct the Sum (which we have to do) in pmatch'. Those utilities are: 
    
    a) 'IndexedSum'. This is basically a singleton list a la SList from sop-core, except it can only represent nonempty lists, has an 
       "explicit" inductive structure, and indicates the position of elements explicitly. 

    b) A 'Length' type family. This doesn't work like 'length' from Prelude: 'Length '[a]' == 0, not 1. 

    c) A type class 'IndexableSum' which is like SListI from sop-core, except for our 'IndexedSum' type. As far as I can tell it is impossible 
       to reconstruct the sum without an inductive GADT that represents the structure of the sum. We need to conjure that structure out of thin air,
       so we need a type class. 

    d) Some dictionary stuff and an axiom to placate GHC and make this all typecheck.
-}

data PDataSum' :: Natural -> Bool -> [[PLabeledType]] -> PType where 
  SumZ_ :: forall {x:: [PLabeledType]} {s :: S}
           . PDataSum' 0 'False '[x] s

  SumZ :: forall {x:: [PLabeledType]} {s :: S}
           . (forall {out :: PType}. (Term s (PDataRecord x) -> Term s out) -> Term s out) 
           -> PDataSum' 0 'True '[x] s

  Inj :: forall {n :: Natural} {x :: [PLabeledType]} {xss :: [[PLabeledType]]} {s :: S} 
        . (KnownNat (n+1))
        => (forall {out :: PType}. (Term s (PDataRecord x) -> Term s out) -> Term s out) 
        -> PDataSum' n 'False xss s 
        -> PDataSum' (n + 1) 'True (x ': xss) s 

  Inj_ :: forall {n :: Natural} {b :: Bool} {x :: [PLabeledType]} {xss :: [[PLabeledType]]} {s :: S} 
        .  KnownNat (n+1)
        => PDataSum' n b xss s 
        -> PDataSum' (n + 1) b (x ': xss) s

-- Used to reconstruct the PDataSum in the pmatch' instance for PlutusType 
data IndexedSum :: Nat -> [k] -> Type where 
  IxZ :: forall k (a :: k). IndexedSum 0 '[a]

  IxS :: forall k (a :: k) (as :: [k]) 
       . IndexedSum (Length as) as -> IndexedSum (Length as + 1) (a ': as)

-- Well, length minus one at least 
type family Length (xss :: [k]) :: Nat where 
  Length '[x] = 0 
  Length (x ': xs) = (Length xs) + 1 

-- Could convert an SList into this, but this is simpler (I don't think SList is suitable here)
class IndexableSum (defs :: [k]) where 
  indexSum :: IndexedSum (Length defs) defs 

-- this is kind of a cheap hack and there's probably a better way 
knownLength :: forall n ks. IndexedSum n ks -> Dict (KnownNat n)
knownLength = \case 
  IxZ -> Dict 
  ixed@(IxS rest) -> case knownLength rest of 
    d@Dict -> go ixed d 
 where 
   go :: forall (n' :: Nat) a as . IndexedSum (n' + 1) (a:as) 
      -> Dict (KnownNat n') 
      -> Dict (KnownNat (n' + 1))
   go (IxS _) Dict = mapDict plusNat (Dict :: Dict (KnownNat n', KnownNat 1))

-- maybe don't need the constraints? 
addLength :: forall k ks. (IndexableSum (k:ks), IndexableSum ks) => Length (k:ks) :~: Length ks + 1 
addLength = UNSAFE.unsafeCoerce Refl 

-- maybe overlap-related pragmas?
instance IndexableSum '[k] where 
  indexSum = IxZ 
  
instance IndexableSum ks => IndexableSum (k ': ks) where 
  indexSum = case addLength @k @ks of 
              Refl -> IxS (indexSum :: IndexedSum (Length ks) ks)

-- the constraint will be satisfied by every nonempty list
instance IndexableSum defs => PlutusType (PDataSum defs) where
  type PInner (PDataSum defs) _ = PData 

  -- this is reasonably straightforward. we don't need to worry about the SumZE case because it's impossible here 
  pcon' :: forall (s :: S) (b :: PType). PDataSum defs s -> Term s (PInner (PDataSum defs) b)
  pcon' (MkPDataSum s) = go s 
    where 
      go :: forall {n :: Natural} {defs :: [[PLabeledType]]} {s :: S}. PDataSum' n 'True defs s -> Term s (PInner (PDataSum defs) b)
      go = \case 
        SumZ f ->  punsafeBuiltin PLC.ConstrData # (pconstant @PInteger (natVal $ Proxy @n)) # (f id) -- rewrite this with pconstrBuiltin?
        Inj f _ -> punsafeBuiltin PLC.ConstrData # (pconstant @PInteger (natVal $ Proxy @n)) # (f id) 
        Inj_ pds -> go pds 

  -- this is more complicated 
  pmatch' :: forall (s :: S) (b :: PType)
           . Term s (PInner (PDataSum defs) b)
          -> (PDataSum defs s -> Term s b) 
          -> Term s b
  pmatch' d f = 
    plet (pasConstr #$ pforgetData $ pdata d) $ \d' ->
    plet (pfstBuiltin # d') $ \constr ->
    plet (psndBuiltin # d') $ \args ->
      go constr args (indexSum :: IndexedSum (Length defs) defs) (f . MkPDataSum)                     
   where 
    -- get the PInteger length of an IndexedSum. 
    iLen :: forall n as s. IndexedSum n as -> Term s PInteger 
    iLen ixed = pconstant $ natVal (Proxy @n) \\ knownLength ixed -- pconstant bad?

    -- Used to "fill in" the remaining elements of the "right hand side" of a sum once we've determined which element the sum ought to contain.
    mkEmpty :: forall n as s. IndexedSum n as -> PDataSum' n 'False as s 
    mkEmpty = \case 
      IxZ -> SumZ_
      i@(IxS rest) -> Inj_ (mkEmpty rest) \\ knownLength i 

    go :: forall n as
        . Term s PInteger -- the index of the constructor in the sum
       -> Term s (PBuiltinList PData)  -- the inner representation of the record at the index of the first argument 
       -> IndexedSum n as -- the GADT which represents the structure of the sum (can't construct it w/o this)
       -> (PDataSum' n 'True as s -> Term s b) -- an accumulator function. represents an empty "left-hand side" of the sum 
       -> Term s b                             
    go constr args ias@(IxS rest) g = 
      pif (constr #== iLen ias) -- if the index of the constructor matches the size of the sum
          (g $ Inj (\k -> k $ punsafeCoerce args) $ mkEmpty rest) -- then cons an injection (which contains an element) onto an empty tail, and append the empty left hand side at the front
          (go constr args rest (g . Inj_)) -- else recurse, "moving the accumulator forward by one"
        \\ knownLength ias -- ghc is bad at math

    go _ args IxZ g = g $ SumZ $ \k -> k $ punsafeCoerce args -- if we've reached the end of the IndexedSum we have to do this, no point in checking that the indices match 

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

-- TODO: This 'PMatch' constraint needs to be changed to 'PlutusType (breaking change).
class (PMatch a, PIsData a) => PIsDataRepr (a :: PType) where
  type PIsDataReprRepr a :: [[PLabeledType]]
  type PIsDataReprRepr a = PDataRecordFields2 (Code (a 'SI))

  pconRepr :: a s -> Term s (PDataSum (PIsDataReprRepr a))
  default pconRepr ::
    forall s code pcode.
    ( Generic (a s)
    , code ~ Code (a s)
    , pcode ~ ToPType2 code
    , All SListI pcode
    , All Singleton code
    , All2 IsBuiltinList pcode
    , AllZipN POP (LiftedCoercible I (Term s)) code pcode
    ) =>
    a s ->
    Term s (PDataSum (PIsDataReprRepr a))
  pconRepr x = punsafeCoerce expected
    where
      expected :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
      expected = gpconRepr @a $ from x

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

gpconRepr ::
  forall a s code pcode.
  ( Generic (a s)
  , code ~ Code (a s)
  , pcode ~ ToPType2 code
  , All SListI pcode
  , All Singleton code
  , All2 IsBuiltinList pcode
  , AllZipN POP (LiftedCoercible I (Term s)) code pcode
  ) =>
  SOP I (Code (a s)) ->
  Term s (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
gpconRepr x = pconstrBuiltin # pconstant (toInteger $ hindex sop) # head (hcollapse sop)
  where
    sop :: SOP (K (Term s (PBuiltinList PData))) pcode
    sop = hcmap (Proxy @IsBuiltinList) (K . dataListFrom) $ hfromI x

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

instance PIsDataRepr a => PlutusType (PIsDataReprInstances a) where
  type PInner (PIsDataReprInstances a) _ = PDataSum (PIsDataReprRepr a)
  pcon' (PIsDataReprInstances x) = pconRepr x
  pmatch' x f = pmatchRepr x (f . PIsDataReprInstances)

newtype DerivePConstantViaData (h :: Type) (p :: PType) = DerivePConstantViaData h

instance (PIsDataRepr p, PLift p, Ledger.FromData h, Ledger.ToData h) => PConstant (DerivePConstantViaData h p) where
  type PConstantRepr (DerivePConstantViaData h p) = Ledger.Data
  type PConstanted (DerivePConstantViaData h p) = p
  pconstantToRepr (DerivePConstantViaData x) = Ledger.toData x
  pconstantFromRepr x = DerivePConstantViaData <$> Ledger.fromData x

-- I wish type families could be applied partially....
class Singleton a
instance Singleton (x ': '[])

class IsBuiltinList a where
  dataListFrom :: Term s a -> Term s (PBuiltinList PData)

instance IsBuiltinList (PDataRecord l) where
  dataListFrom = punsafeCoerce

