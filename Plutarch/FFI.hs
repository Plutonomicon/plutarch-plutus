{-# LANGUAGE UndecidableInstances #-}

module Plutarch.FFI (
  type (>~<),
  PDelayedList (PDCons, PDNil),
  foreignExport,
  foreignImport,
  opaqueExport,
  opaqueImport,
  pdelayList,
  pforceList,
  unsafeForeignExport,
  unsafeForeignImport,
) where

import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TypeLits
import qualified Generics.SOP as SOP
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  ConstructorName,
  DatatypeInfo (ADT, Newtype),
 )
import GHC.Exts (Any)
import Plutarch (
  ClosedTerm,
  PDelayed,
  POpaque,
  PType,
  S,
  pcon,
  pdelay,
  pforce,
  phoistAcyclic,
  plam,
  pto,
  (#),
  (:-->),
 )
import Plutarch.Bool (PBool, PEq, (#==))
import Plutarch.Builtin (PAsData, PData)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  asClosedRawTerm,
  compile',
 )
import Plutarch.Internal.PlutusType (PlutusType (PInner, pcon', pmatch'))
import Plutarch.List (PList, PListLike (PElemConstraint, pcons, pelimList, pnil), pconvertLists, plistEquals)
import Plutarch.String (PString)
import Plutarch.Unit (PUnit)
import Plutus.V1.Ledger.Scripts (Script (unScript), fromCompiledCode)
import PlutusTx.Builtins.Internal (BuiltinBool, BuiltinByteString, BuiltinData, BuiltinUnit)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (DeserializedCode))
import PlutusTx.Prelude (BuiltinString)
import UntypedPlutusCore (fakeNameDeBruijn)
import qualified UntypedPlutusCore as UPLC

data ForallPhantom :: Type
data PhorallPhantom :: PType

data Delayed :: Type -> Type
data DelayedList :: Type -> Type

{- | Plutarch type of delayed lists, compatible with the PlutusTx encoding of
 Haskell lists and convertible with the regular 'PList' using 'pdelayList'
 and 'pforceList'.
-}
data PDelayedList (a :: PType) (s :: S)
  = PDCons (Term s a) (Term s (PDelayedList a))
  | PDNil
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

instance PEq a => PEq (PDelayedList a) where
  (#==) xs ys = plistEquals # xs # ys

-- | Compile and export a Plutarch term so it can be used by `PlutusTx.applyCode`.
foreignExport :: p >~< t => ClosedTerm p -> CompiledCode t
foreignExport = unsafeForeignExport

-- | Import compiled UPLC code (such as a spliced `PlutusTx.compile` result) as a Plutarch term.
foreignImport :: p >~< t => CompiledCode t -> ClosedTerm p
foreignImport = unsafeForeignImport

-- | Export Plutarch term of any type as @CompiledCode Any@.
opaqueExport :: ClosedTerm p -> CompiledCode Any
opaqueExport = unsafeForeignExport

-- | Import compiled UPLC code of any type as a Plutarch opaque term.
opaqueImport :: CompiledCode t -> ClosedTerm POpaque
opaqueImport = unsafeForeignImport

-- | Seriously unsafe, may fail at run time or result in unexpected behaviour in your on-chain validator.
unsafeForeignExport :: ClosedTerm p -> CompiledCode t
unsafeForeignExport t = DeserializedCode program Nothing mempty
  where
    program =
      UPLC.Program () (UPLC.Version () 1 0 0) $
        UPLC.termMapNames fakeNameDeBruijn $
          compile' $
            asClosedRawTerm t

-- | Seriously unsafe, may fail at run time or result in unexpected behaviour in your on-chain validator.
unsafeForeignImport :: CompiledCode t -> ClosedTerm p
unsafeForeignImport c = Term $ const $ TermResult (RCompiled $ UPLC._progTerm $ unScript $ fromCompiledCode c) []

-- | Convert a strict 'PList' to a 'PDelayList', perhaps before exporting it with 'foreignExport'.
pdelayList :: Term s (PList a :--> PDelayedList a)
pdelayList = pconvertLists

-- | Convert a 'PDelayList' to a strict 'PList', probably after importing it with 'foreignImport'.
pforceList :: Term s (PDelayedList a :--> PList a)
pforceList = pconvertLists

instance PlutusType (PDelayedList a) where
  type PInner (PDelayedList a) r = PDelayed (r :--> (a :--> PDelayedList a :--> r) :--> r)
  pcon' (PDCons x xs) = pdelay $ plam $ \_nil cons -> cons # x # xs
  pcon' PDNil = phoistAcyclic $ pdelay $ plam $ \nil _cons -> nil
  pmatch' elim f = pforce elim # f PDNil # (plam $ \x xs -> f $ PDCons x xs)

instance PListLike PDelayedList where
  type PElemConstraint PDelayedList _ = ()
  pelimList cons nil list = pforce (pto list) # nil # plam cons
  pcons = phoistAcyclic $ plam $ \x xs -> pcon (PDCons x xs)
  pnil = pcon PDNil

-- | Equality of inner types - Plutarch on the left and Haskell on the right.
type p >~< t = PlutarchInner p PhorallPhantom ~~ PlutusTxInner t ForallPhantom

type family a ~~ b :: Constraint where
  ForallPhantom ~~ _ = ()
  _ ~~ ForallPhantom = ()
  Delayed a ~~ Delayed b = a ~~ b
  a ~~ b = a ~ b

type family PlutarchInner (p :: PType) (any :: PType) :: Type where
  PlutarchInner PBool _ = BuiltinBool
  PlutarchInner PInteger _ = Integer
  PlutarchInner PString _ = Text
  PlutarchInner PByteString _ = ByteString
  PlutarchInner PData _ = BuiltinData
  PlutarchInner PUnit _ = ()
  PlutarchInner PhorallPhantom _ = ForallPhantom
  PlutarchInner (PAsData a :--> PAsData b) x = PlutarchInner (PData :--> PData) x
  PlutarchInner (PAsData a :--> b) x = PlutarchInner (PData :--> b) x
  PlutarchInner (a :--> b) x = PlutarchInner a x -> PlutarchInner b x
  PlutarchInner (PDelayed a) x = Delayed (PlutarchInner a x)
  PlutarchInner (PDelayedList a) x = DelayedList (PlutarchInner a x)
  PlutarchInner p x = PlutarchInner (PInner p x) x

type family PlutusTxInner (t :: Type) (any :: Type) :: Type where
  PlutusTxInner BuiltinBool _ = BuiltinBool
  PlutusTxInner Integer _ = Integer
  PlutusTxInner BuiltinString _ = Text
  PlutusTxInner BuiltinByteString _ = ByteString
  PlutusTxInner BuiltinData _ = BuiltinData
  PlutusTxInner BuiltinUnit _ = ()
  PlutusTxInner ForallPhantom _ = ForallPhantom
  PlutusTxInner (a -> b) x = PlutusTxInner a x -> PlutusTxInner b x
  PlutusTxInner (Delayed a) x = Delayed (PlutusTxInner a x)
  PlutusTxInner [a] x = DelayedList (PlutusTxInner a x)
  PlutusTxInner a x = TypeEncoding (GCode a) (GDatatypeInfoOf a) x

type TypeEncoding :: [[Type]] -> DatatypeInfo -> Type -> Type
type family TypeEncoding a rep x where
  TypeEncoding '[ '[b]] ( 'Newtype _ _ _) x = PlutusTxInner b x
-- Matching the behaviour of PlutusTx.Lift.Class.sortedCons
  TypeEncoding sop ( 'ADT _ "Bool" _ _) x = Delayed (PlutusTxInner (ScottFn (ScottList sop x) x) x)
  TypeEncoding sop ( 'ADT _ _ cons _) x = Delayed (PlutusTxInner (ScottFn (ScottList (Fst (SortedBy '(sop, NamesOf cons))) x) x) x)

type Fst :: (a, b) -> a
type family Fst x where
  Fst '(a, _) = a

type SortedBy :: ([[Type]], [ConstructorName]) -> ([[Type]], [ConstructorName])
type family SortedBy xs where
  SortedBy '((ts ': tss), (name ': names)) = Insert ts name (SortedBy '(tss, names))
  SortedBy '( '[], '[]) = '( '[], '[])

type Insert :: [Type] -> ConstructorName -> ([[Type]], [ConstructorName]) -> ([[Type]], [ConstructorName])
type family Insert ts name xs where
  Insert ts1 name1 '(ts2 ': tss, name2 : names) = Insert' (TypeLits.CmpSymbol name1 name2) ts1 name1 '(ts2 ': tss, name2 : names)
  Insert ts name '( '[], '[]) = '( '[ts], '[name])

type Insert' :: Ordering -> [Type] -> ConstructorName -> ([[Type]], [ConstructorName]) -> ([[Type]], [ConstructorName])
type family Insert' o ts name xs where
  Insert' 'GT ts1 name1 '(ts2 ': tss, name2 ': names) = Cons ts2 name2 (Insert ts1 name1 '(tss, names))
  Insert' _ ts name '(tss, names) = '(ts ': tss, name ': names)

type Cons :: a -> b -> ([a], [b]) -> ([a], [b])
type family Cons ts name xs where
  Cons ts name '(tss, names) = '(ts ': tss, name ': names)

type NamesOf :: [ConstructorInfo] -> [ConstructorName]
type family NamesOf cs where
  NamesOf ( 'Constructor name ': cs) = name ': NamesOf cs
  NamesOf ( 'Infix name _ _ ': cs) = name ': NamesOf cs
  NamesOf ( 'Record name _ ': cs) = name ': NamesOf cs
  NamesOf '[] = '[]

{- |
  List of scott-encoded constructors of a Haskell type (represented by 'SOP.Code')

  ScottList (Code (Either a b)) c = '[a -> c, b -> c]
-}
type ScottList :: [[Type]] -> Type -> [Type]
type family ScottList code c where
-- We disallow certain shapes because Scott encoding is not appropriate for them.
  ScottList '[] c = TypeError ( 'TypeLits.Text "PlutusType(scott encoding): Data type without constructors not accepted")
  ScottList '[ '[]] c =
    TypeError
      ( 'TypeLits.Text
          "PlutusType(scott encoding): Data type with single nullary constructor not accepted"
      )
  ScottList '[ '[_]] c =
    TypeError
      ( 'TypeLits.Text
          "PlutusType(scott encoding): Data type with single unary constructor not accepted; use newtype!"
      )
  ScottList (xs ': xss) c = ScottFn xs c ': ScottList' xss c

type ScottList' :: [[Type]] -> Type -> [Type]
type family ScottList' code c where
  ScottList' '[] c = '[]
  ScottList' (xs ': xss) c = ScottFn xs c ': ScottList' xss c

{- |
  An individual constructor function of a Scott encoding.

   ScottFn '[a, b] c = (a -> b -> c)
   ScottFn '[] c = c
-}
type ScottFn :: [Type] -> Type -> Type
type family ScottFn xs b where
  ScottFn '[] b = b
  ScottFn (x ': xs) b = x -> ScottFn xs b
