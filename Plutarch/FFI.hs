{-# LANGUAGE UndecidableInstances #-}

module Plutarch.FFI (
  type (>~<),
  PTxList (PTxCons, PTxNil),
  PTxMaybe (PTxJust, PTxNothing),
  foreignExport,
  foreignImport,
  opaqueExport,
  opaqueImport,
  plistFromTx,
  plistToTx,
  pmaybeFromTx,
  pmaybeToTx,
  unsafeForeignExport,
  unsafeForeignImport,
) where

import Control.Lens (over)
import Control.Monad (void)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.TypeLits qualified as TypeLits
import Generics.SOP qualified as SOP
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  ConstructorName,
  DatatypeInfo (ADT, Newtype),
 )
import Plutarch.Bool (PBool, PEq, (#==))
import Plutarch.Builtin (PData)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (
  ClosedTerm,
  Config,
  PDelayed,
  PType,
  RawTerm (RCompiled),
  S,
  Term (Term),
  TermResult (TermResult),
  compile,
  pdelay,
  pforce,
  phoistAcyclic,
  (#),
  (:-->),
 )
import Plutarch.Internal.Generic (PCode)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DPTStrat,
  DerivePlutusType,
  PlutusType (PInner, pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Witness (witness)
import Plutarch.List (PList, PListLike (PElemConstraint, pcons, pelimList, pnil), pconvertLists, plistEquals)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Script (Script (Script))
import Plutarch.Show (PShow)
import Plutarch.String (PString)
import Plutarch.Unit (PUnit)
import PlutusTx.Builtins.Internal (BuiltinBool, BuiltinByteString, BuiltinData, BuiltinUnit)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (DeserializedCode), getPlc)
import PlutusTx.Prelude (BuiltinString)
import UntypedPlutusCore (fakeNameDeBruijn)
import UntypedPlutusCore qualified as UPLC

{- | Plutarch type of lists compatible with the PlutusTx encoding of Haskell
 lists and convertible with the regular 'PList' using 'plistToTx' and
 'plistFromTx'.
-}
data PTxList (a :: PType) (s :: S)
  = PTxCons (Term s a) (Term s (PTxList a))
  | PTxNil
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, PShow)

{- | Plutarch type compatible with the PlutusTx encoding of Haskell 'Maybe' and
 convertible with the regular 'PMaybe' using 'pmaybeToTx' and 'pmaybeFromTx'.
-}
data PTxMaybe (a :: PType) (s :: S)
  = PTxJust (Term s a)
  | PTxNothing
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, PEq, PShow)

instance PEq a => PEq (PTxList a) where
  (#==) xs ys = plistEquals # xs # ys

-- | Compile and export a Plutarch term so it can be used by `PlutusTx.applyCode`.
foreignExport :: forall p t. p >~< t => Config -> ClosedTerm p -> CompiledCode t
foreignExport = let _ = witness (Proxy @(p >~< t)) in unsafeForeignExport

-- | Import compiled UPLC code (such as a spliced `PlutusTx.compile` result) as a Plutarch term.
foreignImport :: forall p t. p >~< t => CompiledCode t -> ClosedTerm p
foreignImport = let _ = witness (Proxy @(p >~< t)) in unsafeForeignImport

-- | Export Plutarch term of any type as @CompiledCode Void@.
opaqueExport :: Config -> ClosedTerm p -> CompiledCode Void
opaqueExport = unsafeForeignExport

-- | Import compiled UPLC code of any type as a Plutarch opaque term.
opaqueImport :: CompiledCode t -> ClosedTerm POpaque
opaqueImport = unsafeForeignImport

-- | Seriously unsafe, may fail at run time or result in unexpected behaviour in your on-chain validator.
unsafeForeignExport :: Config -> ClosedTerm p -> CompiledCode t
unsafeForeignExport config t = DeserializedCode program Nothing mempty
  where
    (Script (UPLC.Program _ version term)) = either (error . T.unpack) id $ compile config t
    program =
      fmap (const mempty) $
        UPLC.Program () version $
          UPLC.termMapNames fakeNameDeBruijn term

-- | Seriously unsafe, may fail at run time or result in unexpected behaviour in your on-chain validator.
unsafeForeignImport :: CompiledCode t -> ClosedTerm p
unsafeForeignImport c =
  Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm $ toNameless (void (getPlc c))) []
  where
    toNameless ::
      UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

-- | Convert a 'PList' to a 'PTxList', perhaps before exporting it with 'foreignExport'.
plistToTx :: Term s (PList a :--> PTxList a)
plistToTx = pconvertLists

-- | Convert a 'PTxList' to a 'PList', probably after importing it with 'foreignImport'.
plistFromTx :: Term s (PTxList a :--> PList a)
plistFromTx = pconvertLists

-- | Convert a 'PMaybe' to a 'PTxMaybe', perhaps before exporting it with 'foreignExport'.
pmaybeToTx :: Term s (PMaybe a :--> PTxMaybe a)
pmaybeToTx =
  plam $
    flip Plutarch.Internal.PlutusType.pmatch $
      Plutarch.Internal.PlutusType.pcon . \case
        PNothing -> PTxNothing
        PJust x -> PTxJust x

-- | Convert a 'PTxMaybe' to a 'PMaybe', probably after importing it with 'foreignImport'.
pmaybeFromTx :: Term s (PTxMaybe a :--> PMaybe a)
pmaybeFromTx =
  plam $
    flip Plutarch.Internal.PlutusType.pmatch $
      Plutarch.Internal.PlutusType.pcon . \case
        PTxNothing -> PNothing
        PTxJust x -> PJust x

newtype PTxList' a r s = PTxList' (Term s (PDelayed (r :--> (a :--> PTxList a :--> r) :--> r)))
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance Plutarch.Internal.PlutusType.DerivePlutusType (PTxList' a r) where type DPTStrat _ = PlutusTypeNewtype

instance PlutusType (PTxList a) where
  type PInner (PTxList a) = PForall (PTxList' a)
  pcon' (PTxCons x xs) = Plutarch.Internal.PlutusType.pcon $ PForall $ Plutarch.Internal.PlutusType.pcon $ PTxList' $ pdelay $ plam $ \_nil cons -> cons # x # xs
  pcon' PTxNil = Plutarch.Internal.PlutusType.pcon $ PForall $ Plutarch.Internal.PlutusType.pcon $ PTxList' $ phoistAcyclic $ pdelay $ plam const
  pmatch' elim f = Plutarch.Internal.PlutusType.pmatch elim \(PForall elim) -> pforce (pto elim) # f PTxNil # plam (\x xs -> f $ PTxCons x xs)

instance PListLike PTxList where
  type PElemConstraint PTxList _ = ()
  pelimList cons nil list = Plutarch.Internal.PlutusType.pmatch (pto list) \(PForall list) -> pforce (pto list) # nil # plam cons
  pcons = phoistAcyclic $ plam $ \x xs -> Plutarch.Internal.PlutusType.pcon (PTxCons x xs)
  pnil = Plutarch.Internal.PlutusType.pcon PTxNil

newtype PTxMaybe' a r s = PTxMaybe' (Term s (PDelayed ((a :--> r) :--> r :--> r)))
  deriving stock (Generic)
  deriving anyclass (PlutusType)
instance Plutarch.Internal.PlutusType.DerivePlutusType (PTxMaybe' a r) where type DPTStrat _ = PlutusTypeNewtype

instance PlutusType (PTxMaybe a) where
  type PInner (PTxMaybe a) = PForall (PTxMaybe' a)
  pcon' (PTxJust x) = Plutarch.Internal.PlutusType.pcon $ PForall $ Plutarch.Internal.PlutusType.pcon $ PTxMaybe' $ pdelay $ plam $ \just _nothing -> just # x
  pcon' PTxNothing = Plutarch.Internal.PlutusType.pcon $ PForall $ Plutarch.Internal.PlutusType.pcon $ PTxMaybe' $ phoistAcyclic $ pdelay $ plam $ \_just nothing -> nothing
  pmatch' elim f = Plutarch.Internal.PlutusType.pmatch elim \(PForall elim) -> pforce (pto elim) # plam (f . PTxJust) # f PTxNothing

type family F (p :: [PType]) (t :: [Type]) :: Constraint where
  F '[] '[] = ()
  F (x ': xs) (y ': ys) = (x >~< y, F xs ys)

type family G (p :: [[PType]]) (t :: [[Type]]) :: Constraint where
  G '[] '[] = ()
  G (x ': xs) (y ': ys) = (F x y, G xs ys)

-- | Equality of inner types - Plutarch on the left and Haskell on the right.
type family (p :: PType) >~< (t :: Type) :: Constraint where
  PBool >~< BuiltinBool = ()
  PInteger >~< Integer = ()
  PString >~< BuiltinString = ()
  PByteString >~< BuiltinByteString = ()
  PData >~< BuiltinData = ()
  PUnit >~< BuiltinUnit = ()
  (a :--> b) >~< (a' -> b') = (a >~< a', b >~< b')
  (PTxList a) >~< [a'] = a >~< a'
  (PTxMaybe a) >~< Maybe a' = a >~< a'
  (PDelayed p) >~< t = (Plutarch.Internal.PlutusType.DPTStrat p ~ PlutusTypeScott, G (PCode p) (TypeEncoding t))

type TypeEncoding a = (TypeEncoding' (GCode a) (GDatatypeInfoOf a))

type TypeEncoding' :: [[Type]] -> DatatypeInfo -> [[Type]]
type family TypeEncoding' a rep where
  TypeEncoding' '[ '[b]] ('Newtype _ _ _) = TypeEncoding b
  -- Matching the behaviour of PlutusTx.Lift.Class.sortedCons
  TypeEncoding' sop ('ADT _ "Bool" _ _) = sop
  TypeEncoding' sop ('ADT _ _ cons _) = Fst (SortedBy '(sop, NamesOf cons))

type Fst :: (a, b) -> a
type family Fst x where
  Fst '(a, _) = a

type SortedBy :: ([[Type]], [ConstructorName]) -> ([[Type]], [ConstructorName])
type family SortedBy xs where
  SortedBy '(ts ': tss, name ': names) = Insert ts name (SortedBy '(tss, names))
  SortedBy '( '[], '[]) = '( '[], '[])

type Insert :: [Type] -> ConstructorName -> ([[Type]], [ConstructorName]) -> ([[Type]], [ConstructorName])
type family Insert ts name xs where
  Insert ts1 name1 '(ts2 ': tss, name2 ': names) = Insert' (TypeLits.CmpSymbol name1 name2) ts1 name1 '(ts2 ': tss, name2 ': names)
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
  NamesOf ('Constructor name ': cs) = name ': NamesOf cs
  NamesOf ('Infix name _ _ ': cs) = name ': NamesOf cs
  NamesOf ('Record name _ ': cs) = name ': NamesOf cs
  NamesOf '[] = '[]
