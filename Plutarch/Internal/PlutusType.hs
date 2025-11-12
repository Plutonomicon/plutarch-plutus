{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.PlutusType (
  PlutusType,
  PInnermost,
  PlutusTypeStratConstraint,
  pcon',
  pmatch',
  pmatch,
  pcon,
  PInner,
  PlutusTypeStrat,
  DerivePlutusType,
  DPTStrat,
  DerivedPInner,
  derivedPCon,
  derivedPMatch,
  DeriveNewtypePlutusType (DeriveNewtypePlutusType, unDeriveNewtypePlutusType),
  DeriveFakePlutusType (DeriveFakePlutusType),
) where

import Plutarch.Builtin.Array (PArray (PArray))
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
 )
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pfalse, pif', ptrue)
import Plutarch.Builtin.ByteString (
  PByte,
  PByteString,
  PEndianness,
  PLogicOpSemantics,
 )
import Plutarch.Builtin.Data (
  PAsData (PAsData),
  PBuiltinList (PCons, PNil),
  PBuiltinPair (PBuiltinPair),
  PData (PData),
  pchooseListBuiltin,
  pconsBuiltin,
  pfstBuiltin,
  pheadBuiltin,
  psndBuiltin,
  ptailBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque (POpaque))
import Plutarch.Builtin.String (PString, ptraceInfo)
import Plutarch.Builtin.Unit (PUnit (PUnit), punit)

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Generics.SOP (
  Code,
  I (I),
  NP (Nil, (:*)),
  NS (Z),
  SOP (SOP),
 )
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint (Head)
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
import {-# SOURCE #-} Plutarch.Internal.IsData (
  PIsData,
  pdata,
  pfromData,
 )
import {-# SOURCE #-} Plutarch.Internal.Lift (
  PlutusRepr,
  getPLifted,
  unsafeHaskToUni,
 )
import Plutarch.Internal.Quantification (PFix (PFix), PForall (PForall), PSome (PSome))
import Plutarch.Internal.Term (
  S,
  Term,
  pdelay,
  perror,
  pforce,
  plam',
  plet,
  punsafeCoerce,
  (#),
  (:-->) (PLam),
 )
import Plutarch.Internal.Witness (witness)
import PlutusCore qualified as PLC

type family PInnermost' (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  PInnermost' a a = a
  PInnermost' a _b = PInnermost' (PInner a) a

type PInnermost a = PInnermost' (PInner a) a

{-# DEPRECATED PlutusTypeStrat "Use the new mechanisms instead" #-}
class PlutusTypeStrat (strategy :: Type) where
  type PlutusTypeStratConstraint strategy :: (S -> Type) -> Constraint
  type DerivedPInner strategy (a :: S -> Type) :: S -> Type
  derivedPCon :: forall a s. (DerivePlutusType a, DPTStrat a ~ strategy) => a s -> Term s (DerivedPInner strategy a)
  derivedPMatch :: forall a s b. (DerivePlutusType a, DPTStrat a ~ strategy) => Term s (DerivedPInner strategy a) -> (a s -> Term s b) -> Term s b

{-# DEPRECATED DerivePlutusType "Use the new mechanisms instead" #-}
class
  ( PInner a ~ DerivedPInner (DPTStrat a) a
  , PlutusTypeStrat (DPTStrat a)
  , PlutusTypeStratConstraint (DPTStrat a) a
  , PlutusType a
  ) =>
  DerivePlutusType (a :: S -> Type)
  where
  type DPTStrat a :: Type
  type DPTStrat a = TypeError ('Text "Please specify a strategy for deriving PlutusType for type " ':<>: 'ShowType a)

class PlutusType (a :: S -> Type) where
  type PInner a :: S -> Type
  type PInner a = DerivedPInner (DPTStrat a) a
  pcon' :: forall s. a s -> Term s (PInner a)
  default pcon' :: DerivePlutusType a => forall s. a s -> Term s (PInner a)
  pcon' = let _ = witness (Proxy @(PlutusType a)) in derivedPCon

  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  -- FIXME buggy GHC, needs AllowAmbiguousTypes
  default pmatch' :: DerivePlutusType a => forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  pmatch' = derivedPMatch

-- | Construct a Plutarch Term via a Haskell datatype
pcon :: PlutusType a => a s -> Term s a
pcon x = punsafeCoerce (pcon' x)

-- | Pattern match over Plutarch Terms via a Haskell datatype
pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x = pmatch' (punsafeCoerce x)

instance PlutusType (a :--> b) where
  type PInner (a :--> b) = a :--> b
  pcon' (PLam f) = plam' f
  pmatch' f g = plet f \f' -> g (PLam (f' #))

instance PlutusType (PForall f) where
  type PInner (PForall f) = PForall f
  pcon' (PForall x) = punsafeCoerce x
  pmatch' x f = f (PForall $ punsafeCoerce x)

instance PlutusType (PSome f) where
  type PInner (PSome f) = PSome f
  pcon' (PSome x) = punsafeCoerce x
  pmatch' x f = f (PSome $ punsafeCoerce x)

instance PlutusType (PFix f) where
  type PInner (PFix f) = f (PFix f)
  pcon' (PFix x) = x
  pmatch' x f = f (PFix x)

--------------------------------------------------------------------------------

data PlutusTypeNewtype

class (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: S -> Type)
instance (PGeneric a, PCode a ~ '[ '[GetPNewtype a]]) => Helper (a :: S -> Type)

instance PlutusTypeStrat PlutusTypeNewtype where
  type PlutusTypeStratConstraint PlutusTypeNewtype = Helper
  type DerivedPInner PlutusTypeNewtype a = GetPNewtype a
  derivedPCon x = case gpfrom x of
    SOP.SOP (SOP.Z (x SOP.:* SOP.Nil)) -> x
    SOP.SOP (SOP.S x) -> case x of {}
  derivedPMatch x f = f (gpto $ SOP.SOP $ SOP.Z $ x SOP.:* SOP.Nil)

type family GetPNewtype' (a :: [[S -> Type]]) :: S -> Type where
  GetPNewtype' '[ '[a]] = a

type family GetPNewtype (a :: S -> Type) :: S -> Type where
  GetPNewtype a = GetPNewtype' (PCode a)

--------------------------------------------------------------------------------

-- | @since 1.10.0
newtype DeriveNewtypePlutusType (a :: S -> Type) s = DeriveNewtypePlutusType
  { unDeriveNewtypePlutusType :: a s
  -- ^ @since 1.10.0
  }

-- Helpers

type family UnTermSingle (x :: Type) :: S -> Type where
  UnTermSingle (Term _ a) = a

class (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt
instance (SOP.Generic (a s), Code (a s) ~ '[ '[Term s pt]]) => H s a pt

instance
  forall (a :: S -> Type) (pt :: S -> Type).
  ( pt ~ UnTermSingle (Head (Head (Code (a Any))))
  , forall s. H s a pt
  ) =>
  PlutusType (DeriveNewtypePlutusType a)
  where
  -- Note:
  -- This is not @PInner (DeriveNewtypePlutusType a) = PInner a@ because
  -- We want the PInner of wrapper type to be the type it wraps not the PInner of that.
  type PInner (DeriveNewtypePlutusType a) = UnTermSingle (Head (Head (Code (a Any))))

  -- This breaks without type signature because of (s :: S) needs to be bind.
  pcon' :: forall s. DeriveNewtypePlutusType a s -> Term s (PInner (DeriveNewtypePlutusType a))
  pcon' (DeriveNewtypePlutusType x) =
    case SOP.unZ $ SOP.unSOP (SOP.from x :: SOP I '[ '[Term s pt]]) of
      (I x) :* Nil -> x :: Term s pt

  pmatch' :: forall s b. Term s (PInner (DeriveNewtypePlutusType a)) -> (DeriveNewtypePlutusType a s -> Term s b) -> Term s b
  pmatch' x f =
    f (DeriveNewtypePlutusType $ SOP.to ((SOP $ Z $ I x :* Nil) :: SOP I '[ '[Term s pt]]))

--------------------------------------------------------------------------------

{- |
This is a cursed derivation strategy that will give you @PlutusType@ with no questions asked. This is occasionally helpful
for deriving @PlutusType@ for another derivation strategy wrapper whose target instance requires @PlutusType@ as superclass.

See @PLiftable@
-}
newtype DeriveFakePlutusType (a :: S -> Type) (s :: S) = DeriveFakePlutusType (a s)

instance PlutusType (DeriveFakePlutusType a) where
  type PInner (DeriveFakePlutusType a) = TypeError ('ShowType a ':<>: 'Text " derived PlutusType with DeriveFakePlutusType. This type is not meant to be used as PlutusType.")

  -- This breaks without type signature because of (s :: S) needs to be bind.
  pcon' :: forall s. DeriveFakePlutusType a s -> Term s (PInner (DeriveFakePlutusType a))
  pcon' _ = error "Attepted to use a type derived with DeriveFakePlutusType"

  pmatch' :: forall s b. Term s (PInner (DeriveFakePlutusType a)) -> (DeriveFakePlutusType a s -> Term s b) -> Term s b
  pmatch' _ _ = error "Attepted to use a type derived with DeriveFakePlutusType"

--------------------------------------------------------------------------------

deriving via (DeriveNewtypePlutusType PInteger) instance PlutusType PInteger

instance PlutusType POpaque where
  type PInner POpaque = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | @since 1.10.0
instance PlutusType PBool where
  type PInner PBool = PBool
  {-# INLINEABLE pcon' #-}
  pcon' PTrue = ptrue
  pcon' PFalse = pfalse
  {-# INLINEABLE pmatch' #-}
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

instance PlutusType PData where
  type PInner PData = PData
  pcon' (PData t) = t
  pmatch' t f = f (PData t)

{- | = Important note

Due to some weirdnesses regarding builtins, 'PBuiltinPair's cannot be
constructed from anything that's not already @Data@-encoded, but as builtin
pairs are, well, /built-in/, we can lift, and 'pmatch', them just fine. Thus,
you should /not/ use 'pcon' for 'PBuiltinPair'.

@since 1.12.0
-}
instance PlutusType (PBuiltinPair a b) where
  type PInner (PBuiltinPair a b) = PBuiltinPair a b
  pcon' _ = ptraceInfo "Do not use pcon for PBuiltinPair; instead, use ppairDataBuiltin or pconstant" perror
  pmatch' t f = f (PBuiltinPair (pfstBuiltin # t) (psndBuiltin # t))

instance PLC.Contains PLC.DefaultUni (PlutusRepr a) => PlutusType (PBuiltinList a) where
  type PInner (PBuiltinList a) = PBuiltinList a
  pcon' (PCons x xs) = pconsBuiltin # x # xs
  pcon' PNil = getPLifted $ unsafeHaskToUni @[PlutusRepr a] []
  pmatch' xs' f = plet xs' $ \xs ->
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pheadBuiltin # xs) (ptailBuiltin # xs)))

instance PIsData a => PlutusType (PAsData a) where
  type PInner (PAsData a) = PData
  pcon' (PAsData t) = punsafeCoerce $ pdata t
  pmatch' t f = f (PAsData $ pfromData $ punsafeCoerce t)

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PByteString) instance PlutusType PByteString

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PByte) instance PlutusType PByte

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PLogicOpSemantics) instance PlutusType PLogicOpSemantics

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PString) instance PlutusType PString

instance PlutusType PUnit where
  type PInner PUnit = PUnit
  pcon' PUnit = punit
  pmatch' x f = plet x \_ -> f PUnit

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PBuiltinBLS12_381_G1_Element) instance PlutusType PBuiltinBLS12_381_G1_Element

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PBuiltinBLS12_381_G2_Element) instance PlutusType PBuiltinBLS12_381_G2_Element

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PBuiltinBLS12_381_MlResult) instance PlutusType PBuiltinBLS12_381_MlResult

-- | @since 1.10.0
deriving via (DeriveNewtypePlutusType PEndianness) instance PlutusType PEndianness

-- | @since 1.11.0
instance PlutusType (PArray a) where
  type PInner (PArray a) = PArray a
  pcon' (PArray t) = t
  pmatch' x f = f (PArray x)
