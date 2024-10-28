{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types and functions that are integral to Plutarch, or primitive to UPLC.
module Plutarch.Internal.Builtin (
  -- * Types
  POpaque (..),
  PInteger (..),
  PBool (..),
  PString (..),

  -- * Functions

  -- ** POpaque
  popaque,

  -- ** PInteger
  pbuiltinAddInteger,
  pbuiltinSubtractInteger,
  pbuiltinMultiplyInteger,
  pbuiltinDivideInteger,
  pbuiltinQuotientInteger,
  pbuiltinRemainderInteger,
  pbuiltinModInteger,
  pbuiltinExpModInteger,
  pbuiltinEqualsInteger,
  pbuiltinLessThanInteger,
  pbuiltinLessThanEqualsInteger,

  -- ** PBool
  pnot,
  pand,
  por,
  pand',
  por',
  (#&&),
  (#||),
  pif,
  pbuiltinIfThenElse,

  -- ** PString
  pbuiltinEqualsString,

  -- ** Other
  pfix,
  pto,
  pbuiltinTrace,
  plam,
  pinl,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType (
    PContravariant',
    PCovariant',
    PInner,
    PVariant',
    pcon',
    pmatch'
  ),
  pcon,
  pmatch,
 )
import Plutarch.Internal.PrettyStack (prettyStack)
import Plutarch.Internal.Term (
  Config (Tracing),
  PDelayed,
  S,
  Term,
  TracingMode (DoTracingAndBinds),
  pdelay,
  pforce,
  pgetConfig,
  phoistAcyclic,
  plam',
  punsafeBuiltin,
  punsafeCoerce,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
  pconstant,
 )
import PlutusCore qualified as PLC

-- An arbitrary term whose type is unknown.
--
-- @since WIP
newtype POpaque (s :: S) = POpaque (Term s POpaque)

-- | @since WIP
instance PlutusType POpaque where
  type PInner POpaque = POpaque
  type PCovariant' POpaque = ()
  type PContravariant' POpaque = ()
  type PVariant' POpaque = ()
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

{- | A Plutus integer.

@since WIP
-}
newtype PInteger (s :: S) = PInteger (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PInteger where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PInteger where
  type PLifted PInteger = Integer

-- | @since WIP
deriving via
  (DerivePConstantDirect Integer PInteger)
  instance
    PConstantDecl Integer

{- | A Plutus boolean.

@since WIP
-}
data PBool (s :: S) = PTrue | PFalse
  deriving stock
    ( -- | @since WIP
      Show
    )

-- | @since WIP
instance PUnsafeLiftDecl PBool where
  type PLifted PBool = Bool

-- | @since WIP
deriving via
  (DerivePConstantDirect Bool PBool)
  instance
    PConstantDecl Bool

-- | @since WIP
instance PlutusType PBool where
  type PInner PBool = PBool
  pcon' PTrue = pconstant True
  pcon' PFalse = pconstant False
  pmatch' b f =
    pforce $ pbuiltinIfThenElse # b # pdelay (f PTrue) # pdelay (f PFalse)

{- | A Plutus string.

@since WIP
-}
newtype PString (s :: S) = PString (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PString where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PString where
  type PLifted PString = Text

-- | @since WIP
deriving via
  (DerivePConstantDirect Text PString)
  instance
    PConstantDecl Text

-- | @since WIP
instance IsString (Term s PString) where
  {-# INLINEABLE fromString #-}
  fromString = pconstant . Text.pack

{- | Forget the type of a term.

@since WIP
-}
popaque ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs.

  @since WIP
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))

{- |
  Safely coerce from a Term to it's 'PInner' representation.

  @since WIP
-}
pto :: Term s a -> Term s (PInner a)
pto = punsafeCoerce

-- | @since WIP
pbuiltinAddInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinAddInteger = punsafeBuiltin PLC.AddInteger

-- | @since WIP
pbuiltinSubtractInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinSubtractInteger = punsafeBuiltin PLC.SubtractInteger

-- | @since WIP
pbuiltinMultiplyInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinMultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

-- | @since WIP
pbuiltinDivideInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinDivideInteger = punsafeBuiltin PLC.DivideInteger

-- | @since WIP
pbuiltinQuotientInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinQuotientInteger = punsafeBuiltin PLC.QuotientInteger

-- | @since WIP
pbuiltinRemainderInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinRemainderInteger = punsafeBuiltin PLC.RemainderInteger

-- | @since WIP
pbuiltinModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinModInteger = punsafeBuiltin PLC.ModInteger

-- | @since WIP
pbuiltinExpModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pbuiltinExpModInteger = punsafeBuiltin PLC.ExpModInteger

{- | A strict if-then-else; both branches get evaluated regardless. Emits
slightly less code than 'pif'.

@since WIP
-}
pbuiltinIfThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pbuiltinIfThenElse = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | @since WIP
pbuiltinLessThanInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinLessThanInteger = punsafeBuiltin PLC.LessThanInteger

-- | @since WIP
pbuiltinLessThanEqualsInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinLessThanEqualsInteger = punsafeBuiltin PLC.LessThanEqualsInteger

{- | A lazy if-then-else.

@since WIP
-}
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif b ifTrue ifFalse = pmatch b $ \case
  PTrue -> ifTrue
  PFalse -> ifFalse

-- | @since WIP
pnot ::
  forall (s :: S).
  Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x ->
  pbuiltinIfThenElse # x # pcon PFalse # pcon PTrue

-- | @since WIP
pbuiltinTrace :: Term s (PString :--> a :--> a)
pbuiltinTrace = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

-- Helpers

class PLamN (a :: Type) (b :: S -> Type) (s :: S) | a -> b, s b -> a where
  -- | Lambda abstraction with any number of arguments.
  -- @since WIP
  plam ::
    forall (c :: S -> Type).
    HasCallStack =>
    (Term s c -> a) ->
    Term s (c :--> b)

instance {-# OVERLAPPABLE #-} a' ~ Term s a => PLamN a' a s where
  {-# INLINEABLE plam #-}
  plam f =
    let cs = callStack
     in plam' $ \x -> pgetConfig $ \case
          -- Note: This works because at the moment, DoTracingAndBinds is the most
          -- general tracing mode.
          Tracing _ DoTracingAndBinds ->
            pforce $ pbuiltinTrace # mkString (prettyStack "L" cs) # pdelay (f x)
          _ -> f x

mkString :: forall (a :: S -> Type) (s :: S). Text -> Term s a
mkString x = punsafeConstantInternal $ PLC.someValue @Text @PLC.DefaultUni x

instance (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s where
  {-# INLINEABLE plam #-}
  plam f = withFrozenCallStack $ plam' $ \x -> plam (f x)

{- | Flipped application for terms.

@since WIP
-}
pinl ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a ->
  (Term s a -> Term s b) ->
  Term s b
pinl v f = f v

{- | Lazy boolean and as a Plutarch function.

@since WIP
-}
pand ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # y # phoistAcyclic (pdelay $ pcon PFalse)

{- | Lazy boolean and on 'Term's.

@since WIP
-}
(#&&) ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
x #&& y = pforce $ pand # x # pdelay y

infixr 3 #&&

{- | As 'pand', but strict.

@since WIP
-}
pand' ::
  forall (s :: S).
  Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # y # x

{- | Lazy boolean inclusive or as a Plutarch function.

@since WIP
-}
por ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x ->
  pbuiltinIfThenElse # x # phoistAcyclic (pdelay $ pcon PTrue)

{- | Lazy boolean inclusive or on 'Term's.

@since WIP
-}
(#||) ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
x #|| y = pforce $ por # x # pdelay y

infixr 2 #||

{- | As 'por', but strict.

@since WIP
-}
por' ::
  forall (s :: S).
  Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # x # y

-- | @since WIP
pbuiltinEqualsInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinEqualsInteger = punsafeBuiltin PLC.EqualsInteger

-- | @since WIP
pbuiltinEqualsString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PBool)
pbuiltinEqualsString = punsafeBuiltin PLC.EqualsString
