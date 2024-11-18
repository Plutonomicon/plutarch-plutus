{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types and functions that are integral to Plutarch, or primitive to UPLC.
module Plutarch.Internal.Builtin (
  -- * Functions

  -- ** PBool
  pnot,
  pand,
  por,
  pand',
  por',
  (#&&),
  (#||),
  pif,
  pif',

  -- ** Other
  pfix,
  pto,
  pbuiltinTrace,
  plam,
  pinl,
  punsafeDowncast,
) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pbuiltinIfThenElse)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Builtin.String (PString)
import Plutarch.Internal.PlutusType (
  PlutusType (PInner),
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
import PlutusCore qualified as PLC

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

{- | A strict if-then-else; both branches get evaluated regardless.

@since WIP
-}
pif' ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ plam $ \cond x y ->
  pforce $ pbuiltinIfThenElse # cond # x # y

-- | @since WIP
pnot ::
  forall (s :: S).
  Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x ->
  pforce $ pbuiltinIfThenElse # x # pcon PFalse # pcon PTrue

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
  pforce $ pbuiltinIfThenElse # x # y # phoistAcyclic (pdelay $ pcon PFalse)

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
  pforce $ pbuiltinIfThenElse # x # y # x

{- | Lazy boolean inclusive or as a Plutarch function.

@since WIP
-}
por ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # pcon PTrue # pforce y

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
  pforce $ pbuiltinIfThenElse # x # x # y

-- | @since WIP
punsafeDowncast ::
  forall (a :: S -> Type) (s :: S).
  Term s (PInner a) ->
  Term s a
punsafeDowncast = punsafeCoerce
