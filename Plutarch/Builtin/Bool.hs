{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Bool (
  -- * Type
  PBool (..),

  -- * Builtin
  pbuiltinIfThenElse,

  -- * Functions
  pif',
  pif,
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
) where

import Data.Kind (Type)
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable,
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType (PInner, pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  PDelayed,
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  punsafeBuiltin,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC

{- | Builtin Plutus boolean.

@since WIP
-}
data PBool (s :: S) = PTrue | PFalse
  deriving stock
    ( -- | @since WIP
      Show
    )

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBool Bool)
  instance
    PLiftable PBool

-- | @since WIP
instance PlutusType PBool where
  type PInner PBool = PBool
  {-# INLINEABLE pcon' #-}
  pcon' =
    pconstant . \case
      PTrue -> True
      PFalse -> False
  {-# INLINEABLE pmatch' #-}
  pmatch' b f =
    pforce . pforce $ pbuiltinIfThenElse # b # pdelay (f PTrue) # pdelay (f PFalse)

-- | @since WIP
pbuiltinIfThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> PDelayed a)
pbuiltinIfThenElse = punsafeBuiltin PLC.IfThenElse

{- | Strict if-then-else. Emits slightly less code than the lazy version.

@since WIP
-}
pif' ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ plam $ \cond ifT ifF ->
  pforce $ pbuiltinIfThenElse # cond # ifT # ifF

{- | Lazy if-then-else.

@since WIP
-}
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif cond ifT ifF = pmatch cond $ \case
  PTrue -> ifT
  PFalse -> ifF

{- | Boolean negation.

@since WIP
-}
pnot ::
  forall (s :: S).
  Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x ->
  pif' # x # pcon PFalse # pcon PTrue

{- | Lazy AND for terms.

@since WIP
-}
(#&&) :: forall (s :: S). Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

infixr 3 #&&

{- | Lazy OR for terms.

@since WIP
-}
(#||) :: forall (s :: S). Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

infixr 2 #||

{- | Hoisted lazy AND at the Plutarch level.

@since WIP
-}
pand :: forall (s :: S). Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # phoistAcyclic (pdelay $ pcon PFalse)

{- | As 'pand', but strict.

@since WIP
-}
pand' :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # pcon PFalse

{- | Hoisted lazy OR at the Plutarch level.

@since WIP
-}
por :: forall (s :: S). Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # phoistAcyclic (pdelay $ pcon PTrue)

{- | As 'por', but strict.

@since WIP
-}
por' :: Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # pcon PTrue
