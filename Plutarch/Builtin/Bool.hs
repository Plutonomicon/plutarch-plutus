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
  pcond,
  ptrue,
  pfalse,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Data
import {-# SOURCE #-} Plutarch.Builtin.Integer
import Plutarch.Internal.Eq
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable,
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.Ord
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
  plet,
  punsafeBuiltin,
  (#),
  (#$),
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

instance PEq PBool where
  {-# INLINEABLE (#==) #-}
  x #== y' = plet y' $ \y -> pif' # x # y #$ pnot # y

instance POrd PBool where
  {-# INLINEABLE (#<) #-}
  x #< y = pif' # x # pconstant False # y
  {-# INLINEABLE (#<=) #-}
  x #<= y = pif' # x # y # pconstant True
  {-# INLINEABLE pmin #-}
  pmin x y = pand' # x # y
  {-# INLINEABLE pmax #-}
  pmax x y = por' # x # y

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
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

instance PIsData PBool where
  pfromDataImpl x =
    phoistAcyclic (plam toBool) # pforgetData x
    where
      toBool :: Term s PData -> Term s PBool
      toBool d = pfstBuiltin # (punsafeBuiltin PLC.UnConstrData # d) #== 1

  pdataImpl x =
    phoistAcyclic (plam toData) # x
    where
      toData :: Term s PBool -> Term s PData
      toData b =
        punsafeBuiltin PLC.ConstrData
          # (pif' # b # 1 # (0 :: Term s PInteger))
          # nil

      nil :: Term s (PBuiltinList PData)
      nil = pnil

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
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

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

{- | Essentially multi-way 'pif'. More precisely, given a list of
condition-action pairs, and an \'action of last resort\', construct a
left-to-right \'chain\' of @pif@s, using the conditions to determine which
action gets taken. The \'action of last resort\' finishes the \'chain\'. For
example:

> pcond [(cond1, act1), (cond2, act2)] act3

does the same thing as

> pif cond1 act1 (pif cond2 act2 act3)

@since WIP
-}
pcond ::
  forall (a :: S -> Type) (s :: S).
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pcond conds lastResort = case conds of
  [] -> lastResort
  (cond, action) : conds' -> pif cond action (pcond conds' lastResort)

ptrue :: Term s PBool
ptrue = pconstant True

pfalse :: Term s PBool
ptrue = pconstant False
