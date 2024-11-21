{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Bool (
  PPartialOrd (..),
  POrd (..),
  pif,
  pif',
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
  PSBool (..),
  pmatchStrict,
  pstrue,
  psfalse,
  psif,
  psif',
  psnot,
  psand,
  psand',
  psor,
  psor',
) where

import Plutarch.Builtin.Bool (PBool (PFalse, PTrue))
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.Other (
  pto,
 )
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.Term (
  PDelayed,
  PType,
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{- | Partial ordering relation.

= Laws

'#<=' must form a partial order. More precisely:

1. @x #<= x@ @=@ @True@ (@#<=@ is reflexive)
2. @(x #<= y) #&& (y #<= x)@ @=@ @x #== y@ (@#<=@ is anti-symmetric)
3. @(x #<= y) #&& (y #<= z)@ @=@ @x #<= z@ (@#<= is transitive)

Furthermore, '#<' must be an equivalent strict partial order to '#<=':

4. @x #<= y@ @=@ @(x #< y) #|| (x #== y)@
5. @x #< x@ @=@ @False@ (@#<@ is irreflexive)
6. @x #< y@ @=@ @pnot (y #< x)@ (@#<@ is asymmetric)
7. @(x #< y) #&& (y #< z)@ @=@ @x #< z@ (@#<@ is transitive)

Lastly, if you define '#>=' or '#>', ensure that the following also hold:

8. @x #> y@ @=@ @y #< x@
9. @x #>= y@ @=@ @pnot (x #< y)@

The default implementations of '#>=' and '#>' ensure these laws.
-}
class PEq t => PPartialOrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  default (#<=) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #<= y = pto x #<= pto y
  (#<) :: Term s t -> Term s t -> Term s PBool
  default (#<) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #< y = pto x #< pto y

  -- | @since WIP
  (#>=) :: forall (s :: S). Term s t -> Term s t -> Term s PBool
  x #>= y = pnot #$ x #< y

  -- | @since WIP
  (#>) :: forall (s :: S). Term s t -> Term s t -> Term s PBool
  x #> y = y #< x

infix 4 #<=
infix 4 #<
infix 4 #>=
infix 4 #>

{- | Total ordering relation.

= Laws

'pmax' and 'pmin' must form a commutative semiring without identity
elements, where addition also distributes over multiplication. More
precisely:

1. @pmax x y@ @=@ @pmax y x@ (@pmax@ is commutative)
2. @pmin x y@ @=@ @pmin y x@ (@pmin@ is commutative)
3. @pmax x (pmax y z)@ @=@ @pmax (pmax x y) z@ (@pmax@ is associative)
4. @pmin x (pmin y z)@ @=@ @pmin (pmin x y) z@ (@pmin@ is associative)
5. @pmax x (pmin y z)@ @=@ @pmin (pmax x y) (pmax x z)@ (@pmax@ distributes
   over @pmin@)
6. @pmin x (pmax y z)@ @=@ @pmax (pmin x y) (pmin x z)@ (@pmin@ distributes
   over @pmax@)

Furthermore, the following must hold relative '#<':

7. @pmin x y@ @=@ @if (x #< y) then x else y@
8. @pmax x y@ @=@ @if (x #< y) then y else x@

Laws 7 and 8 are also the defaults, as for most types, this is the best you
can do.
-}
class PPartialOrd t => POrd t where
  -- | @since WIP
  pmax :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmax x y = pif' # (x #< y) # y # x

  -- | @since WIP
  pmin :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmin x y = pif' # (x #< y) # x # y

instance PEq PBool where
  x #== y' = plet y' $ \y -> pif' # x # y #$ pnot # y

instance PPartialOrd PBool where
  x #< y = pif' # x # pconstant False # y
  x #<= y = pif' # x # y # pconstant True

instance POrd PBool

{- | Strict version of 'pif'.
 Emits slightly less code.
-}
pif' :: Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false

-- | Boolean negation for 'PBool' terms.
pnot :: Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x -> pif' # x # pcon PFalse # pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

-- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pand :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # phoistAcyclic (pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand' :: Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # pcon PFalse

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
por :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # phoistAcyclic (pdelay $ pcon PTrue)

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por' :: Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # pcon PTrue

-- | 'PInner' of 'PSBool'.
newtype PSBoolRaw (a :: PType) (s :: S) = PSBoolRaw (Term s (a :--> a :--> a))

instance PlutusType (PSBoolRaw a) where
  type PInner (PSBoolRaw a) = a :--> a :--> a
  pcon' (PSBoolRaw x) = x
  pmatch' x f = f (PSBoolRaw x)

-- | Scott-encoded bool.
data PSBool (s :: S)
  = PSTrue
  | PSFalse
  deriving stock (Eq, Ord, Show)

instance PlutusType PSBool where
  type PInner PSBool = PForall PSBoolRaw
  pcon' PSTrue = pcon $ PForall $ pcon $ PSBoolRaw $ plam const
  pcon' PSFalse = pcon $ PForall $ pcon $ PSBoolRaw $ plam (const id)
  pmatch' x' f =
    pmatch x' $ \(PForall raw) ->
      pmatch raw $ \(PSBoolRaw x) ->
        pforce $ x # pdelay (f PSTrue) # pdelay (f PSFalse)

-- | Strict version of 'pmatch' for 'PSBool'.
pmatchStrict ::
  forall (r :: PType) (s :: S).
  Term s PSBool ->
  (PSBool s -> Term s r) ->
  Term s r
pmatchStrict x' f =
  pmatch (pto x') $ \(PForall raw) ->
    pmatch raw $ \(PSBoolRaw x) ->
      x # f PSTrue # f PSFalse

pstrue :: forall (s :: S). Term s PSBool
pstrue = pcon PSTrue

psfalse :: forall (s :: S). Term s PSBool
psfalse = pcon PSFalse

-- | Strict @if@ on Scott-encoded bool.
psif' :: forall (s :: S) (a :: PType). Term s PSBool -> Term s a -> Term s a -> Term s a
psif' b t f = pmatchStrict b \case
  PSTrue -> t
  PSFalse -> f

-- | Lazy @if@ on Scott-encoded bool.
psif :: forall (s :: S) (a :: PType). Term s PSBool -> Term s a -> Term s a -> Term s a
psif b t f = pforce $ psif' b (pdelay t) (pdelay f)

-- | @not@ on Scott-encoded bool.
psnot :: forall (s :: S). Term s PSBool -> Term s PSBool
psnot b = psif' b psfalse pstrue

psand' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand' a b = psif' a b psfalse

psand :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand a b = psif a b psfalse

psor' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor' a = psif' a pstrue

psor :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor a = psif a pstrue
