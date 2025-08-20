{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Scott (
  PScottStruct (PScottStruct, unPScottStruct),
  PScottRec (PScottRec, unPScottRec),
  PScottStructInner (PScottStructInner),
  PScottRecInner (PScottRecInner),
  DeriveAsScottStruct (DeriveAsScottStruct, unDeriveAsScottStruct),
  DeriveAsScottRec (DeriveAsScottRec, unDeriveAsScottRec),
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import Generics.SOP (Code, NP (Nil, (:*)), NS (S, Z), SOP (SOP))
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint (All, All2, Head, SListI, SListI2)
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PInner,
  PlutusType,
  pcon,
  pcon',
  pmatch,
  pmatch',
 )
import Plutarch.Internal.Quantification (PForall)
import Plutarch.Internal.Term (
  PDelayed,
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  plam',
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Repr.Internal (
  PRec (PRec, unPRec),
  PStruct (PStruct, unPStruct),
  RecTypePrettyError,
  StructSameRepr,
  UnTermRec,
  UnTermStruct,
  grecEq,
  gstructEq,
  pletL,
 )

-- | @since 1.10.0
newtype PScottStruct (struct :: [[S -> Type]]) (s :: S) = PScottStruct
  { unPScottStruct :: PStruct struct s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance forall struct. (SListI2 struct, PScottStructConstraint struct) => PlutusType (PScottStruct struct) where
  type PInner (PScottStruct struct) = PForall (PScottStructInner struct) -- try `Any`
  pcon' (PScottStruct x) = punsafeCoerce $ pconScottStruct @struct x
  pmatch' x f = pmatchScottStruct @struct (punsafeCoerce x) (f . PScottStruct)

-- NOTE/TODO: Performance has not been tested for this. I feel like this would be terribly slow
-- NOTE: Hoisting here is so that there's no duplicated computation on each argument.
-- GHC freaks out when I put hoist part in a let.

-- | @since 1.10.0
instance (PlutusType (PScottStruct struct), SListI2 struct, All2 PEq struct) => PEq (PScottStruct struct) where
  x #== y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pmatch x' $ \(PScottStruct (PStruct x'')) ->
            pmatch y' $ \(PScottStruct (PStruct y'')) ->
              gstructEq x'' y''
      )
      # x
      # y

-- | @since 1.10.0
newtype PScottRec (struct :: [S -> Type]) (s :: S) = PScottRec
  { unPScottRec :: PRec struct s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance SListI struct => PlutusType (PScottRec struct) where
  type PInner (PScottRec struct) = PForall (PScottRecInner struct)
  pcon' (PScottRec x) = punsafeCoerce $ pconScottRec x
  pmatch' x f = pmatchScottRec (punsafeCoerce x) (f . PScottRec)

-- | @since 1.10.0
instance All PEq struct => PEq (PScottRec struct) where
  x #== y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pmatch x' $ \(PScottRec (PRec x'')) ->
            pmatch y' $ \(PScottRec (PRec y'')) ->
              grecEq x'' y''
      )
      # x
      # y

-- This could be better

-- | @since 1.10.0
newtype PScottStructInner a r s = PScottStructInner (Term s (ScottFn (ScottList a r) r))

-- | @since 1.10.0
newtype PScottRecInner a r s = PScottRecInner (Term s (ScottFn a r))

-- | @since 1.10.0
newtype DeriveAsScottStruct (a :: S -> Type) s = DeriveAsScottStruct
  { unDeriveAsScottStruct :: a s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance
  forall (a :: S -> Type) (struct :: [[S -> Type]]).
  ( SOP.Generic (a Any)
  , struct ~ UnTermStruct (a Any)
  , SListI2 struct
  , forall s. StructSameRepr s a struct
  , PScottStructConstraint struct
  ) =>
  PlutusType (DeriveAsScottStruct a)
  where
  type PInner (DeriveAsScottStruct a) = PScottStruct (UnTermStruct (a Any))
  pcon' (DeriveAsScottStruct x) =
    pcon @(PScottStruct (UnTermStruct (a Any))) $ PScottStruct $ PStruct $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch @(PScottStruct (UnTermStruct (a Any))) x (f . DeriveAsScottStruct . SOP.to . SOP.hcoerce . unPStruct . unPScottStruct)

-- | @since 1.10.0
newtype DeriveAsScottRec (a :: S -> Type) s = DeriveAsScottRec
  { unDeriveAsScottRec :: a s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance
  forall (a :: S -> Type) (struct' :: [Type]) (struct :: [S -> Type]).
  ( SOP.Generic (a Any)
  , '[struct'] ~ Code (a Any)
  , struct ~ UnTermRec struct'
  , SListI struct
  , forall s. StructSameRepr s a '[struct]
  , RecTypePrettyError (Code (a Any))
  ) =>
  PlutusType (DeriveAsScottRec a)
  where
  type PInner (DeriveAsScottRec a) = PScottRec (UnTermRec (Head (Code (a Any))))
  pcon' (DeriveAsScottRec x) =
    pcon $ PScottRec $ PRec $ SOP.unZ $ SOP.unSOP $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch x (f . DeriveAsScottRec . SOP.to . SOP.hcoerce . SOP . (Z @_ @_ @'[]) . unPRec . unPScottRec)

-- Helpers

-- What whatever unholy reason, quantification on constrain only works if like this
class SListI (ScottList struct r) => PScottStructConstraint' struct r

instance SListI (ScottList struct r) => PScottStructConstraint' struct r

class (SListI struct, forall r. PScottStructConstraint' struct r) => PScottStructConstraint struct

instance (SListI struct, forall r. PScottStructConstraint' struct r) => PScottStructConstraint struct

--------------------------------------------------------------------------------------

type ScottFn' :: [S -> Type] -> (S -> Type) -> S -> Type
type family ScottFn' xs r where
  ScottFn' '[] r = r
  ScottFn' (x ': xs) r = x :--> ScottFn' xs r

type ScottFn :: [S -> Type] -> (S -> Type) -> S -> Type
type family ScottFn xs r where
  ScottFn '[] r = PDelayed r
  ScottFn xs r = ScottFn' xs r

-- scottList l r = map (flip scottFn r) l
type ScottList :: [[S -> Type]] -> (S -> Type) -> [S -> Type]
type family ScottList code r where
  ScottList '[] _ = '[]
  ScottList (xs ': xss) r = ScottFn xs r ': ScottList xss r

newtype PLamL' s b as = PLamL' {unPLamL' :: (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)}

-- Explicitly variadic `plam`.
plamL' :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn' as b)
plamL' =
  unPLamL' $ SOP.para_SList (PLamL' \f -> f Nil) (\(PLamL' prev) -> PLamL' \f -> plam' \a -> prev \as -> f (a :* as))

newtype PLamL s b as = PLamL {unPLamL :: (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)}

-- `pdelay`s the 0-arity case.
plamL :: SListI as => (NP (Term s) as -> Term s b) -> Term s (ScottFn as b)
plamL = unPLamL $ SOP.case_SList (PLamL \f -> pdelay $ f Nil) (PLamL plamL')

newtype PAppL' s r as = PAppL' {unPAppL' :: Term s (ScottFn' as r) -> NP (Term s) as -> Term s r}

pappL' :: SListI as => Term s (ScottFn' as c) -> NP (Term s) as -> Term s c
pappL' =
  unPAppL' $ SOP.para_SList (PAppL' \f Nil -> f) (\(PAppL' prev) -> PAppL' \f (x :* xs) -> prev (f # x) xs)

newtype PAppL s r as = PAppL {unPAppL :: Term s (ScottFn as r) -> NP (Term s) as -> Term s r}

pappL :: forall as r s. SListI as => Term s (ScottFn as r) -> NP (Term s) as -> Term s r
pappL = unPAppL $ SOP.case_SList (PAppL \f Nil -> pforce f) (PAppL pappL')

-- Note, we don't have to use delay unit here because when value is unit, that will be the only branch that will get run.
pconScottRec ::
  forall (struct :: [S -> Type]) (s :: S).
  SListI struct =>
  PRec struct s ->
  Term s (PScottRec struct)
pconScottRec (PRec xs) = punsafeCoerce $ plam $ flip pappL' xs

pmatchScottRec ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S).
  SListI struct =>
  Term s (PScottRec struct) ->
  (PRec struct s -> Term s r) ->
  Term s r
pmatchScottRec xs f = punsafeCoerce xs # plamL' (f . PRec)

---------------------------

-- PScottStruct struct <~> Term s (ScottFn (ScottList struct r)) :--> Term s r)

newtype GPCon' s r as = GPCon' {unGPCon' :: NP (Term s) (ScottList as r) -> PStruct as s -> Term s r}

gpcon' :: SListI2 as => NP (Term s) (ScottList as r) -> PStruct as s -> Term s r
gpcon' = unGPCon' $
  SOP.cpara_SList
    (Proxy @SListI)
    (GPCon' \Nil -> \case {})
    \(GPCon' prev) -> GPCon' \(arg :* args) -> \case
      (PStruct (SOP (Z x))) -> pappL arg x
      (PStruct (SOP (S xs))) -> prev args (PStruct $ SOP xs)

pconScottStruct ::
  forall (struct :: [[S -> Type]]) (r :: S -> Type) (s :: S).
  ( SListI2 struct
  , SListI (ScottList struct r)
  ) =>
  PStruct struct s ->
  Term s (ScottFn (ScottList struct r) r :--> r)
pconScottStruct (PStruct xs) =
  pletL xs \(SOP fields) ->
    punsafeCoerce $ plamL \args -> (gpcon' args (PStruct $ SOP fields) :: Term s r)

newtype GPMatch' s r as = GPMatch' {unGPMatch' :: (PStruct as s -> Term s r) -> NP (Term s) (ScottList as r)}

gpmatch' ::
  forall as r s.
  SListI2 as =>
  (PStruct as s -> Term s r) ->
  NP (Term s) (ScottList as r)
gpmatch' = unGPMatch' $ SOP.cpara_SList (Proxy @SListI) (GPMatch' (const Nil)) \(GPMatch' prev) -> GPMatch' \f ->
  plamL (\args -> f (PStruct $ SOP $ Z args)) :* prev (\(PStruct (SOP x)) -> f (PStruct $ SOP (S x)))

pmatchScottStruct ::
  forall struct r s.
  ( PScottStructConstraint struct
  , SListI2 struct
  ) =>
  Term s (PScottStruct struct) ->
  (PStruct struct s -> Term s r) ->
  Term s r
pmatchScottStruct xs f = pappL (punsafeCoerce xs) (gpmatch' f)
