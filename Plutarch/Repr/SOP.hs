{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.SOP (
  PSOPStruct (PSOPStruct, unPSOPStruct),
  PSOPRec (PSOPRec, unPSOPRec),
  DeriveAsSOPStruct (DeriveAsSOPStruct, unDeriveAsSOPStruct),
  DeriveAsSOPRec (DeriveAsSOPRec, unDeriveAsSOPRec),
) where

import Control.Arrow ((&&&))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import Generics.SOP (
  Code,
  K (K),
  NP (Nil, (:*)),
  NS (Z),
  SOP (SOP),
 )
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint (
  All,
  All2,
  Head,
  SListI,
  SListI2,
 )
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PContravariant',
  PContravariant'',
  PCovariant',
  PCovariant'',
  PInner,
  PVariant',
  PVariant'',
  PlutusType,
  pcon,
  pcon',
  pmatch,
  pmatch',
 )
import Plutarch.Internal.Term (
  RawTerm (RCase, RConstr),
  S,
  Term (Term),
  TermResult (TermResult),
  asRawTerm,
  getDeps,
  getTerm,
  phoistAcyclic,
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

-- | @since WIP
newtype PSOPStruct (struct :: [[S -> Type]]) (s :: S) = PSOPStruct
  { unPSOPStruct :: PStruct struct s
  -- ^ @since WIP
  }

-- | @since WIP
instance (SListI2 struct, PSOPStructConstraint struct) => PlutusType (PSOPStruct struct) where
  type PInner (PSOPStruct struct) = POpaque
  type PCovariant' (PSOPStruct struct) = All2 PCovariant'' struct
  type PContravariant' (PSOPStruct struct) = All2 PContravariant'' struct
  type PVariant' (PSOPStruct struct) = All2 PVariant'' struct
  pcon' (PSOPStruct x) = punsafeCoerce $ pconSOPStruct x
  pmatch' x f = pmatchSOPStruct (punsafeCoerce x) (f . PSOPStruct)

-- NOTE/TODO: Performance has not been tested for this. I feel like this would be terribly slow
instance (PlutusType (PSOPStruct struct), All2 PEq struct) => PEq (PSOPStruct struct) where
  x #== y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pmatch x' $ \(PSOPStruct (PStruct x'')) ->
            pmatch y' $ \(PSOPStruct (PStruct y'')) ->
              gstructEq x'' y''
      )
      # x
      # y

-- | @since WIP
newtype PSOPRec (struct :: [S -> Type]) (s :: S) = PSOPRec
  { unPSOPRec :: PRec struct s
  -- ^ @since WIP
  }

-- | @since WIP
instance SListI struct => PlutusType (PSOPRec struct) where
  type PInner (PSOPRec struct) = POpaque
  type PCovariant' (PSOPRec struct) = All PCovariant'' struct
  type PContravariant' (PSOPRec struct) = All PContravariant'' struct
  type PVariant' (PSOPRec struct) = All PVariant'' struct
  pcon' (PSOPRec x) = punsafeCoerce $ pconSOPRec x
  pmatch' x f = pmatchSOPRec (punsafeCoerce x) (f . PSOPRec)

-- | @since WIP
instance All PEq struct => PEq (PSOPRec struct) where
  x #== y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pmatch x' $ \(PSOPRec (PRec x'')) ->
            pmatch y' $ \(PSOPRec (PRec y'')) ->
              grecEq x'' y''
      )
      # x
      # y

-- | @since WIP
newtype DeriveAsSOPStruct (a :: S -> Type) s = DeriveAsSOPStruct
  { unDeriveAsSOPStruct :: a s
  -- ^ @since WIP
  }

-- | @since WIP
instance
  forall (a :: S -> Type) (struct :: [[S -> Type]]).
  ( SOP.Generic (a Any)
  , struct ~ UnTermStruct (a Any)
  , SListI2 struct
  , forall s. StructSameRepr s a struct
  , PSOPStructConstraint struct
  ) =>
  PlutusType (DeriveAsSOPStruct a)
  where
  type PInner (DeriveAsSOPStruct a) = PSOPStruct (UnTermStruct (a Any))
  type PCovariant' (DeriveAsSOPStruct a) = PCovariant' a
  type PContravariant' (DeriveAsSOPStruct a) = PContravariant' a
  type PVariant' (DeriveAsSOPStruct a) = PVariant' a
  pcon' (DeriveAsSOPStruct x) =
    pcon @(PSOPStruct (UnTermStruct (a Any))) $ PSOPStruct $ PStruct $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch @(PSOPStruct (UnTermStruct (a Any))) x (f . DeriveAsSOPStruct . SOP.to . SOP.hcoerce . unPStruct . unPSOPStruct)

-- | @since WIP
newtype DeriveAsSOPRec (a :: S -> Type) s = DeriveAsSOPRec
  { unDeriveAsSOPRec :: a s
  -- ^ @since WIP
  }

-- | @since WIP
instance
  forall (a :: S -> Type) (struct' :: [Type]) (struct :: [S -> Type]).
  ( SOP.Generic (a Any)
  , '[struct'] ~ Code (a Any)
  , struct ~ UnTermRec struct'
  , SListI struct
  , forall s. StructSameRepr s a '[struct]
  , RecTypePrettyError (Code (a Any))
  ) =>
  PlutusType (DeriveAsSOPRec a)
  where
  type PInner (DeriveAsSOPRec a) = PSOPRec (UnTermRec (Head (Code (a Any))))
  type PCovariant' (DeriveAsSOPRec a) = PCovariant' a
  type PContravariant' (DeriveAsSOPRec a) = PContravariant' a
  type PVariant' (DeriveAsSOPRec a) = PVariant' a
  pcon' (DeriveAsSOPRec x) =
    pcon $ PSOPRec $ PRec $ SOP.unZ $ SOP.unSOP $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch x (f . DeriveAsSOPRec . SOP.to . SOP.hcoerce . SOP . (Z @_ @_ @'[]) . unPRec . unPSOPRec)

-- Helpers

class SListI (PCaseTy r struct) => PSOPStructConstraint' struct r

instance SListI (PCaseTy r struct) => PSOPStructConstraint' struct r

class (SListI struct, forall r. PSOPStructConstraint' struct r) => PSOPStructConstraint struct

instance (SListI struct, forall r. PSOPStructConstraint' struct r) => PSOPStructConstraint struct

-- Take struct first for consistency
type family PHandlerTy r (struct :: [S -> Type]) :: S -> Type where
  PHandlerTy r '[] = r
  PHandlerTy r (x ': xs) = x :--> PHandlerTy r xs

type family PCaseTy r (struct :: [[S -> Type]]) :: [S -> Type] where
  PCaseTy _ '[] = '[]
  PCaseTy r (x ': xs) = PHandlerTy r x ': PCaseTy r xs

pconSOPRec ::
  forall (struct :: [S -> Type]) (s :: S). SListI struct => PRec struct s -> Term s (PSOPRec struct)
pconSOPRec (PRec xs) = Term $ \i -> do
  ts <- SOP.hcollapse <$> SOP.htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) xs
  let
    term = RConstr 0 $ fst <$> ts
    deps = mconcat $ snd <$> ts
  pure $ TermResult term deps

-- TODO: better name
newtype MSR s r struct = MSR
  { unMSR :: (PRec struct s -> Term s r) -> Term s (PHandlerTy r struct)
  }

sopHandler ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S). SListI struct => (PRec struct s -> Term s r) -> Term s (PHandlerTy r struct)
sopHandler f =
  let
    go :: MSR s r ys -> MSR s r (y ': ys)
    go (MSR rest) = MSR $ \f ->
      plam $ \x -> rest $ \(PRec rest') -> f $ PRec (x :* rest')

    handler :: Term s (PHandlerTy r struct)
    handler = unMSR (SOP.para_SList (MSR $ \f -> f $ PRec Nil) go) f
   in
    handler

pmatchSOPRec ::
  forall (struct :: [S -> Type]) (r :: S -> Type) (s :: S). SListI struct => Term s (PSOPRec struct) -> (PRec struct s -> Term s r) -> Term s r
pmatchSOPRec xs f = Term $ \i -> do
  (term, deps) <- (getTerm &&& getDeps) <$> asRawTerm xs i
  (handlerTerm, handlerDeps) <- (getTerm &&& getDeps) <$> asRawTerm (sopHandler f) i
  pure $ TermResult (RCase term (pure handlerTerm)) (deps <> handlerDeps)

pconSOPStruct ::
  forall (struct :: [[S -> Type]]) (s :: S). SListI2 struct => PStruct struct s -> Term s (PSOPStruct struct)
pconSOPStruct (PStruct xs') = pletL xs' $ \xs -> Term $ \i -> do
  ts <- SOP.hcollapse <$> SOP.htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) xs
  let
    idx = SOP.hindex xs
    term = RConstr (fromIntegral idx) $ fst <$> ts
    deps = mconcat $ snd <$> ts
  pure $ TermResult term deps

newtype MSS s r struct = MSS
  { unMSS :: (PStruct struct s -> Term s r) -> NP (Term s) (PCaseTy r struct)
  }

pmatchSOPStruct ::
  forall (struct :: [[S -> Type]]) (r :: S -> Type) (s :: S).
  (SListI2 struct, PSOPStructConstraint struct) =>
  Term s (PSOPStruct struct) ->
  (PStruct struct s -> Term s r) ->
  Term s r
pmatchSOPStruct xs h = Term $ \i -> do
  (term, deps) <- (getTerm &&& getDeps) <$> asRawTerm xs i

  let
    go :: forall y ys r s. SListI y => MSS s r ys -> MSS s r (y ': ys)
    go (MSS rest) = MSS $ \f ->
      let
        handler = sopHandler (\(PRec x) -> f $ PStruct $ SOP $ Z x)
        f' (PStruct (SOP x)) = f $ PStruct $ SOP $ SOP.S x
       in
        handler :* rest f'

    handlers' :: NP (Term s) (PCaseTy r struct)
    handlers' = unMSS (SOP.cpara_SList (Proxy @SListI) (MSS $ const Nil) go) h

  handlers <- SOP.hcollapse <$> SOP.htraverse' (\x -> K . (getTerm &&& getDeps) <$> asRawTerm x i) handlers'
  let
    handlerTerms = fst <$> handlers
    handlerDeps = mconcat $ snd <$> handlers

  pure $ TermResult (RCase term handlerTerms) (deps <> handlerDeps)
