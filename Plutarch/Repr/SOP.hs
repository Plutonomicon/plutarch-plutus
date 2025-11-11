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
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import Generics.SOP (
  Code,
  K (K),
  NP (Nil, (:*)),
  NS (S, Z),
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
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.Lift
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PInner,
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
  perror,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Repr.Internal (
  PRec (PRec, unPRec),
  PStruct (PStruct, unPStruct),
  RecAsHaskell,
  RecTypePrettyError,
  StructAsHaskell,
  StructSameRepr,
  UnTermRec,
  UnTermStruct,
  grecEq,
  gstructEq,
  pletL,
 )

-- | @since 1.10.0
newtype PSOPStruct (struct :: [[S -> Type]]) (s :: S) = PSOPStruct
  { unPSOPStruct :: PStruct struct s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance (SListI2 struct, PSOPStructConstraint struct) => PlutusType (PSOPStruct struct) where
  type PInner (PSOPStruct struct) = POpaque
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

-- | @since 1.10.0
newtype PSOPRec (struct :: [S -> Type]) (s :: S) = PSOPRec
  { unPSOPRec :: PRec struct s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
instance SListI struct => PlutusType (PSOPRec struct) where
  type PInner (PSOPRec struct) = POpaque
  pcon' (PSOPRec x) = punsafeCoerce $ pconSOPRec x
  pmatch' x f = pmatchSOPRec (punsafeCoerce x) (f . PSOPRec)

-- | @since 1.10.0
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

{- |
@via@-derivation helper to derive 'PlutusType' instance using SoP encoding. If your type has
only one constructor prefer using 'DeriveAsSOPRec' instead.

@since 1.10.0
-}
newtype DeriveAsSOPStruct (a :: S -> Type) s = DeriveAsSOPStruct
  { unDeriveAsSOPStruct :: a s
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
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
  pcon' (DeriveAsSOPStruct x) =
    pcon @(PSOPStruct (UnTermStruct (a Any))) $ PSOPStruct $ PStruct $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch @(PSOPStruct (UnTermStruct (a Any))) x (f . DeriveAsSOPStruct . SOP.to . SOP.hcoerce . unPStruct . unPSOPStruct)

{- | @via@-derivation helper for SOP encoding, currently behaves exactly like `DeriveAsSOPStruct`
but can be used only on types with a single constructor. It is separate to leave a room for
future optimizations.

@since 1.10.0
-}
newtype DeriveAsSOPRec (a :: S -> Type) s = DeriveAsSOPRec
  { unDeriveAsSOPRec :: a s
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
  PlutusType (DeriveAsSOPRec a)
  where
  type PInner (DeriveAsSOPRec a) = PSOPRec (UnTermRec (Head (Code (a Any))))
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

  pure $ TermResult (RCase term handlerTerms) (handlerDeps <> deps)

--------------------------------------------------------------------------------

class (a ~ AsHaskell b, PLiftable b) => ToAsHaskell (a :: Type) (b :: S -> Type)
instance (a ~ AsHaskell b, PLiftable b) => ToAsHaskell (a :: Type) (b :: S -> Type)

newtype PSOPRecPLiftableHelper (struct :: [S -> Type]) = PSOPRecPLiftableHelper
  { unPSOPRecPLiftableHelper ::
      PLiftedClosed (PSOPRec struct) ->
      Either LiftError (SOP.NP SOP.I (RecAsHaskell struct))
  }

-- | @since WIP
instance
  forall (struct :: [S -> Type]) (hstruct :: [Type]).
  ( SListI struct
  , hstruct ~ RecAsHaskell struct
  , SOP.AllZip ToAsHaskell hstruct struct
  , SOP.All PLiftable struct
  ) =>
  PLiftable (PSOPRec struct)
  where
  type AsHaskell (PSOPRec struct) = SOP SOP.I '[RecAsHaskell struct]
  type PlutusRepr (PSOPRec struct) = PLiftedClosed (PSOPRec struct)
  haskToRepr x =
    let
      f :: forall a b s. ToAsHaskell a b => SOP.I a -> Term s b
      f = pconstant @b . SOP.unI
     in
      mkPLiftedClosed $
        pcon $
          PSOPRec $
            PRec $
              SOP.unZ $
                SOP.unSOP $
                  SOP.htrans (Proxy @ToAsHaskell) f x
  reprToHask x =
    let
      go :: forall y ys. (SOP.SListI ys, PLiftable y) => PSOPRecPLiftableHelper ys -> PSOPRecPLiftableHelper (y ': ys)
      go (PSOPRecPLiftableHelper rest) = PSOPRecPLiftableHelper $ \x -> do
        rest' <-
          rest $
            mkPLiftedClosed $
              pmatch (getPLiftedClosed x) $ \case
                (PSOPRec (PRec (_ :* ds))) -> pcon $ PSOPRec $ PRec ds
        curr <-
          ( plutToRepr @y $
              mkPLifted $
                pmatch (getPLiftedClosed x) $ \case
                  (PSOPRec (PRec (d :* _))) -> d
          )
            >>= reprToHask @y

        pure $ SOP.I curr :* rest'
     in
      SOP.SOP . SOP.Z
        <$> (unPSOPRecPLiftableHelper $ SOP.cpara_SList (Proxy @PLiftable) (PSOPRecPLiftableHelper $ const $ Right Nil) go) x
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = pliftedFromClosed
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = Right . pliftedToClosed

newtype PSOPStructPLiftableHelper struct = PSOPStructPLiftableHelper
  { unPSOPStructPLiftableHelper ::
      PLiftedClosed (PSOPStruct struct) ->
      Either LiftError (SOP SOP.I (StructAsHaskell struct))
  }

class (SOP.AllZip ToAsHaskell (RecAsHaskell y) y, All PLiftable y) => SOPEntryConstraints y
instance (SOP.AllZip ToAsHaskell (RecAsHaskell y) y, All PLiftable y) => SOPEntryConstraints y

class (SListI2 ys, PSOPStructConstraint ys) => SOPRestConstraint ys
instance (SListI2 ys, PSOPStructConstraint ys) => SOPRestConstraint ys

-- | @since WIP
instance
  forall (struct :: [[S -> Type]]) (hstruct :: [[Type]]).
  ( SListI2 struct
  , hstruct ~ StructAsHaskell struct
  , SOP.AllZip2 ToAsHaskell hstruct struct
  , SOP.All2 PLiftable struct
  , MyAll SOPEntryConstraints SOPRestConstraint struct
  , PSOPStructConstraint struct
  ) =>
  PLiftable (PSOPStruct struct)
  where
  type AsHaskell (PSOPStruct struct) = SOP SOP.I (StructAsHaskell struct)
  type PlutusRepr (PSOPStruct struct) = PLiftedClosed (PSOPStruct struct)
  haskToRepr :: SOP SOP.I hstruct -> PLiftedClosed (PSOPStruct struct)
  haskToRepr x =
    let
      f :: forall a b s. ToAsHaskell a b => SOP.I a -> Term s b
      f = pconstant @b . SOP.unI
     in
      mkPLiftedClosed $ pcon $ PSOPStruct $ PStruct $ SOP.htrans (Proxy @ToAsHaskell) f x
  reprToHask :: PLiftedClosed (PSOPStruct struct) -> Either LiftError (SOP SOP.I hstruct)
  reprToHask x =
    let
      go ::
        forall (y :: [S -> Type]) (ys :: [[S -> Type]]).
        (SOPRestConstraint ys, SOPEntryConstraints y) =>
        PSOPStructPLiftableHelper ys ->
        PSOPStructPLiftableHelper (y ': ys)
      go (PSOPStructPLiftableHelper rest) = PSOPStructPLiftableHelper $ \d -> do
        let
          isCurrent :: Bool
          isCurrent =
            plift $
              pmatch (getPLiftedClosed d) $ \case
                (PSOPStruct (PStruct (SOP (S _)))) -> pconstant @PBool False
                (PSOPStruct (PStruct (SOP (Z _)))) -> pconstant @PBool True

        if isCurrent
          then do
            curr <-
              ( plutToRepr @(PSOPRec y) $
                  mkPLifted $
                    pmatch (getPLiftedClosed d) $ \case
                      (PSOPStruct (PStruct (SOP (S _x')))) -> perror
                      (PSOPStruct (PStruct (SOP (Z x')))) -> pcon $ PSOPRec $ PRec x'
              )
                >>= reprToHask @(PSOPRec y)

            pure $ case curr of
              (SOP (Z (curr' :: NP SOP.I (RecAsHaskell y)))) -> SOP $ Z curr'
              _ -> error "no B"
          else do
            SOP next <-
              rest $
                mkPLiftedClosed $
                  pmatch (getPLiftedClosed d) $ \case
                    (PSOPStruct (PStruct (SOP (S x')))) -> pcon $ PSOPStruct $ PStruct $ SOP x'
                    (PSOPStruct (PStruct (SOP (Z _x')))) -> perror

            pure $ SOP $ S next
     in
      ( unPSOPStructPLiftableHelper $
          my_cpara_SList
            (Proxy @SOPEntryConstraints)
            (Proxy @SOPRestConstraint)
            (PSOPStructPLiftableHelper $ const $ pure $ SOP $ error "absurd: empty SOP")
            go
      )
        x
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = pliftedFromClosed
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = Right . pliftedToClosed

-- "my" copy of `cpara_SList` that allows you to add constraint on the "rest" part of the type
type family MyAllF (c :: k -> Constraint) (d :: [k] -> Constraint) (xs :: [k]) :: Constraint where
  MyAllF _c _d '[] = ()
  MyAllF c d (x ': xs) = (c x, d xs, MyAll c d xs)

class (MyAllF c d xs, SListI xs) => MyAll (c :: k -> Constraint) (d :: [k] -> Constraint) (xs :: [k]) where
  my_cpara_SList ::
    Proxy c ->
    Proxy d ->
    r '[] ->
    (forall y ys. (c y, d ys, MyAll c d ys) => r ys -> r (y ': ys)) ->
    r xs

instance MyAll c d '[] where
  my_cpara_SList _p _d nil _cons = nil
  {-# INLINE my_cpara_SList #-}

instance (c x, d xs, MyAll c d xs) => MyAll c d (x ': xs) where
  my_cpara_SList p d nil cons =
    cons (my_cpara_SList p d nil cons)
  {-# INLINE my_cpara_SList #-}
