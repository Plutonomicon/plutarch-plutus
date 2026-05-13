{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.Internal.TermCont (
  hashOpenTerm,
  TermCont (TermCont),
  runTermCont,
  unTermCont,
  tcont,
  pfindAllPlaceholders,
) where

import Data.Hashable (Hashed, hashed)
import Data.Kind (Type)
import Data.List (nub)
import Data.String (fromString)
import Plutarch.Internal.Term (
  Config (Tracing),
  HoistedTerm (..),
  RawTerm (..),
  S,
  Term (Term),
  TermResult (TermResult),
  TracingMode (DetTracing),
  asRawTerm,
  getTerm,
  perror,
  pgetConfig,
 )
import Plutarch.Internal.Trace (ptraceInfo)

newtype TermCont :: forall (r :: S -> Type). S -> Type -> Type where
  TermCont :: forall r s a. {runTermCont :: (a -> Term s r) -> Term s r} -> TermCont @r s a

unTermCont :: TermCont @a s (Term s a) -> Term s a
unTermCont t = runTermCont t id

instance Functor (TermCont s) where
  fmap f (TermCont g) = TermCont $ \h -> g (h . f)

instance Applicative (TermCont s) where
  pure x = TermCont $ \f -> f x
  x <*> y = do
    x <- x
    x <$> y

instance Monad (TermCont s) where
  (TermCont f) >>= g = TermCont $ \h ->
    f
      ( \x ->
          runTermCont (g x) h
      )

instance MonadFail (TermCont s) where
  fail s = TermCont $ \_ ->
    pgetConfig $ \case
      -- Note: This currently works because DetTracing is the most specific
      -- tracing mode.
      Tracing _ DetTracing -> ptraceInfo "Pattern matching failure in TermCont" perror
      _ -> ptraceInfo (fromString s) perror

tcont :: ((a -> Term s r) -> Term s r) -> TermCont @r s a
tcont = TermCont

hashOpenTerm :: Term s a -> TermCont s (Hashed RawTerm)
hashOpenTerm x = TermCont $ \f -> Term $ do
  TermResult t _ <- asRawTerm x
  let h = hashed t
  asRawTerm (f h)

-- This can technically be done outside of TermCont.
-- Need to pay close attention when killing branch with this.
-- If term is pre-evaluated (via `evalTerm`), RawTerm will no longer hold
-- tagged RPlaceholder.

-- | Finds all placeholder ids and returns it
pfindAllPlaceholders :: Term s a -> TermCont s [Integer]
pfindAllPlaceholders x = TermCont $ \f -> Term $ do
  y@(TermResult yRaw _) <- asRawTerm x
  asRawTerm (f . nub . findPlaceholder yRaw . getTerm $ y)
  where
    findPlaceholder :: RawTerm -> RawTerm -> [Integer]
    findPlaceholder t = \case
      RLamAbs _ x -> findPlaceholder t x
      RApply x xs -> findPlaceholder t x <> foldMap (findPlaceholder t) xs
      RForce x -> findPlaceholder t x
      RDelay x -> findPlaceholder t x
      RHoisted (HoistedTerm _ x) -> findPlaceholder t x
      RPlaceHolder idx -> [idx]
      RConstr _ xs -> foldMap (findPlaceholder t) xs
      RCase x xs -> findPlaceholder t x <> foldMap (findPlaceholder t) xs
      RVar _ -> []
      RConstant _ -> []
      RBuiltin _ -> []
      RCompiled _ -> []
      RError -> []
      RLet v f -> findPlaceholder t v <> findPlaceholder t f
      RFix x -> findPlaceholder t x
