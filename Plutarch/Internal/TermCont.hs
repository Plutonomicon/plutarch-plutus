{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.Internal.TermCont (
  hashOpenTerm,
  TermCont (TermCont),
  runTermCont,
  unTermCont,
  tcont,
  pfindPlaceholder,
  pfindAllPlaceholders,
) where

import Data.Kind (Type)
import Data.List (nub)
import Data.String (fromString)
import Plutarch.Internal.Term (
  Config (Tracing),
  Dig,
  HoistedTerm (..),
  PType,
  RawTerm (..),
  S,
  Term (Term),
  TracingMode (DetTracing),
  asRawTerm,
  getTerm,
  hashRawTerm,
  perror,
  pgetConfig,
 )
import Plutarch.Internal.Trace (ptraceInfo)

newtype TermCont :: forall (r :: PType). S -> Type -> Type where
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

hashOpenTerm :: Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i -> do
  y <- asRawTerm x i
  asRawTerm (f . hashRawTerm . getTerm $ y) i

-- This can technically be done outside of TermCont.
-- Need to pay close attention when killing branch with this.
-- If term is pre-evaluated (via `evalTerm`), RawTerm will no longer hold
-- tagged RPlaceholder.

{- | Given a term, and an integer tag, this function checks if the term holds and
@PPlaceholder@ with the given integer tag.
-}
pfindPlaceholder :: Integer -> Term s a -> TermCont s Bool
pfindPlaceholder idx x = TermCont $ \f -> Term $ \i -> do
  y <- asRawTerm x i

  let
    findPlaceholder (RLamAbs _ x) = findPlaceholder x
    findPlaceholder (RApply x xs) = any findPlaceholder (x : xs)
    findPlaceholder (RForce x) = findPlaceholder x
    findPlaceholder (RDelay x) = findPlaceholder x
    findPlaceholder (RHoisted (HoistedTerm _ x)) = findPlaceholder x
    findPlaceholder (RPlaceHolder idx') = idx == idx'
    findPlaceholder (RConstr _ xs) = any findPlaceholder xs
    findPlaceholder (RCase x xs) = any findPlaceholder (x : xs)
    findPlaceholder (RVar _) = False
    findPlaceholder (RConstant _) = False
    findPlaceholder (RBuiltin _) = False
    findPlaceholder (RCompiled _) = False
    findPlaceholder RError = False

  asRawTerm (f . findPlaceholder . getTerm $ y) i

-- | Finds all placeholder ids and returns it
pfindAllPlaceholders :: Term s a -> TermCont s [Integer]
pfindAllPlaceholders x = TermCont $ \f -> Term $ \i -> do
  y <- asRawTerm x i

  let
    findPlaceholder (RLamAbs _ x) = findPlaceholder x
    findPlaceholder (RApply x xs) = findPlaceholder x <> foldMap findPlaceholder xs
    findPlaceholder (RForce x) = findPlaceholder x
    findPlaceholder (RDelay x) = findPlaceholder x
    findPlaceholder (RHoisted (HoistedTerm _ x)) = findPlaceholder x
    findPlaceholder (RPlaceHolder idx) = [idx]
    findPlaceholder (RConstr _ xs) = foldMap findPlaceholder xs
    findPlaceholder (RCase x xs) = findPlaceholder x <> foldMap findPlaceholder xs
    findPlaceholder (RVar _) = []
    findPlaceholder (RConstant _) = []
    findPlaceholder (RBuiltin _) = []
    findPlaceholder (RCompiled _) = []
    findPlaceholder RError = []

  asRawTerm (f . nub . findPlaceholder . getTerm $ y) i
