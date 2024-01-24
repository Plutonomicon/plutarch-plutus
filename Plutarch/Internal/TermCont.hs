{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.Internal.TermCont (
  hashOpenTerm,
  TermCont (TermCont),
  runTermCont,
  unTermCont,
  tcont,
) where

import Data.Kind (Type)
import Data.String (fromString)
import Plutarch.Internal (
  Dig,
  PType,
  S,
  Term (Term),
  asRawTerm,
  getTerm,
  hashRawTerm,
  pgetConfig,
  tracingMode,
  pattern DetTracing,
 )
import Plutarch.Trace (ptraceError)

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
    pgetConfig \c -> case tracingMode c of
      DetTracing -> ptraceError "Pattern matching failure in TermCont"
      _ -> ptraceError $ fromString s

tcont :: ((a -> Term s r) -> Term s r) -> TermCont @r s a
tcont = TermCont

hashOpenTerm :: Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i -> do
  y <- asRawTerm x i
  asRawTerm (f . hashRawTerm . getTerm $ y) i
