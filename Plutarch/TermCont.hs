module Plutarch.TermCont (
  hashOpenTerm,
  TermCont (TermCont, runTermCont),
) where

import Data.String (fromString)
import Plutarch.Internal (Dig, Term (Term), asRawTerm, getTerm, hashRawTerm)
import Plutarch.Trace (ptraceError)

newtype TermCont s a = TermCont {runTermCont :: forall b. (a -> Term s b) -> Term s b}

instance Functor (TermCont s) where
  fmap f (TermCont g) = TermCont $ \h -> g (h . f)

instance Applicative (TermCont s) where
  pure x = TermCont $ \f -> f x
  x <*> y = do
    x <- x
    y <- y
    pure (x y)

instance Monad (TermCont s) where
  (TermCont f) >>= g = TermCont $ \h ->
    f
      ( \x ->
          runTermCont (g x) h
      )

instance MonadFail (TermCont s) where
  fail s = TermCont $ \_ ->
    ptraceError $ fromString s

hashOpenTerm :: Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i ->
  let inner = f $ hashRawTerm . getTerm $ asRawTerm x i
   in asRawTerm inner i
