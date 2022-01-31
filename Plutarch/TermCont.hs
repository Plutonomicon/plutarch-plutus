module Plutarch.TermCont (
  hashOpenTerm,
  TermCont (TermCont, runTermCont),
  runTermContId,
  tcont,
) where

import Data.String (fromString)
import Plutarch.Internal (Dig, Term (Term), asRawTerm, getTerm, hashRawTerm)
import Plutarch.Trace (ptraceError)

newtype TermCont r s a = TermCont {runTermCont :: (a -> Term s r) -> Term s r}

instance Functor (TermCont r s) where
  fmap f (TermCont g) = TermCont $ \h -> g (h . f)

instance Applicative (TermCont r s) where
  pure x = TermCont $ \f -> f x
  x <*> y = do
    x <- x
    y <- y
    pure (x y)

instance Monad (TermCont r s) where
  (TermCont f) >>= g = TermCont $ \h ->
    f
      ( \x ->
          runTermCont (g x) h
      )

instance MonadFail (TermCont r s) where
  fail s = TermCont $ \_ ->
    ptraceError $ fromString s

runTermContId :: TermCont a s (Term s a) -> Term s a
runTermContId x = runTermCont x id

tcont :: ((a -> Term s r) -> Term s r) -> TermCont r s a
tcont = TermCont

hashOpenTerm :: Term s a -> TermCont r s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i ->
  let inner = f $ hashRawTerm . getTerm $ asRawTerm x i
   in asRawTerm inner i
