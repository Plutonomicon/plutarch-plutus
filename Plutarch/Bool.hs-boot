module Plutarch.Bool (PBool (..), pif) where

import Plutarch.Internal (Term, S)

data PBool (s :: S) = PTrue | PFalse

pif :: Term s PBool -> Term s a -> Term s a -> Term s a
