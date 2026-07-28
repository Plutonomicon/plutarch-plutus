{-# LANGUAGE TemplateHaskell #-}

module TH.SOP (PThese (..), PEither (..)) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (pand, pfalse)
import Plutarch.Primitive.Match (pmatch)
import Plutarch.TH.Strategy (Strategy (SOP), deriveFor)

data PThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThis (Term s a)
  | PThat (Term s b)
  | PThese (Term s a) (Term s b)

deriveFor ''PThese SOP

data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)

deriveFor ''PEither SOP
