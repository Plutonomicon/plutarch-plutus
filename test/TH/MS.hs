{-# LANGUAGE TemplateHaskell #-}

module TH.MS (PTheseMS (..)) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (pand, pfalse)
import Plutarch.TH.Strategy (Strategy (MogensenScott), deriveFor)

data PTheseMS (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisMS (Term s a)
  | PThatMS (Term s b)
  | PTheseMS (Term s a) (Term s b)

deriveFor ''PTheseMS MogensenScott
