{-# LANGUAGE TemplateHaskell #-}

module TH.Data (PTheseData (..)) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Data (PAsData)
import Plutarch.TH.Strategy (Strategy (DataPlutus), deriveFor)

data PTheseData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisData (Term s (PAsData a))
  | PThatData (Term s (PAsData b))
  | PTheseData (Term s (PAsData a)) (Term s (PAsData b))

deriveFor ''PTheseData DataPlutus
