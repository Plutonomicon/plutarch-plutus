{-# LANGUAGE TemplateHaskell #-}

module TH.Enum (PColour (..)) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (pcoerce, (#))
import Plutarch.TH.Strategy (deriveFor)
import Plutarch.TH.Strategy qualified as Strategy

data PColour (s :: S)
  = PRed
  | PGreen
  | PBlue

deriveFor ''PColour Strategy.Enum
