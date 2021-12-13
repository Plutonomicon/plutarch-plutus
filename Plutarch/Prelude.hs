module Plutarch.Prelude
  ( (:-->)
  , PDelayed
  , Term
  , plam
  , papp
  , pdelay
  , pforce
  , phoistAcyclic
  , perror
  , (£$)
  , (£)
  , plet
  , pinl
  , pcon
  , pmatch
  , pto
  , pfix
  , Type
) where

import Prelude ()

import Plutarch
import Data.Kind (Type)
