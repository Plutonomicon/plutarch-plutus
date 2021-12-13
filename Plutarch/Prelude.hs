module Plutarch.Prelude
  ( (:-->)
  , PDelayed
  , Term
  , pLam
  , pApp
  , pDelay
  , pForce
  , pHoistAcyclic
  , pError
  , (£$)
  , (£)
  , pLam2
  , pLam3
  , pLam4
  , pLam5
  , pLet
  , pInl
  , pCon
  , pMatch
  , pTo
  , pFix
  , Type
) where

import Prelude ()

import Plutarch
import Data.Kind (Type)
