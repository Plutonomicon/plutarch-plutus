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
  , pUnsafeCoerce
  , pUnsafeBuiltin
  , pUnsafeConstant
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
  , pUnsafeFrom
  , pTo
  , Type
) where

import Prelude ()

import Plutarch
import Data.Kind (Type)
