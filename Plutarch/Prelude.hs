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
  , printTerm
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
) where

import Prelude ()

import Plutarch
