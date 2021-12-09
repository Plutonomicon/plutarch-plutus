module Plutarch.Prelude
  ( PInteger
  , PBool
  , (:-->)
  , PDelayed
  , POpaque
  , PData
  , Term
  , pLam
  , pApp
  , pDelay
  , pForce
  , pHoist
  , pError
  , pCoerce
  , pBuiltin
  , pConstant
  , PEq(..)
  , POrd(..)
  , printTerm
  , (£$)
  , (£)
  , pLam2
  , pLam3
  , pLam4
  , pLam5
  , pLet
  , pInl
) where

import Prelude ()

import Plutarch
import Plutarch.Internal.Core
