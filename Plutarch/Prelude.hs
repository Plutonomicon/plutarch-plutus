module Plutarch.Prelude (
  (:-->),
  PDelayed,
  Term,
  plam,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  (#$),
  (#),
  plet,
  pinl,
  pcon,
  pmatch,
  pto,
  pfix,
  Type,
  S,
  PType,
) where

import Prelude ()

import Data.Kind (Type)
import Plutarch
