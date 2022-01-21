module Plutarch.Prelude (
  (:-->),
  PDelayed,
  Term,
  plam,
  plam',
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
  S (SI),
  PType,
  DerivePNewtype (DerivePNewtype),
) where

import Prelude ()

import Data.Kind (Type)
import Plutarch
