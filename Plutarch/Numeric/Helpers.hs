module Plutarch.Numeric.Helpers (
  pexpBySquaring,
  pizero,
  pione,
) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pfix,
  plam',
  punsafeCase,
  punsafeConstant,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (PlutarchType, (#), (#$))
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (
  pequalsInteger,
  pquotientInteger,
  premainderInteger,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import PlutusCore qualified as PLC

-- | @since wip
pexpBySquaring ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s (a :--> a :--> a) ->
  Term s (a :--> PInteger :--> a)
pexpBySquaring f = plam' $ \x -> pfix $ \self -> plam' $ \i ->
  -- We know that `i` cannot be non-positive here.
  pif
    (pequalsInteger # i # pione)
    x
    ( let two = punsafeConstant (PLC.someValue @Integer 2)
          stepDown = self #$ pquotientInteger # i # two
          squared = f # stepDown # stepDown
       in -- Because the remainder by two can only be 0 or 1, we can use
          -- `punsafeCase` here for speed.
          punsafeCase (premainderInteger # i # two)
            . NEVector.cons (toSomeTerm squared)
            . NEVector.singleton
            $ toSomeTerm (f # squared # x)
    )

-- | @since wip
pizero :: forall (s :: S). Term s PInteger
pizero = punsafeConstant (PLC.someValue @Integer 0)

-- | @since wip
pione :: forall (s :: S). Term s PInteger
pione = punsafeConstant (PLC.someValue @Integer 1)
