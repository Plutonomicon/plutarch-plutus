module Plutarch.VerifySpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Unsafe (punsafeBuiltin)
import Plutarch.Verify (
  PTryFrom (ptryFrom),
  PTryFromRecur (ptryFromRecur),
 )
import qualified PlutusCore as PLC

spec :: Spec
spec = do
  describe "verification_untrusted_data" . pgoldenSpec $ do
    "erroneous" @\ do
      "int /= bytestring"
        @| checkShallow
          @PInteger
          @PByteString
          (pconstant "foo")
        @-> pfails
      "(String, Integer) /= (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> pfails
      "[String] /= [Integer]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PInteger))
          ((pcons # (pdata $ pconstant 3)) #$ (psingleton # (pdata $ pconstant 4)))
        @-> pfails
    "working" @\ do
      "int == int"
        @| checkShallow @PInteger @PInteger (pconstant 42)
        @-> psucceeds
      "(String, String) == (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> psucceeds
      "(POpaque, POpaque) == (POpaque, POpaque)"
        @| checkShallow
          @(PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          @(PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          ( punsafeBuiltin PLC.MkPairData
              # (pdata $ pcon PUnit)
              # (pdata $ pcon PUnit)
          )
        @-> psucceeds
      "[String] == [String]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PByteString))
          ((pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "bar")))
        @-> psucceeds

checkShallow ::
  forall (target :: PType) (actual :: PType).
  (PTryFrom target, PIsData actual) =>
  ClosedTerm actual ->
  ClosedTerm target
checkShallow = reprTargetActual ptryFrom

checkDeep ::
  forall (target :: PType) (actual :: PType).
  (PTryFromRecur target, PIsData actual) =>
  ClosedTerm actual ->
  ClosedTerm target
checkDeep = reprTargetActual ptryFromRecur

reprTargetActual ::
  forall (target :: PType) (actual :: PType).
  (PIsData actual) =>
  ClosedTerm (PData :--> target) ->
  ClosedTerm actual ->
  ClosedTerm target
reprTargetActual f x = f #$ pforgetData $ pdata x
