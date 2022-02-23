module Plutarch.UPLCSpec (spec) where

import qualified PlutusCore as PLC
import Test.Syd

import Plutarch (POpaque)
import Plutarch.Internal (punsafeConstantInternal)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Unsafe (punsafeBuiltin)

spec :: Spec
spec = do
  -- Tests for the behaviour of UPLC itself.
  describe "uplc-behaviour" . pgoldenSpec $ do
    "2:[1]"
      @| ( let l :: Term _ (PBuiltinList PInteger) =
                punsafeConstantInternal . PLC.Some $
                  PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            in pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         )
    "fails:True:[1]"
      @| ( let l :: Term _ (PBuiltinList POpaque) =
                punsafeConstantInternal . PLC.Some $
                  PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            in pforce (punsafeBuiltin PLC.MkCons) # pcon PTrue # l @-> pfails
         )
    "(2,1)"
      @| punsafeConstantInternal . PLC.Some
      $ PLC.ValueOf
        ( PLC.DefaultUniApply
            (PLC.DefaultUniApply PLC.DefaultUniProtoPair PLC.DefaultUniInteger)
            PLC.DefaultUniInteger
        )
        (1, 2)
    "fails:MkPair-1-2"
      @| punsafeBuiltin PLC.MkPairData # (1 :: Term _ PInteger) # (2 :: Term _ PInteger)
        @-> pfails
