{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Parser where

import Plutarch.Builtin.ByteString
import Plutarch.Builtin.Data
import Plutarch.Builtin.Integer

import Plutarch.Internal.IsData
import Plutarch.Internal.ListLike
import Plutarch.Internal.Term

class PParser a b where
  pparse :: Term s a -> Maybe (Term s b) -> Term s b

instance PParser PData (PAsData PInteger) where
  pparse x Nothing = plet (pasInt # x) $ const $ punsafeCoerce x
  pparse x (Just failure) =
    pforce $
      pchooseData
        # x
        # pdelay failure
        # pdelay failure
        # pdelay failure
        # pdelay (punsafeCoerce x)
        # pdelay failure

instance PParser PData (PAsData PByteString) where
  pparse x Nothing = plet (pasByteStr # x) $ const $ punsafeCoerce x
  pparse x (Just failure) =
    pforce $
      pchooseData
        # x
        # pdelay failure
        # pdelay failure
        # pdelay failure
        # pdelay failure
        # pdelay (punsafeCoerce x)

instance PParser PData PInteger where
  pparse x failure =
    pfromData $ pparse @_ @(PAsData PInteger) x (pdata <$> failure)

instance PParser PData PByteString where
  pparse x failure =
    pfromData $ pparse @_ @(PAsData PByteString) x (pdata <$> failure)

instance PParser PData a => PParser PData (PAsData (PBuiltinList a)) where
  pparse x Nothing =
    plet
      (pasList # x)
      ( precList
          (\self y ys -> plet (pparse @PData @a y Nothing) $ const $ self # ys)
          (const $ punsafeCoerce x)
          #
      )
  pparse _ _ = undefined

instance PParser PData (PAsData a) => PParser PData (PBuiltinList (PAsData a)) where
  pparse x failure =
    pfromData $ pparse @_ @(PAsData (PBuiltinList (PAsData a))) x (pdata <$> failure)

-- $> import Plutarch.Prelude; import Plutarch.Internal.Parser; import Plutarch.Pretty

-- $> :k! PlutusRepr (PAsData PByteString)

-- $> :t pconstant @(PAsData (PBuiltinList (PAsData PByteString))) ["hello"]

-- $> :t pconstant @(PAsData (PBuiltinList (PAsData PInteger))) [1, 2, 3, 4]

-- $> plift $ pparse @PData @PByteString (pforgetData $ pconstant @(PAsData PByteString) "hello world") (Just $ pconstant @PByteString "foo bar")

-- $> plift $ pparse @PData @(PBuiltinList (PAsData PInteger)) (pforgetData $ pconstant @(PAsData (PBuiltinList (PAsData PByteString))) ["hello"]) Nothing
