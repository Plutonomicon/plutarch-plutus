{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.PLam (
  PLamN,
  plam,
  pinl,
) where

import Data.Kind (Type)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Plutarch.Builtin.String (ptraceInfo)
import Plutarch.Internal.PrettyStack (prettyStack)
import Plutarch.Internal.Term (
  Config (Tracing),
  PType,
  S,
  Term,
  pgetConfig,
  plam',
  punsafeConstantInternal,
  (:-->),
  pattern DoTracingAndBinds,
 )
import PlutusCore qualified as PLC

{- $plam
 Lambda abstraction.

 The 'PLamN' constraint allows
 currying to work as expected for any number of arguments.

 > id :: Term s (a :--> a)
 > id = plam (\x -> x)

 > const :: Term s (a :--> b :-> a)
 > const = plam (\x y -> x)
-}

mkstring :: Text.Text -> Term s a
mkstring x = punsafeConstantInternal $ PLC.someValue @Text.Text @PLC.DefaultUni x

class PLamN (a :: Type) (b :: PType) (s :: S) | a -> b, s b -> a where
  plam :: forall c. HasCallStack => (Term s c -> a) -> Term s (c :--> b)

instance {-# OVERLAPPABLE #-} a' ~ Term s a => PLamN a' a s where
  plam f =
    let cs = callStack
     in plam' $ \x -> pgetConfig $ \case
          -- Note: This works because at the moment, DoTracingAndBinds is the
          -- most general tracing mode.
          Tracing _ DoTracingAndBinds ->
            ptraceInfo (mkstring $ prettyStack "L" cs) $ f x
          _ -> f x

instance (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s where
  plam f = withFrozenCallStack $ plam' $ \x -> plam (f x)

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v
