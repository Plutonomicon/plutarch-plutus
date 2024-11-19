{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.PLam (
  plam,
  plamNamed,
  pinl,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack, getCallStack, withFrozenCallStack) -- , SrcLoc(..))
import Plutarch.Internal (
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
import Plutarch.Internal.PrettyStack (prettyStack)
import Plutarch.Internal.Trace (ptraceInfo)
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

class PLamNNamed (a :: Type) (b :: PType) (s :: S) | a -> b, s b -> a where
  plamNamed' :: forall c. HasCallStack => Maybe Text -> (Term s c -> a) -> Term s (c :--> b)

instance {-# OVERLAPPABLE #-} a' ~ Term s a => PLamNNamed a' a s where
  plamNamed' name f =
    let cs = callStack
     in plam' name $ \x -> pgetConfig $ \case
          -- Note: This works because at the moment, DoTracingAndBinds is the
          -- most general tracing mode.
          Tracing _ DoTracingAndBinds ->
            ptraceInfo (mkstring $ prettyStack "L" cs) $ f x
          _ -> f x

instance (a' ~ Term s a, PLamNNamed b' b s) => PLamNNamed (a' -> b') (a :--> b) s where
  plamNamed' name f = withFrozenCallStack $ plam' name $ \x -> plamNamed' name (f x)

plamNamed :: forall a b c s. (HasCallStack, PLamNNamed a b s) => Text -> (Term s c -> a) -> Term s (c :--> b)
plamNamed name =
  plamNamed' (Just name)

plam :: forall a b c s. (HasCallStack, PLamNNamed a b s) => (Term s c -> a) -> Term s (c :--> b)
plam =
  let
    cs = getCallStack callStack
    name =
      case dropWhile (\(x, _) -> x == "plam") cs of
        [] -> Nothing
        cs' ->
          let
            (functionName, _details) = head cs'
           in
            --          in Just $ Text.pack $ srcLocFile details <> ":" <> show (srcLocStartLine details) <> ":" <> functionName
            Just $ Text.pack functionName
   in
    plamNamed' name

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v
