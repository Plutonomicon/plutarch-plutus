module Plutarch.Test.TrailSpecMonad (
  -- * Types
  TrailSpecM,
  TrailSpec,
  TrailSpecWith,

  -- * Functions
  runTrailSpec,
  ancestorTrail,

  -- * Variants of hspec functions that work in `TrailSpecM` monad
  describe,
  it,
) where

import Control.Monad.Reader
import GHC.Stack (HasCallStack)
import Test.Hspec.Core.Spec (Arg, Example, SpecM)
import qualified Test.Hspec.Core.Spec
import Test.Hspec.Hedgehog (hedgehog, (===))

type TrailSpec = TrailSpecWith ()

type TrailSpecWith a = TrailSpecM (SpecM a) ()

{- | Like `SpecM`, but also leaves a trail from root node, allowing retrieval of
 ancestor path from anywhere in the test hierarchy. See `ancestorTrail`.

 When using this to define your test hierarchy, you ought to use `describe` to
 group sub-trees. Use `lift` to drop-down into the underlying `SpecM` monad.

 m ~ SpecM a
-}
newtype TrailSpecM m r = TrailSpecM (ReaderT [String] m r)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | Go from `TrailSpecM` monad to `SpecM` monad
runTrailSpec :: TrailSpecM (SpecM a) r -> SpecM a r
runTrailSpec (TrailSpecM m) = runReaderT m []

-- | Return the ancestor groups, from immediate parent to the root.
ancestorTrail :: TrailSpecM (SpecM a) [String]
ancestorTrail = TrailSpecM ask

-- | Like `Test.Hspec.Core.Spec.describe` but for `TrailSpecM`.
describe :: HasCallStack => String -> TrailSpecWith a -> TrailSpecWith a
describe s (TrailSpecM m) =
  TrailSpecM . withReaderT (s :) $ do
    env <- ask
    lift $ Test.Hspec.Core.Spec.describe s $ runReaderT m env

-- | Like `Test.Hspec.Core.Spec.it` but lifted to `TrailSpecM`.
it :: (HasCallStack, Example a) => String -> a -> TrailSpecWith (Arg a)
it s t = lift $ Test.Hspec.Core.Spec.it s t

_test_trailSpec :: TrailSpec
_test_trailSpec = do
  as0 <- ancestorTrail
  it "ancestorTrail at root" . hedgehog $ do
    as0 === []
  describe "child" $ do
    describe "grandchild" $ do
      as <- ancestorTrail
      it "ancestorTrail" . hedgehog $ do
        as === ["grandchild", "child"]
