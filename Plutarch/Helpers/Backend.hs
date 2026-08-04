module Plutarch.Helpers.Backend (
  getFresh,
) where

import Control.Monad.State (MonadState (get), modify)
import Data.Kind (Type)

-- | @since wip
getFresh ::
  forall (a :: Type) (m :: Type -> Type).
  (MonadState a m, Num a) =>
  m a
getFresh = do
  fresh <- get
  modify (+ 1)
  pure fresh
