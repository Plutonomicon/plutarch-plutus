module Plutarch.Pretty.Internal.Types (
  PrettyCursor (..),
  PrettyState (..),
  PrettyMonad,
  forkState,
  normalizeCursor,
  specializeCursor,
  memorizeName,
  insertName,
  insertBindings,
  builtinFunAtRef,
  nameOfRef,
) where

import Control.Monad ((<=<))
import Control.Monad.Reader (ReaderT)
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState (get, put), StateT)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Txt
import System.Random.Stateful (STGenM, StdGen)

import PlutusCore qualified as PLC
import UntypedPlutusCore (DefaultFun, Index)

import Plutarch.Pretty.Internal.Config (forcedPrefix)

{- | Notifies the prettifier what "state" the cursor currently is, so it can decide
whether or not to wrap the target expression in parens.

Normal indicates no parens wrapping is necessary, even for complex expressions.

Special indicates complex expressions should be wrapped in parens.

Usually, "Special" just hints at one of three states:

1. Applying - The expression is being applied like a function.
2. Applied - The expression is being applied as a function argument.
3. Unary arg - The expression is being used as an argument to a high arity unary operator (~ and !).
-}
data PrettyCursor = Normal | Special
  deriving stock (Bounded, Enum, Eq, Show)

data PrettyState = PrettyState
  { ps'nameMap :: Map Index Text
  , ps'names :: Set Text
  , ps'cursor :: PrettyCursor
  }

type PrettyMonad s = ReaderT (STGenM StdGen s) (StateT PrettyState (ST s))

forkState :: MonadState s m => m b -> m b
forkState x = get >>= (\s -> x <* put s)

normalizeCursor :: PrettyState -> PrettyState
normalizeCursor x = x {ps'cursor = Normal}

specializeCursor :: PrettyState -> PrettyState
specializeCursor x = x {ps'cursor = Special}

memorizeName :: Text -> PrettyState -> PrettyState
memorizeName n x@PrettyState {ps'names} = x {ps'names = Set.insert n ps'names}

-- | Insert a fresh binding onto the name map, i.e a name at index 0 - incrementing all other indices.
insertName :: Text -> PrettyState -> PrettyState
insertName name x@PrettyState {ps'nameMap} =
  x
    { ps'nameMap = Map.mapKeys (+ 1) ps'nameMap <> Map.singleton 0 name
    }

insertBindings :: [Text] -> PrettyState -> PrettyState
insertBindings names prst@PrettyState {ps'nameMap} =
  prst
    { ps'nameMap =
        Map.mapKeys (+ nameCount) ps'nameMap
          <> foldMap (uncurry Map.singleton) (zip [0 .. (nameCount - 1)] names)
    }
  where
    nameCount = fromIntegral $ length names

builtinFunAtRef :: Map Index Text -> Index -> Maybe DefaultFun
builtinFunAtRef nameMap = builtinFunFromName <=< flip nameOfRef nameMap

nameOfRef :: Index -> Map Index Text -> Maybe Text
nameOfRef ix = Map.lookup (ix - 1)

builtinFunFromName :: Text -> Maybe DefaultFun
builtinFunFromName res =
  if Txt.take prefixLen res == forcedPrefix
    then helper $ Txt.drop prefixLen res
    else helper res
  where
    prefixLen = Txt.length forcedPrefix
    helper s = find (\e -> showText e == s) builtinFunNames
    builtinFunNames = [minBound .. maxBound] :: [PLC.DefaultFun]

showText :: Show a => a -> Text
showText = Txt.pack . show
