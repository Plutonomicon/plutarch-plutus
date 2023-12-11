module Plutarch.Test.ListSyntax (
  ListSyntax,
  runListSyntax,
  listSyntaxAdd,
  listSyntaxAddSubList,
) where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foldable (for_)

listSyntaxAddSubList :: Semigroup k => k -> ListSyntax (k, v) -> ListSyntax (k, v)
listSyntaxAddSubList name m =
  for_ (runListSyntax m) $ \(k, v) -> do
    let k' = name <> k
    listSyntaxAdd (k', v)

newtype ListSyntaxM elem a = ListSyntax {unListSyntax :: Writer [elem] a}
  deriving newtype (Functor, Applicative, Monad)

type ListSyntax elem = ListSyntaxM elem ()

runListSyntax :: ListSyntax elem -> [elem]
runListSyntax =
  execWriter . unListSyntax

listSyntaxAdd :: elem -> ListSyntax elem
listSyntaxAdd = ListSyntax . tell . one
  where
    one x = [x]
