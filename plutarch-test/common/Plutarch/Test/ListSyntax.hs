module Plutarch.Test.ListSyntax (
  ListSyntax,
  runListSyntax,
  listSyntaxAdd,
  listSyntaxAddSubList,
) where

import Control.Monad (void)
import Control.Monad.Writer (Writer, execWriter, tell)

listSyntaxAddSubList :: Semigroup k => k -> ListSyntax (k, v) -> ListSyntax (k, v)
listSyntaxAddSubList name m =
  void $
    flip traverse (runListSyntax m) $ \(k, v) -> do
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
