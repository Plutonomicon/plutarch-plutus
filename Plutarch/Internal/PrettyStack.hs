module Plutarch.Internal.PrettyStack (prettyStack) where

import Data.Text (Text, pack)
import GHC.Stack (CallStack, SrcLoc (srcLocFile, srcLocStartLine), getCallStack)

prettyStack :: Text -> CallStack -> Text
prettyStack prefix cs = case getCallStack cs of
  ((_, loc) : _) ->
    let loc' = pack loc.srcLocFile <> ":" <> pack (show loc.srcLocStartLine)
     in prefix <> "[" <> loc' <> "]"
  _ -> "<missing call stack>"
