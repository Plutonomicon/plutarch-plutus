# Haskell synonym of Plutarch types

Several sections of the guide use the terminology "Haskell synonym". What does it mean? It's simply the Haskell type that _is supposed to_ correspond to a Plutarch type. There doesn't _necessarily_ have to be some sort of concrete connection (though there can be, using [`PLiftable`](../Typeclasses/PLiftable.md)) - it's merely a connection you can establish mentally.

This detail does come into play in concrete use cases though. After compiling your Plutarch code to a `Script`, when you pass Haskell data types as arguments to the `Script` - they obviously need to correspond to the actual arguments of the Plutarch code. For example, if the Plutarch code is a function taking `PByteString`, after compilation to `Script`, you _should_ pass in the Haskell data type that actually shares the same representation as `PByteString` - the "Haskell synonym", so to speak. In this case, that's `ByteString`\*.

\[\*]: You can't actually pass a `ByteString` into a compiled script. Notice that you can only pass `Data` arguments using `applyArguments` (from `Plutarch.Evaluate`). The Haskell synonym to `Data` is `PAsData a` (for any `a`), and `PData`.

Also see: [Figuring out the representation of a Plutarch type](../Tricks/RepresentationOfPlutarchType.md).
