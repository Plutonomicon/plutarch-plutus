This document discusses various rules of thumb and general trivia, aiming to make life as a Plutarch user or auditor easier.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

- [Plutarch functions are strict](./Tricks/PlutarchFunctionsStrict.md)
- [Don't duplicate work](./Tricks/DontDuplicateWork.md)
  - [Where should arguments be `plet`ed?](./Tricks/DontDuplicateWork.md#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](./Tricks/PreferPlutarchFunctions.md)
- [When to use Haskell level functions?](./Tricks/UsingHaskellLevelFunctions.md)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./Tricks/DifferenceBetweenPconAndPconstant.md)
- [Let Haskell level functions take responsibility of evaluation](./Tricks/ResponsibilityOfEvaluationInHaskellFunctions.md)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./Tricks/makeIsDataIndexed,HaskellADTs,PIsDataRepr.md)
- [Prefer statically building constants whenever possible](./Tricks/PreferStaticallyBuildingConstants.md)
- [Figuring out the representation of a Plutarch type](./Tricks/RepresentationOfPlutarchType.md)
- [Prefer pattern matching on the result of `pmatch` immediately](./Tricks/PreferMatchingOnPmatchResultImmediately.md)
- [Working with bound fields yielded by `pletFields`](./Tricks/WorkingWithBoundFields.md)
