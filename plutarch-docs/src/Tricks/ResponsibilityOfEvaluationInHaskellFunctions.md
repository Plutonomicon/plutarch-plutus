# Let Haskell level functions take responsibility of evaluation

We've discussed how a Haskell level function that operates on Plutarch level terms needs to 
[be careful](./Don't%20duplicate%20work.md) about [work duplication](../Usage/Avoid%20work%20duplication%20using%20plet.md). 
Related to this point, it's good practice to design your Haskell level functions so that _it takes responsibility_ for evaluation.

The user of your Haskell level function doesn't know how many times it uses the argument it has been passed! If it uses the 
argument multiple times without `plet`ing it - there's duplicate work! There are two solutions here:

- The user `plet`s the argument before passing it to the Haskell level function.
- The Haskell level function takes responsibility of its argument and `plet`s it itself.

The former is problematic since it's based on _assumption_. What if the Haskell level function is a good rule follower, and correctly `plet`s its argument if using it multiple times? Well, then there's a redundant `plet` (though back-to-back `plet`s _will_ be optimized away into one).

Instead, try to offload the responsibility for evaluation to the Haskell level function - so that it only `plet`s when it needs to.
