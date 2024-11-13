# Prefer Plutarch level functions

Plutarch level functions have a lot of advantages - they can be hoisted; they are strict so you can [use their arguments however many times you like without duplicating work](./Don't%20duplicate%20work.md) etc. Unless you _really_ need laziness, like `pif` does, try to use Plutarch level functions.

Also see: [Hoisting](./../Concepts/Hoisting.md).
