# plutarch-trace

## Usage
You **need** `cabal-install >= 3.0` for this. Your package's cabal file **must** use a `cabal-version` of 3.0 or higher.

* Add `plutarch-trace` to your `build-depends`.

The next steps depend on whether or not you want tracing enabled.
### Enable Tracing
* Add `plutarch-trace:enable` to your `build-depends`.
* Add a mixin similar to the following-

  ```cabal
  plutarch-trace (Plutarch.Trace) requires (Plutarch.TraceSig as Plutarch.Trace.Enable),
  ```

  This makes the `Plutarch.Trace` module available for you to access in your code - and it sets it up with the real tracing functions.

  You can rename the `Plutarch.Trace` module if you want, to something else-
  ```cabal
  plutarch-trace (Plutarch.Trace as FooBar) requires (Plutarch.TraceSig as Plutarch.Trace.Enable),
  ```
  This makes it so you import `Plutarch.Trace` as `FooBar`. This *will* hide the `Plutarch.Trace` module, meaning you can only import it as `FooBar`. Otherwise it's the exact same as just importing `Plutarch.Trace` without renaming.

Example `.cabal` file-
```cabal
cabal-version: 3.0
name: foo
version: 1.0.0

executable foo-exe
  main-is:
    Main.hs
  build-depends:
    base,
    plutarch-trace,
    plutarch-trace:enable
  mixins:
    plutarch-trace (Plutarch.Trace) requires (Plutarch.TraceSig as Plutarch.Trace.Enable)
  default-language: Haskell2010
```

### Disable Tracing
* Add `plutarch-trace:disable` to your `build-depends`.
* Add a mixin similar to the following-

  ```cabal
  plutarch-trace (Plutarch.Trace) requires (Plutarch.TraceSig as Plutarch.Trace.Disable),
  ```

  This makes the `Plutarch.Trace` module available for you to access in your code - and it sets it up with dummy functions that don't actually trace.

  You can rename the `Plutarch.Trace` module if you want, to something else-
  ```cabal
  plutarch-trace (Plutarch.Trace as FooBar) requires (Plutarch.TraceSig as Plutarch.Trace.Enable),
  ```
  This makes it so you import `Plutarch.Trace` as `FooBar`. This *will* hide the `Plutarch.Trace` module, meaning you can only import it as `FooBar`. Otherwise it's the exact same as just importing `Plutarch.Trace` without renaming.

Example `.cabal` file-
```cabal
cabal-version: 3.0
name: foo
version: 1.0.0

executable foo-exe
  main-is:
    Main.hs
  build-depends:
    base,
    plutarch-trace,
    plutarch-trace:disable
  mixins:
    plutarch-trace (Plutarch.Trace as Plutarch.NoTrace) requires (Plutarch.TraceSig as Plutarch.Trace.Disable)
  default-language: Haskell2010
```

### Both!
Yuo want both huh? Well you can do that too! You can import `Plutarch.Trace` with two different setups and alias them to two different names.
* Add both `plutarch-trace:enable` and `plutarch-trace:disable` to your `build-depends`.
* Add both mixins-

  ```cabal
  plutarch-trace (Plutarch.Trace) requires (Plutarch.TraceSig as Plutarch.Trace.Enable),
  plutarch-trace (Plutarch.Trace as Plutarch.NoTrace) requires (Plutarch.TraceSig as Plutarch.Trace.Disable)
  ```

  Here, we set the *tracing enabled* module as `Plutarch.Trace`, and rename the *tracing disabled* module as `Plutarch.NoTrace`. Of course, you need different names for them. So be sure to rename them using `as`.

Example `.cabal` file-
```cabal
cabal-version: 3.0
name: foo
version: 1.0.0

executable foo-exe
  main-is:
    Main.hs
  build-depends:
    base,
    plutarch-trace,
    plutarch-trace:enable,
    plutarch-trace:disable
  mixins:
    plutarch-trace (Plutarch.Trace) requires (Plutarch.TraceSig as Plutarch.Trace.Enable),
    plutarch-trace (Plutarch.Trace as Plutarch.NoTrace) requires (Plutarch.TraceSig as Plutarch.Trace.Disable)
  default-language: Haskell2010
```

### Keep it generic
If you're a library author, you may want to leave the instantiation to the user of your library. In this case, you should simply add `plutarch-trace` as a dependency and use `Plutarch.Trace` in your code. Ignore other deps, ignore mixins.

Library `.cabal` file-
```cabal
cabal-version: 3.0
name: foo
version: 1.0.0

lib
  hs-source-dirs:
    src
  exposed-modules:
    Foo
    Bar
  build-depends:
    base,
    plutarch-trace
  default-language: Haskell2010
```

You won't be able to load your project into the repl with this though. There is no interface file for the abstract `Plutarch.Trace` module. That will only exist after instantiation. See [Common Issues #2](#common-issues).

Users of your must add your library in their `build-depends`, alongside `plutarch-trace:enable` or `plutarch-trace:disable` (or both), alongside the appropriate mixins-
```cabal
  mixins:
    foo (Foo Bar) requires (Plutarch.TraceSig as Plutarch.Trace.Enable)
```

> Note that it's *the `foo` library* that is instantiated with the concrete implementation. Every dependency that uses a generic signature needs to be instantiated like this. If there are more dependencies that are generic (e.g a `plutarch-trace` dependency), all of them need to be instantiated individually (unless you want generic use out of them and want to leave instantiating out to the next user above).

`foo` is the package name that uses `plutarch-trace` in a generic way. `(Foo, Bar)` is a comma separated list of all the modules that you want to make visible from `foo`. If you don't want to rename any of the modules, you can actually just leave out the module list in parens and just write `foo requires ...` to make all modules visible with their original name.

## Contributing
### Learning Backpack
* [really-small-backpack-example](https://github.com/danidiaz/really-small-backpack-example)
* [backpack-str](https://github.com/haskell-backpack/backpack-str)

### Common Issues
* Changed a few module names/structures and getting an error like "The library package ... does not require ..." or "The library package ... does not expose ..."? If you're sure you put the stuff in `signatures` and `exposed-modules` correctly - just do a `cabal clean` and rebuild.
* If GHCi reports that it cannot find interface files for the signatures when loading into `cabal repl` - it means whatever package you're loading into is using a generic signature and not instantiating it with any implementation. You cannot use the repl on these generic packages. You must instantiate it with a concrete impl in your `mixins`.
