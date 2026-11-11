---
synopsis: Build packages with build-type Hooks per component
packages: [cabal-install, Cabal]
issues: 9986
---

`cabal-install` now builds the components of a `build-type: Hooks` package one
by one, as it does for `Simple` and `Configure` packages, instead of falling
back to building the package as a whole. A `Hooks` package can therefore have
internal libraries, which need a per-component build.

The components share the package's setup, which is compiled once.

This needs the fix to `Cabal` described below, so it applies to a package whose
setup is built with `Cabal` 3.19 or later. With an older `Cabal` the package is
still built as a whole.

In `Cabal`, a pre-build rule that runs a build tool of its own package now finds
the tool when a single component is configured: the executable is then built
separately and found on the `PATH`, like the executable of another package.
