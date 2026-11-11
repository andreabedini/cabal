---
synopsis: Build a package's setup once, as a node of its own in the build plan
packages: [cabal-install]
issues: [9986]
---

A package whose setup has to be compiled (`build-type: Custom` or `Hooks`, or a
`cabal-version` newer than `cabal-install` knows) now has that setup as a
separate entry in the build plan. It is compiled once, before the package, and
shows up in the list of things to build:

```
In order, the following will be built:
 - foo-1.0 (setup) (first run)
 - foo-1.0 (lib:foo) (first run)
```

Before, the setup was compiled as a side effect of configuring the package, and
checked again before every later step.

- When the setup is rebuilt, the package is configured again, not just built
  again. This covers a change to `Setup.hs` or `SetupHooks.hs`, to a module of
  the package that they import, and to a local package the setup depends on.
- In a multi-repl session, a local package the setup depends on is always built
  on disk, as one that a build tool depends on is.
- A setup that is not built inside the project is kept in the store and shared
  between projects. Packages with a compiled setup get a new hash, so they are
  rebuilt once.
- `plan.json` lists the setup as an entry with `"component-name": "setup"`. The
  entries of the package refer to it with the new key `setup-depends`.
  `components.setup.depends`, on the entry of a package that is built as a whole,
  is unchanged.
- A package with a `cabal-version` newer than `cabal-install` knows compiles its
  own `Setup` executable instead of sharing one from the setup executable cache.
  A per-component build of such a package used to fail with an internal error
  when that cache had no matching executable.
