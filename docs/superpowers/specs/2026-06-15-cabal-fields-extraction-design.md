# Design: extract a focused `Cabal-fields` package

Date: 2026-06-15

## Goal

Separate the parsing of the *cabal file format* — the layer "up to `Field`" — out of
`Cabal-syntax` and into a new, focused package `Cabal-fields`. `Cabal-fields` should be
the lower layer (`Cabal-syntax` depends on it), should contain only what is needed to lex
and parse a cabal file into a `[Field Position]` (and pretty-print it back), and should
**not** depend on `Cabal-syntax` or drag the heavy `Distribution.Compat.Prelude` /
value-parsing / pretty machinery along with it.

## Background / constraints discovered

- The natural seam is `readFields`, which is **pure** and independent of the higher-level
  parse monad:

  ```haskell
  readFields  :: B8.ByteString -> Either Text.Parsec.Error.ParseError [Field Position]
  readFields' :: B8.ByteString -> Either Text.Parsec.Error.ParseError ([Field Position], [LexWarning])
  ```

  (`ParseError` is parsec's own type, not `Distribution.Fields.ParseResult`.)

- `Distribution.Types.Version` imports `Distribution.Parsec` (the `Parsec` type-class) **and**
  `Distribution.Pretty`. Therefore any module needing `Version` transitively needs the whole
  value-parsing + pretty machinery. `Distribution.Fields.ParseResult` (stores the
  cabal-spec-version as `Maybe Version`) and `Distribution.Fields.ConfVar` (parses condition
  expressions into `Version`/`Condition`/`ConfVar`) both need `Version`. **They cannot move
  into a focused `Cabal-fields`.** This is the boundary.

- The `Field` AST derives only `Eq/Show/Functor/Foldable/Traversable/Ord` — no
  `Binary`/`NFData`/`Structured`. The support types that *do* move (`Position`, `PWarning`,
  `PWarnType`, `PSource`) carry only `Binary` + `NFData` instances — **no `Structured`** — so
  `Cabal-fields` never needs `Distribution.Utils.Structured`.

## Decisions (confirmed with the user)

1. **`ConfVar` stays in `Cabal-syntax`** (it is semantic, beyond the raw field format).
2. **Backward compatibility via re-export**: `Cabal-syntax` depends on `Cabal-fields` and uses
   `reexported-modules:` so existing imports keep working. No downstream package changes.
3. **Baseline / VCS**: the on-disk files are ground truth (jj workspace; the stray `.git` was
   removed). Use **jj** for all version-control operations.
4. **`ParseResult` stays in `Cabal-syntax`** — `readFields` does not use it, and it needs
   `Version`.
5. **Support modules are renamed into the `Distribution.Fields.*` namespace** so that
   `Cabal-fields` uses no `Distribution.Parsec.*` module names:
   `Distribution.Parsec.{Position,Source,Warning}` become
   `Distribution.Fields.{Position,Source,Warning}`. The former names remain importable via
   re-export aliases from `Cabal-syntax` (so no other package changes).
6. **The unused `CABAL_PARSEC_DEBUG` debug code is dropped** from the moved lexer/parser
   modules (it was gated behind a flag that was never wired up), so `Cabal-fields` needs
   neither `vector` nor `Debug.Trace`.

## Module split

### Move into `Cabal-fields` (new, lower-layer package)

| Module | Notes |
|---|---|
| `Distribution.Fields.Field` | `Field`/`FieldLine`/`SectionArg`/`Name` AST |
| `Distribution.Fields.Lexer` | Alex `.x` source (needs `alex` build-tool + `array`) |
| `Distribution.Fields.LexerMonad` | the `Lex` monad + lexer warning types |
| `Distribution.Fields.Parser` | `readFields` / `readFields'` |
| `Distribution.Fields.Pretty` | `showFields`, `PrettyField`, `fromParsecFields`, … |
| `Distribution.Fields.Position` | positions (renamed from `Distribution.Parsec.Position`) |
| `Distribution.Fields.Warning` | `PWarning`/`PWarnType`/`PWarningWithSource` (references `Source`; renamed from `Distribution.Parsec.Warning`) |
| `Distribution.Fields.Source` | `PSource` (needed by `Warning`; renamed from `Distribution.Parsec.Source`) |
| `Distribution.Fields.Internal` | **new**, non-exposed: small copies of `showToken`/`showTokenStr` (from `Distribution.Pretty`) and `fromUTF8BS` (from `Distribution.Utils.Generic`), so those heavy modules are not dragged in |

### Stay in `Cabal-syntax`

| Module | Why it stays |
|---|---|
| `Distribution.Fields` (umbrella) | kept as-is so its public export list is unchanged; imports moved sub-modules from `Cabal-fields` + `ParseResult` locally |
| `Distribution.Fields.ParseResult` | stores `Maybe Version` |
| `Distribution.Fields.ConfVar` | needs `Version`/`Condition`/`ConfVar` + `Parsec` class |
| `Distribution.Parsec.Error` | used by `ParseResult`; imports `Distribution.Fields.Source`/`Warning` from `Cabal-fields` |
| `Distribution.Parsec.FieldLineStream` | value-parsing bridge; used by `ConfVar` / `Parsec` class |
| `Distribution.Parsec` (the class) | heavy value-parsing machinery |

## Dropping `Distribution.Compat.Prelude`

Each moved module replaces `import Distribution.Compat.Prelude` (+ `import Prelude ()`) with
explicit imports from `base` / `bytestring` / `containers` (and `Data.List.NonEmpty`,
`Data.Map.Strict`, etc. as currently used). For the instances on moved types:

- `Binary`: import `Binary(..)` directly from `Data.Binary` (the empty instances rely on
  binary's `Generic` default; `binary >= 0.8`).
- `NFData`: replace `instance NFData T where rnf = genericRnf` with an **empty**
  `instance NFData T`, relying on deepseq's `Generic` default method (`deepseq >= 1.4.3`).

No `Structured` instances are involved.

### Target `Cabal-fields` build-depends

`base`, `bytestring`, `containers`, `array` (Alex), `parsec`, `binary`, `deepseq`, `text`
(for the UTF8 helper), `pretty` (`Fields.Pretty` builds `PP.Doc`), `filepath` (`Warning` uses
`normalise`). **No** `vector`, `mtl`, or `transformers`: the `Lex` monad is hand-rolled and the
`Parser` uses parsec + `Data.Functor.Identity`. The old `#ifdef CABAL_PARSEC_DEBUG` debug code
(which pulled in `text`/`vector` and `Debug.Trace`) has been removed; `text` remains only for the
UTF8 helper. Version bounds copied from `Cabal-syntax.cabal`. `build-tool-depends: alex:alex`
moves here.

## Package wiring

- New `Cabal-fields/Cabal-fields.cabal` (`cabal-version: 3.6`, version/license/author copied
  from `Cabal-syntax`, `hs-source-dirs: src`), exposing the eight moved modules and listing
  the internal helper `Distribution.Fields.Internal` under `other-modules`. It also ships the
  grammar reference via `extra-doc-files: GRAMMAR.md`.
- `Cabal-syntax.cabal`: drop the moved modules from `exposed-modules`; add
  `build-depends: Cabal-fields ^>= 3.17`; add a `reexported-modules:` stanza that re-exports
  all eight moved modules under their new names **and** re-exports
  `Distribution.Fields.{Position,Source,Warning}` under their former
  `Distribution.Parsec.{Position,Source,Warning}` names (via `… as …`) for backward
  compatibility; remove the now-unused `alex` build-tool. (`array` is **kept** — it is still
  used by `Distribution.Compat.Graph` and `Language.Haskell.Extension`.)
- Register `Cabal-fields` in `project-cabal/pkgs/cabal.config` **and** in the standalone
  `cabal.bootstrap.project` (which lists its packages manually rather than importing the config).

## Testing / verification

This is a pure code move; existing suites are the safety net.

1. **`Cabal-fields` builds in isolation** (`cabal build Cabal-fields`), **and** a grep over the
   moved sources proves zero residual `import Distribution.*` outside the moved/internal set —
   the real proof it does not depend on `Cabal-syntax`.
2. `cabal build Cabal-syntax Cabal cabal-install` succeeds (re-exports resolve; no downstream
   edits needed).
3. Existing `Cabal-tests` parser tests pass unchanged.

## Out of scope

- Migrating downstream packages to import from `Cabal-fields` directly (re-export keeps them
  working; can be done later).
- Moving `ParseResult`/`ConfVar`/`Version` or refactoring the `Parsec` class.
- Introducing a `Distribution.Fields` umbrella in `Cabal-fields` (its name is occupied by
  `Cabal-syntax`'s richer umbrella); `Distribution.Fields.Parser` serves as the entry point.
