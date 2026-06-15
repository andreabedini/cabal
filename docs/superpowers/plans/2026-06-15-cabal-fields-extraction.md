# Cabal-fields Extraction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the cabal-file-format parsing layer ("up to `Field`") out of `Cabal-syntax` into a new, focused, lower-layer package `Cabal-fields` that does not depend on `Cabal-syntax` or on `Distribution.Compat.Prelude`.

**Architecture:** Eight modules move down into `Cabal-fields` (`Distribution.Fields.{Field,Lexer,LexerMonad,Parser,Pretty}` + `Distribution.Parsec.{Position,Warning,Source}`), plus a new non-exposed helper `Distribution.Fields.Internal` holding small copies of `showToken`/`showTokenStr`/`fromUTF8BS` (so the heavy `Distribution.Pretty`/`Distribution.Utils.Generic` are not dragged in). `Cabal-syntax` depends on `Cabal-fields` and re-exports the moved modules via `reexported-modules:` so no downstream package changes. `ParseResult`, `ConfVar`, the `Distribution.Fields` umbrella, `Distribution.Parsec.Error`, `Distribution.Parsec.FieldLineStream`, and the `Parsec` class stay in `Cabal-syntax` (they need `Version`).

**Tech Stack:** Haskell, Cabal (`cabal-version: 3.6`), Alex, parsec; deps `base, bytestring, containers, array, binary, deepseq, parsec, pretty, text, filepath`. Build tool: `cabal build`. VCS: **jj** (see note).

---

## VCS note (read first)

This is a **jj** workspace. The sandbox cannot write to the jj store, so file creation/edits/moves
are done by the implementer, but **commit checkpoints must be run by the user**. Each task ends with
a checkpoint command of the form:

```
jj describe -m "<message>" && jj new
```

jj auto-snapshots the working copy, so the file moves/edits are already captured; `jj describe`
just labels the change and `jj new` starts the next one. If running tasks via subagents, surface
the checkpoint command to the user and wait for them to run it before the next task. Do **not** use
`git` — the repository has no `.git` directory by design.

## File move reference (source → destination)

All moves keep the module name and the `src/Distribution/...` sub-path; only the package directory changes.

| From | To |
|---|---|
| `Cabal-syntax/src/Distribution/Parsec/Position.hs` | `Cabal-fields/src/Distribution/Parsec/Position.hs` |
| `Cabal-syntax/src/Distribution/Parsec/Source.hs` | `Cabal-fields/src/Distribution/Parsec/Source.hs` |
| `Cabal-syntax/src/Distribution/Parsec/Warning.hs` | `Cabal-fields/src/Distribution/Parsec/Warning.hs` |
| `Cabal-syntax/src/Distribution/Fields/Field.hs` | `Cabal-fields/src/Distribution/Fields/Field.hs` |
| `Cabal-syntax/src/Distribution/Fields/LexerMonad.hs` | `Cabal-fields/src/Distribution/Fields/LexerMonad.hs` |
| `Cabal-syntax/src/Distribution/Fields/Lexer.x` | `Cabal-fields/src/Distribution/Fields/Lexer.x` |
| `Cabal-syntax/src/Distribution/Fields/Parser.hs` | `Cabal-fields/src/Distribution/Fields/Parser.hs` |
| `Cabal-syntax/src/Distribution/Fields/Pretty.hs` | `Cabal-fields/src/Distribution/Fields/Pretty.hs` |

## Import-rewrite recipe (applies to every moved module)

Every moved module currently has `import Distribution.Compat.Prelude` and `import Prelude ()`.
The recipe for each is:

1. **Delete** the line `import Distribution.Compat.Prelude`.
2. **Delete** the line `import Prelude ()` (this restores the normal `Prelude`, so basic names like
   `map`, `Maybe`, `++`, `show` need no import).
3. **Add** explicit imports only for the non-`Prelude` identifiers the module uses. Each task below
   lists the exact imports to add for that module.
4. Replace any `instance NFData T where rnf = genericRnf` with a bare `instance NFData T` (relies on
   deepseq's `Generic` default; `deepseq >= 1.4.3`).
5. Build (`cabal build Cabal-fields`). If GHC reports a not-in-scope identifier, add the obvious
   `base`/`containers` import for it, then rebuild. Repeat until clean.

---

### Task 1: Scaffold the `Cabal-fields` package

**Files:**
- Create: `Cabal-fields/Cabal-fields.cabal`
- Create: `Cabal-fields/src/Distribution/Fields/Internal.hs`
- Create: `Cabal-fields/LICENSE` (copy of `Cabal-syntax/LICENSE`)
- Modify: `project-cabal/pkgs/cabal.config`

- [ ] **Step 1: Create the package directory and copy the licence**

```bash
mkdir -p Cabal-fields/src/Distribution/Fields Cabal-fields/src/Distribution/Parsec
cp Cabal-syntax/LICENSE Cabal-fields/LICENSE
```

- [ ] **Step 2: Write `Cabal-fields/Cabal-fields.cabal`**

```cabal
cabal-version: 3.6
name:          Cabal-fields
version:       3.17.0.0
copyright:     2003-2025, Cabal Development Team (see AUTHORS file)
license:       BSD-3-Clause
license-file:  LICENSE
author:        Cabal Development Team <cabal-devel@haskell.org>
maintainer:    cabal-devel@haskell.org
homepage:      http://www.haskell.org/cabal/
bug-reports:   https://github.com/haskell/cabal/issues
synopsis:      Parsing of the .cabal file format up to fields
description:
    This library provides the lexer and parser for the .cabal file format,
    producing a list of fields and sections from a ByteString. It is the
    low-level parsing layer underneath Cabal-syntax.
category:       Distribution
build-type:     Simple

source-repository head
  type:     git
  location: https://github.com/haskell/cabal/
  subdir:   Cabal-fields

library
  default-language: Haskell2010
  hs-source-dirs: src

  build-depends:
    , array      >= 0.4.0.1  && < 0.6
    , base       >= 4.17     && < 5
    , binary     >= 0.8.1    && < 0.9
    , bytestring >= 0.10.4.0 && < 0.13
    , containers >= 0.5.2    && < 0.9
    , deepseq    >= 1.4.3    && < 1.7
    , filepath   >= 1.3.0.1  && < 1.6
    , parsec     >= 3.1.13.0 && < 3.2
    , pretty     >= 1.1.1    && < 1.2
    , text       >= 2.0.2    && < 2.2

  ghc-options:
    -Wall
    -fno-ignore-asserts
    -Wtabs
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wno-unticked-promoted-constructors
    -Wcompat
    -Wnoncanonical-monad-instances

  build-tool-depends:
    -- https://github.com/haskell/alex/issues/288
    alex:alex < 3.5.4.1 || > 3.5.4.1

  -- exposed-modules are added one task at a time as the files arrive;
  -- by the end of Task 7 this block lists all eight moved modules:
  --   Distribution.Fields.Field
  --   Distribution.Fields.Lexer
  --   Distribution.Fields.LexerMonad
  --   Distribution.Fields.Parser
  --   Distribution.Fields.Pretty
  --   Distribution.Parsec.Position
  --   Distribution.Parsec.Source
  --   Distribution.Parsec.Warning
  exposed-modules:

  other-modules:
    Distribution.Fields.Internal
```

Note: start with an **empty** `exposed-modules:` (only `Distribution.Fields.Internal` under
`other-modules`). A library with no exposed modules and one internal module builds fine. Each later
task adds its module(s) to `exposed-modules` once the corresponding file has been moved in.

- [ ] **Step 3: Write the internal helper `Cabal-fields/src/Distribution/Fields/Internal.hs`**

These are verbatim copies of `showToken`/`showTokenStr` (from `Distribution.Pretty`) and
`fromUTF8BS` (from `Distribution.Utils.Generic`), the only three symbols the moved modules need
from those heavy modules.

```haskell
-- | Internal helpers copied from Distribution.Pretty and Distribution.Utils.Generic,
-- so that Cabal-fields does not depend on those heavy modules.
module Distribution.Fields.Internal
  ( showToken
  , showTokenStr
  , fromUTF8BS
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.ByteString as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Text.PrettyPrint as PP

-- | Pretty-print a token, quoting it if it looks dodgy.
showToken :: String -> PP.Doc
showToken = PP.text . showTokenStr

showTokenStr :: String -> String
showTokenStr str
  -- if token looks like a comment (starts with --), print it in quotes
  | "--" `isPrefixOf` str = show str
  -- also if token ends with a colon (e.g. executable name), print it in quotes
  | ":" `isSuffixOf` str = show str
  | not (any dodgy str) && not (null str) = str
  | otherwise = show str
  where
    dodgy c = isSpace c || c == ','

-- | Decode 'SBS.ByteString' to 'String' as UTF8, replacing invalid bytes leniently.
fromUTF8BS :: SBS.ByteString -> String
fromUTF8BS = T.unpack . T.decodeUtf8With T.lenientDecode
```

- [ ] **Step 4: Register the package in `project-cabal/pkgs/cabal.config`**

Change:

```cabal
packages:
    Cabal
  , Cabal-described
  , Cabal-syntax
  , Cabal-hooks
```

to:

```cabal
packages:
    Cabal
  , Cabal-described
  , Cabal-fields
  , Cabal-syntax
  , Cabal-hooks
```

- [ ] **Step 5: Build the skeleton**

Run: `cabal build Cabal-fields`
Expected: PASS — only `Distribution.Fields.Internal` compiles (`exposed-modules` is empty for now).

- [ ] **Step 6: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: scaffold package with internal helper module" && jj new
```

---

### Task 2: Move `Position`, `Source`, `Warning`

**Files:**
- Move: `Position.hs`, `Source.hs`, `Warning.hs` (see move reference table)
- Modify: each moved file's imports
- Modify: `Cabal-fields/Cabal-fields.cabal` (add the three `Distribution.Parsec.*` exposed-modules)

- [ ] **Step 1: Move the three files**

```bash
mv Cabal-syntax/src/Distribution/Parsec/Position.hs Cabal-fields/src/Distribution/Parsec/Position.hs
mv Cabal-syntax/src/Distribution/Parsec/Source.hs   Cabal-fields/src/Distribution/Parsec/Source.hs
mv Cabal-syntax/src/Distribution/Parsec/Warning.hs  Cabal-fields/src/Distribution/Parsec/Warning.hs
```

- [ ] **Step 2: Rewrite `Position.hs` imports**

Apply the import-rewrite recipe. Replace the import block
```haskell
import Distribution.Compat.Prelude
import Prelude ()
```
with
```haskell
import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
```
and change `instance NFData Position where rnf = genericRnf` to `instance NFData Position`.

- [ ] **Step 3: Rewrite `Source.hs` imports**

Replace
```haskell
import qualified Data.ByteString as BS
import Distribution.Compat.Prelude
import Prelude ()
```
with
```haskell
import qualified Data.ByteString as BS
import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
```
and change `instance NFData src => NFData (PSource src) where rnf = genericRnf` to
`instance NFData src => NFData (PSource src)`.

- [ ] **Step 4: Rewrite `Warning.hs` imports**

Replace
```haskell
import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import Distribution.Parsec.Source
import System.FilePath (normalise)
import Prelude ()
```
with
```haskell
import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Distribution.Parsec.Position
import Distribution.Parsec.Source
import System.FilePath (normalise)
```
and change both `instance NFData ... where rnf = genericRnf` lines (`PWarnType`, `PWarning`) to bare
`instance NFData PWarnType` and `instance NFData PWarning`.

- [ ] **Step 5: Add the three exposed modules in `Cabal-fields.cabal`**

Add these three lines under `exposed-modules:`:
```
    Distribution.Parsec.Position
    Distribution.Parsec.Source
    Distribution.Parsec.Warning
```

- [ ] **Step 6: Build**

Run: `cabal build Cabal-fields`
Expected: PASS. If GHC flags a missing name, add its `base`/`containers` import and rebuild.

- [ ] **Step 7: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Parsec Position/Source/Warning, drop Compat.Prelude" && jj new
```

---

### Task 3: Move `Field`

**Files:**
- Move: `Field.hs`
- Modify: `Field.hs` imports; `Cabal-fields.cabal` (add `Distribution.Fields.Field` to exposed-modules)

- [ ] **Step 1: Move the file**

```bash
mv Cabal-syntax/src/Distribution/Fields/Field.hs Cabal-fields/src/Distribution/Fields/Field.hs
```

- [ ] **Step 2: Rewrite imports**

Replace
```haskell
import Distribution.Compat.Prelude
import Distribution.Pretty (showTokenStr)
import Distribution.Utils.Generic (fromUTF8BS)
import Prelude ()
```
with
```haskell
import Data.List (intercalate)
import GHC.Generics (Generic)
import Distribution.Fields.Internal (showTokenStr, fromUTF8BS)
```
Keep the existing `import Data.ByteString`, `import qualified Data.ByteString.Char8 as B`,
`import qualified Data.Char as Char`, and the CPP-guarded `import qualified Data.Foldable1 as F1`.
Note: the `Foldable1` instances (under `#if MIN_VERSION_base(4,18,0)`) use the `:|` constructor; if
GHC reports `:|` not in scope, add — inside that same CPP guard — `import Data.List.NonEmpty (NonEmpty ((:|)))`.

- [ ] **Step 3: Add `Distribution.Fields.Field` to `exposed-modules` in `Cabal-fields.cabal`**

- [ ] **Step 4: Build**

Run: `cabal build Cabal-fields`
Expected: PASS (resolve any remaining not-in-scope names per the recipe).

- [ ] **Step 5: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Fields.Field, drop Compat.Prelude" && jj new
```

---

### Task 4: Move `LexerMonad`

**Files:**
- Move: `LexerMonad.hs`
- Modify: `LexerMonad.hs` imports; `Cabal-fields.cabal` (add `Distribution.Fields.LexerMonad` to exposed-modules)

- [ ] **Step 1: Move the file**

```bash
mv Cabal-syntax/src/Distribution/Fields/LexerMonad.hs Cabal-fields/src/Distribution/Fields/LexerMonad.hs
```

- [ ] **Step 2: Rewrite imports**

Delete `import Distribution.Compat.Prelude` and `import Prelude ()`. Keep the existing
`import qualified Data.ByteString as B`, `import qualified Data.List.NonEmpty as NE`,
`import qualified Data.Map.Strict as Map`, the `Distribution.Parsec.Position`/`Warning` imports, and
the entire `#ifdef CABAL_PARSEC_DEBUG` block (text/vector, dead by default — leave verbatim).
If GHC reports a not-in-scope name after removal (e.g. `foldl'` from `Data.List`, or
`NonEmpty(..)`), add the matching `base`/`containers` import per the recipe.

- [ ] **Step 3: Add `Distribution.Fields.LexerMonad` to `exposed-modules` in `Cabal-fields.cabal`**

- [ ] **Step 4: Build**

Run: `cabal build Cabal-fields`
Expected: PASS.

- [ ] **Step 5: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Fields.LexerMonad, drop Compat.Prelude" && jj new
```

---

### Task 5: Move `Lexer.x` (Alex)

**Files:**
- Move: `Lexer.x`
- Modify: `Lexer.x` imports; `Cabal-fields.cabal` (add `Distribution.Fields.Lexer` to exposed-modules)

- [ ] **Step 1: Move the file**

```bash
mv Cabal-syntax/src/Distribution/Fields/Lexer.x Cabal-fields/src/Distribution/Fields/Lexer.x
```

- [ ] **Step 2: Rewrite imports**

Delete `import Distribution.Compat.Prelude`. Keep `import Prelude ()` **and**
`import qualified Prelude` as they are (the Alex template references `Prelude.*` qualified). Keep the
explicit `Data.ByteString`/`Data.Word`/`Distribution.Fields.LexerMonad`/`Distribution.Parsec.Position`
imports and the `#ifdef CABAL_PARSEC_DEBUG` block verbatim. If GHC reports names previously provided
by `Compat.Prelude` as not in scope (likely `Control.Monad` helpers such as `when`/`unless`, or
`Data.Char` functions), add the corresponding `base` imports, e.g.
```haskell
import Control.Monad (when, unless)
import Data.Char (chr, ord)
```
(add only those actually reported), then rebuild.

- [ ] **Step 3: Add `Distribution.Fields.Lexer` to `exposed-modules` in `Cabal-fields.cabal`**

- [ ] **Step 4: Build**

Run: `cabal build Cabal-fields`
Expected: PASS (Alex runs as a build tool; `alex:alex` is already in `build-tool-depends`).

- [ ] **Step 5: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Fields.Lexer (Alex), drop Compat.Prelude" && jj new
```

---

### Task 6: Move `Parser`

**Files:**
- Move: `Parser.hs`
- Modify: `Parser.hs` imports; `Cabal-fields.cabal` (add `Distribution.Fields.Parser` to exposed-modules)

- [ ] **Step 1: Move the file**

```bash
mv Cabal-syntax/src/Distribution/Fields/Parser.hs Cabal-fields/src/Distribution/Fields/Parser.hs
```

- [ ] **Step 2: Rewrite imports**

Delete `import Distribution.Compat.Prelude` and `import Prelude ()`. Keep all existing imports:
`Data.ByteString.Char8 as B8`, `Data.Functor.Identity`, `Distribution.Fields.Field`,
`Distribution.Fields.Lexer`, `Distribution.Fields.LexerMonad (...)`, `Distribution.Parsec.Position`,
the `Text.Parsec.*` imports, and the `#ifdef CABAL_PARSEC_DEBUG` text block (verbatim). Add any
not-in-scope name GHC reports per the recipe.

- [ ] **Step 3: Add `Distribution.Fields.Parser` to `exposed-modules` in `Cabal-fields.cabal`**

- [ ] **Step 4: Build**

Run: `cabal build Cabal-fields`
Expected: PASS.

- [ ] **Step 5: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Fields.Parser, drop Compat.Prelude" && jj new
```

---

### Task 7: Move `Pretty`, then verify `Cabal-fields` isolation

**Files:**
- Move: `Pretty.hs`
- Modify: `Pretty.hs` imports; `Cabal-fields.cabal` (add `Distribution.Fields.Pretty` to exposed-modules)

- [ ] **Step 1: Move the file**

```bash
mv Cabal-syntax/src/Distribution/Fields/Pretty.hs Cabal-fields/src/Distribution/Fields/Pretty.hs
```

- [ ] **Step 2: Rewrite imports**

Replace
```haskell
import Distribution.Compat.Prelude
import Distribution.Pretty (showToken)
import Prelude ()

import Distribution.Fields.Field (FieldName)
import Distribution.Utils.Generic (fromUTF8BS)
```
with
```haskell
import Distribution.Fields.Internal (showToken, fromUTF8BS)
import Distribution.Fields.Field (FieldName)
```
Keep `import qualified Data.ByteString as BS`, `import qualified Text.PrettyPrint as PP`, and
`import qualified Distribution.Fields.Parser as P`. Add any not-in-scope name GHC reports.

- [ ] **Step 3: Add `Distribution.Fields.Pretty` to `exposed-modules` in `Cabal-fields.cabal`** (all eight modules now exposed)

- [ ] **Step 4: Build the whole package**

Run: `cabal build Cabal-fields`
Expected: PASS.

- [ ] **Step 5: Prove isolation — no dependency on `Cabal-syntax`**

Run:
```bash
rg -n '^import +Distribution\.' Cabal-fields/src | rg -v 'Distribution\.Fields\.(Field|Lexer|LexerMonad|Parser|Pretty|Internal)|Distribution\.Parsec\.(Position|Warning|Source)'
```
Expected: **no output** (every `Distribution.*` import is to a module inside `Cabal-fields`). Any line
printed names a module still pointing back into `Cabal-syntax` and must be resolved before continuing.

- [ ] **Step 6: Checkpoint (user runs)**

```
jj describe -m "Cabal-fields: move Fields.Pretty; package is self-contained" && jj new
```

---

### Task 8: Wire `Cabal-syntax` to depend on `Cabal-fields`

**Files:**
- Modify: `Cabal-syntax/Cabal-syntax.cabal`

- [ ] **Step 1: Add `Cabal-fields` to `build-depends`**

In the `library` `build-depends` list, add (alphabetical position, after `bytestring`):
```cabal
    , Cabal-fields == 3.17.0.0
```

- [ ] **Step 2: Remove the moved modules from `exposed-modules`**

Delete these five lines:
```
    Distribution.Fields.Field
    Distribution.Fields.Lexer
    Distribution.Fields.LexerMonad
    Distribution.Fields.Parser
    Distribution.Fields.Pretty
```
and these three lines:
```
    Distribution.Parsec.Position
    Distribution.Parsec.Warning
    Distribution.Parsec.Source
```
Keep `Distribution.Fields`, `Distribution.Fields.ConfVar`, `Distribution.Fields.ParseResult`,
`Distribution.Parsec`, `Distribution.Parsec.Error`, and `Distribution.Parsec.FieldLineStream` in
`exposed-modules`.

- [ ] **Step 3: Add a `reexported-modules` stanza**

Immediately after the (now shorter) `exposed-modules` block, add:
```cabal
  reexported-modules:
    , Distribution.Fields.Field
    , Distribution.Fields.Lexer
    , Distribution.Fields.LexerMonad
    , Distribution.Fields.Parser
    , Distribution.Fields.Pretty
    , Distribution.Parsec.Position
    , Distribution.Parsec.Source
    , Distribution.Parsec.Warning
```

- [ ] **Step 4: Remove the now-unused `alex` build tool**

`Lexer.x` no longer lives in `Cabal-syntax`. Delete the `build-tool-depends` stanza:
```cabal
  build-tool-depends:
    -- https://github.com/haskell/alex/issues/288
    alex:alex < 3.5.4.1 || > 3.5.4.1
```

- [ ] **Step 5: Check whether `array` is still used by `Cabal-syntax`**

Run:
```bash
rg -n 'import +(qualified +)?Data\.Array' Cabal-syntax/src
```
If there is **no output**, remove the `, array >= 0.4.0.1 && < 0.6` line from `Cabal-syntax`'s
`build-depends`. If there is output, leave `array` in place.

- [ ] **Step 6: Build `Cabal-syntax`**

Run: `cabal build Cabal-syntax`
Expected: PASS. The umbrella `Distribution.Fields`, `ParseResult`, `ConfVar`, `Error`, and
`FieldLineStream` resolve their imports of the moved modules through the `Cabal-fields` dependency;
the `reexported-modules` make the moved modules importable from `Cabal-syntax`.

- [ ] **Step 7: Checkpoint (user runs)**

```
jj describe -m "Cabal-syntax: depend on Cabal-fields and re-export moved modules" && jj new
```

---

### Task 9: Full-tree build and test

**Files:** none (verification only)

- [ ] **Step 1: Build the dependent packages**

Run: `cabal build Cabal cabal-install`
Expected: PASS — no downstream source changes were needed (re-exports preserve every import path).

- [ ] **Step 2: Run the parser test suite**

Run: `cabal test Cabal-tests`
Expected: PASS (the parser/pretty round-trip tests exercise the moved code via `Cabal-syntax`
re-exports; behaviour is unchanged because this is a pure code move).

- [ ] **Step 3: Final isolation re-check**

Run:
```bash
rg -n '^import +Distribution\.' Cabal-fields/src | rg -v 'Distribution\.Fields\.(Field|Lexer|LexerMonad|Parser|Pretty|Internal)|Distribution\.Parsec\.(Position|Warning|Source)'
```
Expected: no output.

- [ ] **Step 4: Final checkpoint (user runs)**

```
jj describe -m "Cabal-fields extraction: full build and tests green" && jj new
```

---

## Notes / out of scope

- Removing the `#ifdef CABAL_PARSEC_DEBUG` debug code (and confirming `text` could then be dropped
  if the UTF8 helper were also removed) is deliberately **not** done here.
- Migrating `Cabal`, `cabal-install`, `Cabal-tests`, etc. to import directly from `Cabal-fields`
  (instead of via the `Cabal-syntax` re-exports) is a separate, optional follow-up.
- `ParseResult`, `ConfVar`, the `Distribution.Fields` umbrella, `Distribution.Parsec.Error`,
  `Distribution.Parsec.FieldLineStream`, and the `Parsec` class intentionally remain in
  `Cabal-syntax` because they depend on `Version`.
