# Cabal File Grammar Reference

This document describes the grammar of the **cabal file format** — the lexical and
syntactic structure shared by `.cabal` package descriptions and `cabal.project` files — in
enough detail to reimplement a compatible parser from scratch. The reference implementation is
the `Cabal-fields` package (`Cabal-fields/src/Distribution/Fields/`).

The format is parsed in two layers:

* This document covers the **field layer**: turning bytes into a list of *fields* and
  *sections* (`[Field Position]`). That is the whole of `Cabal-fields`, and the function
  `readFields` is its entry point.
* It does **not** cover the *value* layer — how the contents of a particular field (a version
  range, a dependency list, a condition in an `if`) are interpreted. That is the job of higher
  layers in `Cabal-syntax` (field grammars, the `Parsec` type class, `ConfVar`). The
  conditional-expression grammar is summarised in an appendix for orientation only.

Sections 1–4 are **implementation-independent**: they specify the language. Sections 5–6
describe **how `Cabal-fields` implements it** (Alex + Parsec) and the design choices involved.

---

## Table of Contents

1. [Overview](#overview)
2. [Abstract syntax tree](#abstract-syntax-tree)
3. [Lexical grammar](#lexical-grammar)
   - [Encoding and bytes](#encoding-and-bytes)
   - [Character classes](#character-classes)
   - [Token patterns](#token-patterns)
   - [Lexer modes](#lexer-modes)
   - [Tokenisation rules by mode](#tokenisation-rules-by-mode)
   - [Token types](#token-types)
   - [Position tracking](#position-tracking)
   - [Warnings](#warnings)
4. [Syntactic grammar](#syntactic-grammar)
   - [Conceptual grammar](#conceptual-grammar)
   - [Left-factored grammar](#left-factored-grammar)
   - [The `{` disambiguation rule](#the--disambiguation-rule)
   - [Indentation rules](#indentation-rules)
5. [Implementation: lexer](#implementation-lexer)
6. [Implementation: parser](#implementation-parser)
7. [Entry points](#entry-points)
8. [Worked examples](#worked-examples)
9. [Notes, limitations, and known quirks](#notes-limitations-and-known-quirks)
10. [Appendix: conditional-expression grammar (Cabal-syntax layer)](#appendix-conditional-expression-grammar-cabal-syntax-layer)

---

## Overview

Parsing proceeds in two tightly-coupled stages that run interleaved:

1. **Lexing** — a byte-oriented lexer operating in several *modes* produces a stream of located
   tokens (`LToken`). Indentation is **significant and is not discarded**: at the start of each
   logical line the lexer emits an `Indent n` token carrying the column (character offset) of
   the first non-space character. Comments and blank lines are skipped.

2. **Parsing** — a parser consumes the token stream and builds `[Field Position]`. Crucially the
   parser **drives the lexer's mode**: when it is about to read a field value it switches the
   lexer into a mode where the rest of the value is returned as opaque text lines rather than
   structured tokens, then switches back.

The grammar is **layout-sensitive**, like Python or Haskell: indentation determines the extent
of a section's body and of a multi-line field value. A brace syntax (`{ }`) is also supported as
an explicit, layout-insensitive alternative and the two may be mixed.

---

## Abstract syntax tree

Source: `Cabal-fields/src/Distribution/Fields/Field.hs`.

```
CabalFile      ::=  [Field ann]

Field ann      ::=  Field   (Name ann) [FieldLine ann]          -- a "name: value" field
                 |  Section (Name ann) [SectionArg ann] [Field ann]  -- a "name args { body }" section

FieldLine ann  ::=  FieldLine ann ByteString
                    -- one physical line of a field value.
                    -- Invariant: the ByteString contains no newline.

SectionArg ann ::=  SecArgName  ann ByteString   -- identifier or dotted number (e.g. 7.6.3)
                 |  SecArgStr   ann ByteString   -- quoted string, quotes stripped
                 |  SecArgOther ann ByteString   -- operators, parentheses, etc.

Name ann       ::=  Name ann ByteString
                    -- Invariant: the ByteString is lower-case ASCII.
```

The type parameter `ann` is an annotation; `readFields` instantiates it to `Position` (a
1-indexed `row`/`column` pair, `Distribution.Fields.Position`).

Key points a reimplementation must reproduce:

* **Field names are normalised to lower case** on construction (`mkName` maps ASCII
  upper-case to lower-case). Section names are also `Name`s and so are likewise lower-cased.
* A `Field`'s value is a *list of lines*; the parser stores each physical line separately and
  does **not** join them, strip further whitespace, or interpret escapes.
* A quoted-string section argument (`SecArgStr`) has its surrounding quotes removed by the
  lexer, but escape sequences inside it are **not** interpreted.
* Annotations are positions; the position of a `Field`/`Section` is the position of its `Name`.

---

## Lexical grammar

### Encoding and bytes

The lexer is **byte-oriented**. Input is assumed to be UTF-8 but this is **not validated**;
bytes are passed through into the resulting `ByteString`s unchanged. Two consequences:

* The lexer never fails merely because the input is not valid UTF-8; field/section payloads may
  themselves be invalid UTF-8 if the input was.
* Position **columns are counted in bytes**, except for one compensation: the two-byte UTF-8
  non-breaking space (`U+00A0` = `0xC2 0xA0`) is counted as a single column when it occurs in
  leading whitespace. For other multi-byte characters, reported columns can be too large. This
  is a documented limitation, not a contract.

### Character classes

```
space          = U+0020 (a single space)
ctlchar        = [U+0000 .. U+001F] ∪ {U+007F}
printable      = [U+0000 .. U+00FF] \ ctlchar          -- excludes control chars, so no \n or \r
symbol'        = [ , = < > + * & | ! $ % ^ @ # ? / \ ~ ]
symbol         = symbol' ∪ { - . }                     -- symbol' plus hyphen and dot
spacetab       = { space, \t }
paren          = [ ( ) [ ] ]
namecore       = printable \ ( { space, :, " , {, } } ∪ paren ∪ symbol' )
instr          = (printable ∪ { space }) \ { " }       -- characters allowed inside a quoted string
instresc       = printable                             -- any printable, after a backslash in a string
comment        = printable ∪ { \t }

field_layout'  = printable \ { space }                 -- first char of a layout field line
field_layout   = printable ∪ { \t }                    -- subsequent chars of a layout field line
field_braces'  = printable \ ( { space, {, } } )       -- first char of a braced field line
field_braces   = (printable ∪ { \t }) \ { {, } }       -- subsequent chars of a braced field line
```

The most important consequence: identifiers/names (`namecore+`) may contain `-` and `.` (these
are in `symbol` but not `symbol'`), but **not** `:`, `"`, `{`, `}`, parentheses, brackets, or any
of the operator characters in `symbol'`.

### Token patterns

```
bom          = 0xEF 0xBB 0xBF                 -- UTF-8 byte-order mark
nbsp         = 0xC2 0xA0                       -- UTF-8 non-breaking space
nbspspacetab = spacetab | nbsp
nl           = "\n" | "\r\n" | "\r"
name         = namecore+
string       = '"' ( instr | '\' instresc )* '"'
oplike       = symbol+
```

### Lexer modes

The lexer is a state machine with **seven modes** (Alex "start codes"). The initial mode is the
BOM-handling mode; it immediately hands off to *beginning-of-line, section context*. Modes come
in *beginning-of-line* (`bol_*`) and *mid-line* (`in_*`) pairs:

| Mode               | Context                              | Entered when                         |
|--------------------|--------------------------------------|--------------------------------------|
| *(initial)*        | start of file, optional BOM          | at start; transitions to `bol_section` |
| `bol_section`      | start of a line, section context     | after any newline in section context |
| `in_section`       | mid-line, section context            | after consuming a line's indent      |
| `bol_field_layout` | start of a line, layout field value  | after a newline while reading a layout field value |
| `in_field_layout`  | mid-line, layout field value         | parser switches in to read a field value |
| `bol_field_braces` | start of a line, braced field value  | after a newline while reading a braced field value |
| `in_field_braces`  | mid-line, braced field value         | parser switches in to read a braced field value |

A reimplementation does not need exactly these seven states, but it must reproduce their
*observable behaviour*: indentation tokens at line starts in section context, opaque line
tokens inside field values, and comment/blank-line skipping in every beginning-of-line context.

The **parser** only ever selects `in_field_layout` and `in_field_braces` (to read values), and
always restores `in_section` afterward. The `bol_*` modes are entered automatically by the lexer
on each newline; the parser never names them.

### Tokenisation rules by mode

Listed in priority order within each mode (Alex uses longest-match, then first-rule-wins).

**Initial mode**

| Input         | Action |
|---------------|--------|
| `bom?`        | If a BOM is present, emit a `BOM` warning and reset the position as if it were absent. Switch to `bol_section` and continue. |

**`bol_section`, `bol_field_layout`, `bol_field_braces` (shared)**

| Input                       | Action |
|-----------------------------|--------|
| `nbspspacetab* nl`          | Blank line (possibly indented, possibly with NBSP): emit nothing, advance to next line. |
| `spacetab* "--" comment*`   | A comment line (note: no trailing `nl` in the pattern, so a comment on the final line without a newline still matches): emit nothing. |

**`bol_section` (in addition to the shared rules)**

| Input             | Action |
|-------------------|--------|
| `nbspspacetab*`   | Leading whitespace at the start of a content line. Let `n` be its width in **characters** (each NBSP counts as 1). If the remaining input is empty, emit `EOF`. Otherwise emit `Indent n` and switch to `in_section`. |
| `spacetab* "{"`   | An open brace whose only predecessors on the line are spaces/tabs: emit `OpenBrace` **with no preceding `Indent`**. (See [the `{` disambiguation rule](#the--disambiguation-rule).) |
| `spacetab* "}"`   | Likewise for a close brace: emit `CloseBrace` with no preceding `Indent`. |

**`in_section`**

| Input            | Action |
|------------------|--------|
| `spacetab+`      | Inter-token whitespace: skip. |
| `"--" comment*`  | Trailing/inline comment: skip. |
| `name`           | Emit `TokSym` with the matched bytes. |
| `string`         | Emit `TokStr` with the bytes **between** the quotes (escapes not interpreted). |
| `oplike`         | Emit `TokOther` with the matched operator bytes. |
| `paren`          | Emit `TokOther` with the single paren/bracket. |
| `":"`            | Emit `Colon`. |
| `"{"`            | Emit `OpenBrace`. |
| `"}"`            | Emit `CloseBrace`. |
| `nl`             | Newline: advance to next line and switch to `bol_section`. |

**`bol_field_layout`** — only the shared blank-line/comment rules, plus:

| Input            | Action |
|------------------|--------|
| `nbspspacetab*`  | As in `bol_section`: empty remaining input → `EOF`; otherwise emit `Indent n` and switch to `in_field_layout`. (No brace handling here.) |

**`in_field_layout`** — a field value, layout style

| Input                           | Action |
|---------------------------------|--------|
| `spacetab+`                     | Leading whitespace on the value line: skip. |
| `field_layout' field_layout*`   | The remainder of the line (a non-empty run not starting with a space): emit `TokFieldLine`. |
| `nl`                            | Newline: advance and switch to `bol_field_layout`. |

**`bol_field_braces`**

| Input | Action |
|-------|--------|
| `()`  | Matches the empty string immediately: switch to `in_field_braces` and continue. (Indentation is irrelevant inside braces.) |

**`in_field_braces`** — a field value, braces style

| Input                           | Action |
|---------------------------------|--------|
| `spacetab+`                     | Whitespace: skip. |
| `field_braces' field_braces*`   | A non-empty run not containing `{` or `}`: emit `TokFieldLine`. |
| `"{"`                           | Emit `OpenBrace`. |
| `"}"`                           | Emit `CloseBrace`. |
| `nl`                            | Newline: advance and switch to `bol_field_braces`. |

If, in any mode, no rule matches, the lexer emits a `LexicalError` token carrying the remaining
input and stops.

### Token types

```
Token  ::=  TokSym       ByteString   -- identifier / name / number  (name pattern)
         |  TokStr       ByteString   -- quoted-string content, quotes stripped
         |  TokOther     ByteString   -- operator (oplike) or a paren/bracket
         |  Indent       Int          -- start of a content line; Int = indent width in characters
         |  TokFieldLine ByteString   -- one line of a field value (no newline)
         |  Colon                     -- ':'
         |  OpenBrace                 -- '{'
         |  CloseBrace                -- '}'
         |  EOF
         |  LexicalError ByteString   -- unrecognised input; carries the rest of the stream

LToken ::=  L Position Token          -- a token with its source position
```

### Position tracking

Positions are 1-indexed `(row, column)` pairs starting at `(1, 1)`. The lexer advances the
column by the **byte** length of each match and increments the row (resetting the column to 1)
on each newline. The single correction is for leading NBSP runs, whose character width is used
for the emitted `Indent` and to fix up the column. See
[limitations](#notes-limitations-and-known-quirks) for the non-ASCII caveat.

### Warnings

The lexer (and one post-parse pass) accumulate non-fatal warnings:

| Warning                       | Produced when |
|-------------------------------|---------------|
| `LexWarningBOM`               | a UTF-8 BOM appears at the start of the file |
| `LexWarningNBSP`              | a non-breaking space (`0xC2 0xA0`) is used as whitespace |
| `LexWarningTab`               | a tab is used in leading whitespace |
| `LexBraces`                   | brace syntax (`{`/`}`) is used to delimit a field/section |
| `LexInconsistentIndentation` | (post-parse) sibling fields/sections in one block start at different columns |

Warnings are reported in source order. Multiple occurrences of one warning type are coalesced
into a single warning listing all positions. `LexBraces` is recorded but **not** surfaced as a
user-visible warning (it is dropped when converting to public warnings).

---

## Syntactic grammar

In the productions below the terminals are tokens:

* `'\n'`  = an `Indent` token (a newline followed by indentation);
* `name`  = a `TokSym` token;
* `arg`   = a section argument: `TokSym`, `TokStr`, or `TokOther`;
* `line`  = a single `TokFieldLine`;
* `content` = zero or more `TokFieldLine`;
* `'{'`, `'}'`, `':'` = the corresponding tokens.

### Conceptual grammar

This is the intended language, before factoring for a one-token-lookahead parser:

```
CabalStyleFile ::= SecElems

SecElems       ::= SecElem* '\n'?
SecElem        ::= '\n' SecElemLayout | SecElemBraces
SecElemLayout  ::= FieldLayout | FieldBraces | SectionLayout | SectionBraces
SecElemBraces  ::= FieldInline | FieldBraces |                 SectionBraces
FieldLayout    ::= name ':' line? ('\n' line)*
FieldBraces    ::= name ':' '\n'? '{' content '}'
FieldInline    ::= name ':' content
SectionLayout  ::= name arg* SecElems
SectionBraces  ::= name arg* '\n'? '{' SecElems '}'
```

### Left-factored grammar

This is the form the reference parser actually implements (one token of lookahead, no
backtracking across alternatives except where noted):

```
SecElems              ::= SecElem*

SecElem               ::= '\n' name SecElemLayout
                        |      name SecElemBraces

SecElemLayout         ::= ':'   FieldLayoutOrBraces
                        | arg*  SectionLayoutOrBraces

SecElemBraces         ::= ':'   FieldInlineOrBraces
                        | arg*  '\n'? '{' SecElems '\n'? '}'

FieldLayoutOrBraces   ::= '\n'? '{' content '}'
                        | line? ('\n' line)*

FieldInlineOrBraces   ::= '\n'? '{' content '}'
                        | content

SectionLayoutOrBraces ::= '\n'? '{' SecElems '\n'? '}'
                        | SecElems
```

The distinction between the two top-level alternatives of `SecElem` is layout: an element that
begins on its own line (preceded by an `Indent`) is in **layout context** and may itself use
layout or braces; an element that begins **without** a leading `Indent` (i.e. immediately after
`{` or after a sibling on the same line) is in **non-layout context** and its sub-structure must
use braces or inline form.

`content` in a field context is a run of `TokFieldLine` lexed in braces mode; `line` is a single
`TokFieldLine` lexed in layout mode (see [lexer modes](#lexer-modes)).

### The `{` disambiguation rule

Several productions contain `'\n'? '{'` — an optional newline/indent followed by an open brace.
`SectionLayoutOrBraces` is the awkward one: its other alternative, `SecElems`, can itself begin
with `'\n'`, so the choice is not decidable with a single token of lookahead by naive factoring.

The format resolves this **in the lexer, not the parser**: a line whose first non-space
character is `{` (or `}`) is lexed as just the `OpenBrace` (resp. `CloseBrace`) token, with **no
preceding `Indent`**. Therefore the parser, at each `'\n'? '{'` decision point, sees exactly one
of:

* an `Indent` — the next element really does start on a fresh line (no brace), or
* an `OpenBrace`/`CloseBrace` — a brace follows (even if it was alone on its own line).

Every `'\n'? '{'` thus collapses to just `'{'` at the token level, and the parser needs only one
token of lookahead and no `try`. A `{` or `}` appearing **mid-line** (after other tokens in
`in_section`) is lexed normally as a brace token.

A reimplementer who tokenises differently must preserve this observable property, or must
otherwise resolve the two-token ambiguity (e.g. with bounded backtracking).

### Indentation rules

The parser threads a current **indentation level** `ilevel` (a character count).

1. **Element start.** An element in layout context must begin with `Indent n` where `n >= ilevel`.
2. **Children.** After an element whose own line was indented to `j`, its children (a section's
   body, or a field value's continuation lines) require indent `>= j + 1`.
3. **Braces reset.** Inside `{ … }` the level resets to `0`, so braced content may sit at any
   column.
4. **Consistency (post-parse).** All siblings in one block must share the *same* column;
   differences are reported as `LexInconsistentIndentation`. This is checked after parsing
   because the level rule only enforces a *minimum*, not equality.

| Context                                  | Minimum indent for next element |
|------------------------------------------|---------------------------------|
| top level                                | `>= 0`                          |
| inside a section whose header is at `j`  | `>= j + 1`                      |
| inside `{ }` (any depth)                 | `>= 0` (reset)                  |
| layout field-value continuation lines    | `>= j + 1` (`j` = field header indent) |
| field value inside `{ }`                 | no requirement                  |

---

## Implementation: lexer

Source: `Cabal-fields/src/Distribution/Fields/Lexer.x` (an Alex specification) and
`Cabal-fields/src/Distribution/Fields/LexerMonad.hs`.

* **Alex, `%encoding "latin1"`.** Declaring latin1 makes Alex treat the input as a stream of raw
  bytes, which is why the lexer is byte-oriented and UTF-8-agnostic.
* **`Lex` monad.** A minimal hand-written state monad (`newtype Lex a = Lex (LexState -> LexResult a)`)
  threading `LexState { curPos, curInput, curCode, warnings }`. It is not built on `mtl`/
  `transformers`. `curCode` is the active Alex start code; the parser changes it to switch modes.
* **Token actions.** Each Alex rule's action is `Position -> Int -> ByteString -> Lex LToken`
  (position, byte length, matched input). Helpers `tok`/`toki` build simple tokens;
  `checkLeadingWhitespace`/`checkWhitespace` compute character-width indents and raise the
  tab/NBSP warnings.
* **`lexToken`** runs one step of `alexScan`, advances the position by the matched byte length,
  updates the input, and dispatches the matched action (skipping `AlexSkip` matches such as
  whitespace/comments). `AlexEOF`/`AlexError` produce `EOF`/`LexicalError`.
* **`checkPosition`** is a vestigial no-op (`checkPosition _ _ _ _ = return ()`). It was the hook
  for an optional, never-enabled debug cross-check of byte-vs-character positions; the debug code
  has been removed, leaving the no-op so the call site in `lexToken` is unchanged.
* **Entry points for the parser.** `mkLexState` builds the initial state (position `1:1`, start
  code `0`, no warnings); `lexToken` produces the next token; `setStartCode`/`getStartCode` and
  the exported mode constants `bol_section`, `in_section`, `in_field_layout`, `in_field_braces`
  let the parser drive the mode.

## Implementation: parser

Source: `Cabal-fields/src/Distribution/Fields/Parser.hs`, built on Parsec.

* **Tokens as a Parsec `Stream`.** `LexState'` (note the prime) is a lazy, co-inductive stream
  wrapping the lexer's `LexState`:

  ```haskell
  data LexState' = LexState' !LexState (LToken, LexState')
  ```

  Each node pairs the lexer state with *the next token and the rest of the stream*, computed
  lazily by running `lexToken`. The `Stream` instance's `uncons` yields tokens until it sees
  `EOF` (which it reports as end-of-stream). The parser type is
  `type Parser a = ParsecT LexState' () Identity a`.

* **Mode switching is the crux.** Because the stream is lazy, the parser can change the lexer
  mode mid-parse and have it take effect on the *next* token:

  ```haskell
  setLexerMode code = do
    LexState' ls _ <- getInput
    setInput $! mkLexState' ls{curCode = code}   -- discard the precomputed next token,
                                                  -- rebuild the stream with the new start code
  ```

  `inLexerMode mode p` sets `mode`, runs `p`, then restores `in_section`. It is used to read
  field values: `in_field_layout` for indented multi-line values, `in_field_braces` for braced
  values (and for inline values). Lexer warnings are likewise stored in the threaded `LexState`,
  so the parser reads/updates them via `getInput`/`setInput` (`getLexerWarnings`,
  `addLexerWarning`).

* **Grammar functions** map one-to-one onto the [left-factored grammar](#left-factored-grammar):
  `cabalStyleFile` → `elements` → `element` → `elementInLayoutContext` /
  `elementInNonLayoutContext` → `fieldLayoutOrBraces` / `fieldInlineOrBraces` /
  `sectionLayoutOrBraces`. `indentOfAtLeast` wraps the `Indent`-token consumption with the
  `>= ilevel` guard inside a `try` (so a too-shallow indent does not consume the token).
  `openBrace` additionally records a `LexBraces` warning.

* **Post-parse indentation check.** `checkIndentation` walks the resulting tree and compares each
  block's sibling columns, appending `LexInconsistentIndentation` warnings. It is a separate pass
  because `indentOfAtLeast` only enforces a minimum, never equality.

**Why this architecture.** Keeping indentation as explicit `Indent` tokens (rather than handling
layout purely in the lexer, à la Haskell's layout algorithm) lets the *parser* decide what a
given indent means in context. Letting the parser drive the lexer mode keeps field values
completely opaque to the structured tokeniser — the lexer does not need to know what any field
means — while still sharing one position-tracking pass. The lazy-stream `Stream` instance is what
makes parser-driven mode switching expressible within Parsec.

---

## Entry points

```haskell
-- Parse cabal-style bytes into the field AST. Input is assumed (not checked) to be UTF-8.
readFields  :: ByteString -> Either ParseError [Field Position]

-- As readFields, but also return the accumulated lexer warnings.
readFields' :: ByteString -> Either ParseError ([Field Position], [LexWarning])
```

Both are in `Distribution.Fields.Parser`. `readFields'`:

1. wraps the input in a `LexState` (start code `0`, position `1:1`);
2. builds the lazy `LexState'` token stream;
3. runs Parsec's `parse` with the `cabalStyleFile` grammar;
4. returns warnings in source order — lexer warnings (accumulated in reverse, then reversed)
   followed by the post-parse indentation warnings.

`ParseError` here is Parsec's own error type, not Cabal's higher-level `ParseResult`.

---

## Worked examples

### Simple field

Input:

```
name: my-package
```

Token stream:

```
L (1:1)  (Indent 0)
L (1:1)  (TokSym "name")
L (1:5)  Colon
  -- parser switches to in_field_layout --
L (1:7)  (TokFieldLine "my-package")
L (2:1)  EOF
```

Result: `Field (Name (1:1) "name") [FieldLine (1:7) "my-package"]`.

### Multi-line field (layout style)

Input:

```
build-depends:
    base >= 4,
    containers
```

Token stream:

```
L (1:1)  (Indent 0)
L (1:1)  (TokSym "build-depends")
L (1:14) Colon
  -- in_field_layout; first value line is empty → optional fieldContent = Nothing --
L (2:1)  (Indent 4)
L (2:5)  (TokFieldLine "base >= 4,")
L (3:1)  (Indent 4)
L (3:5)  (TokFieldLine "containers")
L (4:1)  EOF
```

Result:

```
Field (Name (1:1) "build-depends")
  [ FieldLine (2:5) "base >= 4,"
  , FieldLine (3:5) "containers" ]
```

### Section with indented children

Input:

```
library
  exposed-modules: Foo
  build-depends:   base
```

Result:

```
Section (Name (1:1) "library") []
  [ Field (Name (2:3) "exposed-modules") [FieldLine (2:20) "Foo"]
  , Field (Name (3:3) "build-depends")   [FieldLine (3:20) "base"] ]
```

### Brace-delimited section (mid-line `{`)

Input:

```
library { exposed-modules: Foo }
```

Token stream (note `OpenBrace` is mid-line, lexed in `in_section`; the elements inside braces get
**no** leading `Indent`; and a braced field line keeps interior/trailing spaces up to the brace):

```
L (1:1)  (Indent 0)
L (1:1)  (TokSym "library")
L (1:9)  OpenBrace
L (1:11) (TokSym "exposed-modules")
L (1:26) Colon
  -- in_field_braces --
L (1:28) (TokFieldLine "Foo ")
L (1:32) CloseBrace
L (2:1)  EOF
```

Result: `Section (Name (1:1) "library") [] [Field (Name (1:11) "exposed-modules") [FieldLine (1:28) "Foo "]]`
— note the field line is `"Foo "` (trailing space before `}` is retained).

### Brace at start of line (disambiguated)

Input:

```
library
{
  exposed-modules: Foo
}
```

Line 2's `{` matches the `bol_section` rule `spacetab* "{"`, so it is emitted as `OpenBrace`
**without** a preceding `Indent`. The parser therefore sees `OpenBrace` directly after the
`library` header and takes the braces branch of `SectionLayoutOrBraces`. The closing `}` on its
own line is handled the same way (`spacetab* "}"` → `CloseBrace`, no `Indent`).

---

## Notes, limitations, and known quirks

* **Byte vs. character columns.** Columns advance by bytes. Leading NBSP runs are corrected to
  character width, but other multi-byte UTF-8 characters can make reported columns too large.
  The source notes this (`FIXME: we want len_chars here really` in `lexToken`).
* **No UTF-8 validation.** `readFields` does not reject invalid UTF-8; payload bytes pass through
  verbatim. As the `readFields` doctests show, *latin1* (non-UTF-8) NBSP bytes are **not** treated
  as whitespace — a leading `0xA0` is lexed as part of a name, which can yield surprising errors
  (e.g. a section name followed by content with a colon). Only the UTF-8 NBSP `0xC2 0xA0` is
  recognised as whitespace (and warned about).
* **String escapes are not interpreted.** `TokStr`/`SecArgStr` carry the bytes between the quotes
  as-is, including any backslash escapes.
* **Tabs** are accepted as leading whitespace but warned about (`LexWarningTab`); a source `TODO`
  notes that disallowing tabs as leading space is desirable.
* **`checkPosition` is a no-op** in the current code (its debug body was removed); it performs no
  validation.
* **Dead remnant:** `describeToken` contains a commented-out `SemiColon` case; there is no
  semicolon token in the grammar.
* **Grammar-comment nuance:** the *conceptual* grammar's `SectionBraces` omits the `'\n'?` before
  the closing `}` that both the *left-factored* grammar and the implementation
  (`optional tokIndent` before `closeBrace`) allow. The left-factored form is the accurate one.
  In practice the lexer's `spacetab* "}"` rule already strips any `Indent` before a `}` at the
  start of a line, so this optional indent is rarely exercised.

---

## Appendix: conditional-expression grammar (Cabal-syntax layer)

This grammar is **not** part of `Cabal-fields`. Once `Cabal-fields` has produced
`[Field Position]`, a higher layer interprets the `SectionArg`s of an `if`/`elif` section as a
boolean condition. The reference for this lives in `Cabal-syntax`
(`Distribution.Fields.ConfVar`), which runs a second Parsec pass over the `[SectionArg Position]`
list. It is summarised here only for orientation:

```
Condition  ::= CondOr
CondOr     ::= CondAnd ('||' CondAnd)*
CondAnd    ::= Cond    ('&&' Cond)*
Cond       ::= 'true' | 'false'                       -- case-insensitive
             | '(' CondOr ')'
             | '!' Cond
             | 'os'   '(' OsName ')'
             | 'arch' '(' ArchName ')'
             | 'flag' '(' FlagName ')'
             | 'impl' '(' CompilerName VersionRange? ')'
```

Operator precedence, tightest to loosest: unary `!` > `&&` > `||`. The leaf names (`OsName`,
`FlagName`, version ranges, …) are parsed with the `Parsec` type-class parsers of the wider Cabal
library, again over the opaque bytes that `Cabal-fields` produced. See `Cabal-syntax` for the
authoritative definition.
