{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Non-fatal parser\/lexer warnings, their classification ('PWarnType'), and
-- helpers to render them. See @Cabal-fields/GRAMMAR.md@ for which inputs trigger
-- each warning; note that 'readFields'' returns lexer warnings as
-- 'Distribution.Fields.LexerMonad.LexWarning's, which are converted to these
-- 'PWarning's by 'Distribution.Fields.LexerMonad.toPWarnings'.
module Distribution.Fields.Warning
  ( PWarning (..)
  , PWarningWithSource (..)
  , PSource (..)
  , showPSourceAsFilePath
  , PWarnType (..)
  , showPWarning
  , showPWarningWithSource
  ) where

import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Distribution.Fields.Position
import Distribution.Fields.Source
import System.FilePath (normalise)

-- | Type of parser warning. We do classify warnings.
--
-- Different application may decide not to show some, or have fatal behaviour on others
data PWarnType
  = -- | Unclassified warning
    PWTOther
  | -- | Invalid UTF encoding
    PWTUTF
  | -- | @true@ or @false@, not @True@ or @False@
    PWTBoolCase
  | -- | there are version with tags
    PWTVersionTag
  | -- | New syntax used, but no @cabal-version: >= 1.2@ specified
    PWTNewSyntax
  | -- | Old syntax used, and @cabal-version >= 1.2@ specified
    PWTOldSyntax
  | PWTDeprecatedField
  | PWTInvalidSubsection
  | PWTUnknownField
  | PWTUnknownSection
  | PWTTrailingFields
  | -- | extra main-is field
    PWTExtraMainIs
  | -- | extra test-module field
    PWTExtraTestModule
  | -- | extra benchmark-module field
    PWTExtraBenchmarkModule
  | PWTLexNBSP
  | PWTLexBOM
  | PWTLexTab
  | -- | legacy cabal file that we know how to patch
    PWTQuirkyCabalFile
  | -- | Double dash token, most likely it's a mistake - it's not a comment
    PWTDoubleDash
  | -- | e.g. name or version should be specified only once.
    PWTMultipleSingularField
  | -- | Workaround for derive-package having build-type: Default. See <https://github.com/haskell/cabal/issues/5020>.
    PWTBuildTypeDefault
  | -- | Version operators used (without cabal-version: 1.8)
    PWTVersionOperator
  | -- | Version wildcard used (without cabal-version: 1.6)
    PWTVersionWildcard
  | -- | Warnings about cabal-version format.
    PWTSpecVersion
  | -- | Empty filepath, i.e. literally ""
    PWTEmptyFilePath
  | -- | sections contents (sections and fields) are indented inconsistently
    PWTInconsistentIndentation
  | -- | Experimental feature
    PWTExperimental
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary PWarnType
instance NFData PWarnType

-- | A parser warning: its classification, the source position it occurred at,
-- and a human-readable message.
data PWarning = PWarning {pwarningType :: !PWarnType, pwarningPosition :: !Position, pwarningMessage :: !String}
  deriving (Eq, Ord, Show, Generic)

-- | A 'PWarning' together with the source ('PSource') it came from.
data PWarningWithSource src = PWarningWithSource {pwarningSource :: !(PSource src), pwarning :: !PWarning}
  deriving (Eq, Ord, Show, Generic, Functor)

instance Binary PWarning
instance NFData PWarning

-- | Render a warning prefixed with a (normalised) file path and its position,
-- in the conventional @path:row:col: message@ form.
--
-- >>> showPWarning "pkg.cabal" (PWarning PWTOther (Position 2 3) "oops")
-- "pkg.cabal:2:3: oops"
showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning _ pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

-- | Render a warning whose source is a 'String' file path.
--
-- >>> showPWarningWithSource (PWarningWithSource (PKnownSource "f.cabal") (PWarning PWTOther (Position 1 1) "msg"))
-- "f.cabal:1:1: msg"
showPWarningWithSource :: PWarningWithSource String -> String
showPWarningWithSource (PWarningWithSource source pwarn) =
  showPWarning (showPSourceAsFilePath source) pwarn

-- | Render a 'String'-valued 'PSource' as a file path, using @\"???\"@ for an
-- unknown source.
showPSourceAsFilePath :: PSource String -> String
showPSourceAsFilePath source =
  case source of
    PKnownSource src -> src
    PUnknownSource -> "???"
