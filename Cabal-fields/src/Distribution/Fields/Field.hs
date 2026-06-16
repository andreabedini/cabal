{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Cabal-like file AST types: 'Field', 'Section' etc
--
-- These types are parameterized by an annotation (in practice a source 'Position').
-- They are the output of 'Distribution.Fields.Parser.readFields'; see
-- @Cabal-fields\/GRAMMAR.md@ for the full grammar and the invariants these types carry
-- (e.g. lower-cased names, newline-free field lines).
module Distribution.Fields.Field
  ( -- * Cabal file
    Field (..)
  , fieldName
  , fieldAnn
  , fieldUniverse
  , FieldLine (..)
  , fieldLineAnn
  , fieldLineBS
  , SectionArg (..)
  , sectionArgAnn

    -- * Name
  , FieldName
  , Name (..)
  , mkName
  , getName
  , nameAnn

    -- * Conversions to String
  , sectionArgsToString
  , fieldLinesToString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import Data.List (intercalate)
import Distribution.Fields.Internal (showTokenStr, fromUTF8BS)
#if MIN_VERSION_base(4,18,0)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Foldable1 as F1
#endif

-- $setup
-- >>> :set -XOverloadedStrings

-------------------------------------------------------------------------------
-- Cabal file
-------------------------------------------------------------------------------

-- | A single top-level element of a cabal-like file: either a /field/
-- (@foo: bar@) or a /section/ (@library ... { ... }@). A whole file is a
-- @['Field' ann]@, as returned by 'Distribution.Fields.Parser.readFields'.
--
-- The @ann@ type parameter is an annotation carried on every node. After
-- parsing it is a source 'Distribution.Fields.Position.Position'; the
-- 'Functor'\/'Foldable'\/'Traversable' instances let you map over or collect
-- those annotations.
data Field ann
  = -- | A field: its 'Name' and its value as a list of 'FieldLine's (the content
    -- after the @:@, one entry per physical line).
    Field !(Name ann) [FieldLine ann]
  | -- | A section: its 'Name', its 'SectionArg' arguments (e.g. a library name or
    -- an @if@ condition), and its body (a nested list of 'Field's).
    Section !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | @since 3.12.0.0
deriving instance Ord ann => Ord (Field ann)

-- | The 'Name' of a field or section.
fieldName :: Field ann -> Name ann
fieldName (Field n _) = n
fieldName (Section n _ _) = n

-- | The annotation of a field or section (the annotation of its 'Name').
fieldAnn :: Field ann -> ann
fieldAnn = nameAnn . fieldName

-- | All transitive descendants of a 'Field', including itself, in pre-order
-- (a node before its children).
--
-- /Note:/ the resulting list is never empty.
--
-- >>> fieldUniverse (Section (mkName () "outer") [] [Field (mkName () "inner") []])
-- [Section (Name () "outer") [] [Field (Name () "inner") []],Field (Name () "inner") []]
fieldUniverse :: Field ann -> [Field ann]
fieldUniverse f@(Section _ _ fs) = f : concatMap fieldUniverse fs
fieldUniverse f@(Field _ _) = [f]

-- | A single physical line of a field's value, with its annotation (its source
-- position) and the raw bytes of that line.
--
-- A field's value is a list of these; the parser does not join them, trim
-- further, or interpret escapes.
--
-- /Invariant:/ the 'ByteString' contains no newline characters.
data FieldLine ann = FieldLine !ann !ByteString
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | @since 3.12.0.0
deriving instance Ord ann => Ord (FieldLine ann)

-- | The annotation (source position) of a field line.
--
-- @since 3.0.0.0
fieldLineAnn :: FieldLine ann -> ann
fieldLineAnn (FieldLine ann _) = ann

-- | The raw bytes of a field line (no newline; see the 'FieldLine' invariant).
--
-- @since 3.0.0.0
fieldLineBS :: FieldLine ann -> ByteString
fieldLineBS (FieldLine _ bs) = bs

-- | A single section argument — the tokens between a section name and its body,
-- e.g. the @base >=4@ in @build-depends@ is not a section, but the @win32@ in
-- @if os(win32)@ and the library name in @library mylib@ are section arguments.
-- The three constructors record how the lexer classified the token.
data SectionArg ann
  = -- | identifier, or something which looks like number. Also many dot numbers, i.e. "7.6.3"
    SecArgName !ann !ByteString
  | -- | quoted string
    SecArgStr !ann !ByteString
  | -- | everything else, mm. operators (e.g. in if-section conditionals)
    SecArgOther !ann !ByteString
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | @since 3.12.0.0
deriving instance Ord ann => Ord (SectionArg ann)

-- | Extract annotation from 'SectionArg'.
sectionArgAnn :: SectionArg ann -> ann
sectionArgAnn (SecArgName ann _) = ann
sectionArgAnn (SecArgStr ann _) = ann
sectionArgAnn (SecArgOther ann _) = ann

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

-- | The raw bytes of a field or section name.
type FieldName = ByteString

-- | A field or section name together with its annotation.
--
-- /Invariant/: the 'FieldName' is lower-case ASCII. Construct values with
-- 'mkName', which enforces this; do not use the 'Name' constructor with
-- arbitrary-case input if you rely on the invariant.
data Name ann = Name !ann !FieldName
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | @since 3.12.0.0
deriving instance Ord ann => Ord (Name ann)

-- | Construct a 'Name', normalising the bytes to lower case (ASCII) so that
-- names compare case-insensitively.
--
-- >>> mkName () "Foo-Bar"
-- Name () "foo-bar"
mkName :: ann -> FieldName -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

-- | The (already lower-cased) bytes of a 'Name'.
--
-- >>> getName (mkName () "Camel")
-- "camel"
getName :: Name ann -> FieldName
getName (Name _ bs) = bs

-- | The annotation of a 'Name'.
nameAnn :: Name ann -> ann
nameAnn (Name ann _) = ann

-------------------------------------------------------------------------------
-- To Strings
-------------------------------------------------------------------------------

-- | Render section arguments back to a single space-separated 'String'.
-- 'SecArgName' and 'SecArgStr' are quoted if they would otherwise be ambiguous
-- (see 'Distribution.Fields.Internal.showTokenStr'); 'SecArgOther' is emitted verbatim.
-- Annotations are ignored.
--
-- >>> sectionArgsToString [SecArgName () "base", SecArgOther () ">=", SecArgName () "4"]
-- "base >= 4"
--
-- @since 3.6.0.0
sectionArgsToString :: [SectionArg ann] -> String
sectionArgsToString = unwords . map toStr
  where
    toStr :: SectionArg ann -> String
    toStr (SecArgName _ bs) = showTokenStr (fromUTF8BS bs)
    toStr (SecArgStr _ bs) = showTokenStr (fromUTF8BS bs)
    toStr (SecArgOther _ bs) = fromUTF8BS bs

-- | Join a field's 'FieldLine's into a single newline-separated 'String'.
--
-- /Note:/ this doesn't preserve original indentation or blank lines, since the
-- annotations (e.g. positions) are ignored; it simply intercalates the line
-- bytes with @\\n@.
--
-- >>> fieldLinesToString [FieldLine () "alpha", FieldLine () "beta"]
-- "alpha\nbeta"
--
-- @since 3.6.0.0
fieldLinesToString :: [FieldLine ann] -> String
fieldLinesToString =
  -- intercalate to avoid trailing newline.
  intercalate "\n" . map toStr
  where
    toStr (FieldLine _ bs) = fromUTF8BS bs

-------------------------------------------------------------------------------
-- Foldable1
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,18,0)

-- | @since 3.12.0.0
instance F1.Foldable1 Field where
  foldMap1 f (Field x ys) =
    F1.fold1 (F1.foldMap1 f x :| map (F1.foldMap1 f) ys)
  foldMap1 f (Section x ys zs) =
    F1.fold1 (F1.foldMap1 f x :| map (F1.foldMap1 f) ys ++ map (F1.foldMap1 f) zs)

-- | @since 3.12.0.0
instance F1.Foldable1 FieldLine where
  foldMap1 = (. fieldLineAnn)

-- | @since 3.12.0.0
instance F1.Foldable1 SectionArg where
  foldMap1 = (. sectionArgAnn)

-- | @since 3.12.0.0
instance F1.Foldable1 Name where
  foldMap1 = (. nameAnn)

#endif
