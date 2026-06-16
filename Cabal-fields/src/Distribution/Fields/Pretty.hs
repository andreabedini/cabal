{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

-- | Pretty-printing of cabal-like files.
--
-- 'PrettyField' is an intermediate representation (parallel to the parser's
-- 'P.Field') whose values are already-rendered 'PP.Doc's. Build one from a parsed
-- @['P.Field' ann]@ with 'fromParsecFields' (or 'genericFromParsecFields' for
-- custom rendering), then turn it into text with 'showFields'. See
-- @Cabal-fields/GRAMMAR.md@ for the underlying grammar.
--
-- @since 3.0.0.0
module Distribution.Fields.Pretty
  ( -- * Fields
    CommentPosition (..)
  , PrettyField (..)
  , showFields
  , showFields'

    -- * Transformation from 'P.Field'
  , fromParsecFields
  , genericFromParsecFields
  , prettyFieldLines
  , prettySectionArgs
  ) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Distribution.Fields.Internal (showToken, fromUTF8BS)
import Distribution.Fields.Field (FieldName)

import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString as BS
import qualified Text.PrettyPrint as PP

-- $setup
-- >>> :set -XOverloadedStrings

-- | Whether a comment block attached to a field/section should be emitted
--   before or after it (or that there is no comment). Used as the annotation
--   interpretation by 'showFields'.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

-- | A field or section ready for pretty-printing: like the parser's 'P.Field',
-- but with field values and section arguments already rendered to 'PP.Doc's.
data PrettyField ann
  = -- | A field: annotation, name, and the rendered value document.
    PrettyField ann FieldName PP.Doc
  | -- | A section: annotation, name, rendered arguments, and body.
    PrettySection ann FieldName [PP.Doc] [PrettyField ann]
  | -- | Nothing to print (rendered as no output); useful as a neutral element.
    PrettyEmpty
  deriving (Functor, Foldable, Traversable)

-- | Pretty-print a list of 'PrettyField's to text, using 4-space indentation.
-- The first argument maps each annotation to an optional comment block.
--
-- Round-tripping a parsed field through 'fromParsecFields' and back:
--
-- >>> fmap (showFields (const NoComment) . fromParsecFields) (P.readFields "name:foo\n")
-- Right "name: foo\n"
--
-- Note: the comment function should return 'String's without newlines, already
-- prefixed (with @--@) to count as comments. This unsafety is left in place so
-- one could generate empty lines between comment lines.
showFields :: (ann -> CommentPosition) -> [PrettyField ann] -> String
showFields rann = showFields' rann (const id) 4

-- | 'showFields' with user specified indentation.
showFields'
  :: (ann -> CommentPosition)
  -- ^ Convert an annotation to lined to precede the field or section.
  -> (ann -> [String] -> [String])
  -- ^ Post-process non-annotation produced lines.
  -> Int
  -- ^ Indentation level.
  -> [PrettyField ann]
  -- ^ Fields/sections to show.
  -> String
showFields' rann post n = unlines . renderFields (Opts rann indent post)
  where
    -- few hardcoded, "unrolled"  variants.
    indent
      | n == 4 = indent4
      | n == 2 = indent2
      | otherwise = (replicate (max n 1) ' ' ++)

    indent4 :: String -> String
    indent4 [] = []
    indent4 xs = ' ' : ' ' : ' ' : ' ' : xs

    indent2 :: String -> String
    indent2 [] = []
    indent2 xs = ' ' : ' ' : xs

data Opts ann = Opts
  { _optAnnotation :: ann -> CommentPosition
  , _optIndent :: String -> String
  , _optPostprocess :: ann -> [String] -> [String]
  }

renderFields :: Opts ann -> [PrettyField ann] -> [String]
renderFields opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields
    blocks =
      filter (not . null . _contentsBlock) $ -- empty blocks cause extra newlines #8236
        map (renderField opts len) fields

    maxNameLength !acc [] = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection{} : rest) = maxNameLength acc rest
    maxNameLength !acc (PrettyEmpty : rest) = maxNameLength acc rest

-- | Block of lines with flags for optional blank lines before and after
data Block = Block
  { _beforeBlock :: Margin
  , _afterBlock :: Margin
  , _contentsBlock :: [String]
  }

data Margin = Margin | NoMargin
  deriving (Eq)

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
  NoMargin <> NoMargin = NoMargin
  _ <> _ = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0
  where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks
      where
        ins
          | surr' <> before == Margin = ("" :)
          | otherwise = id

renderField :: Opts ann -> Int -> PrettyField ann -> Block
renderField (Opts rann indent post) fw (PrettyField ann name doc) =
  Block before after content
  where
    content = case comments of
      CommentBefore cs -> cs ++ post ann lines'
      CommentAfter cs -> post ann lines' ++ cs
      NoComment -> post ann lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter [] -> NoMargin
      NoComment -> NoMargin
      _ -> Margin

    (lines', after) = case lines narrow of
      [] -> ([name' ++ ":"], NoMargin)
      [singleLine]
        | length singleLine < 60 ->
            ([name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow], NoMargin)
      _ -> ((name' ++ ":") : map indent (lines (PP.render doc)), Margin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style{PP.lineLength = PP.lineLength PP.style - fw}
renderField opts@(Opts rann indent post) _ (PrettySection ann name args fields) =
  Block Margin Margin $
    attachComments
      (post ann [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args])
      ++ map indent (renderFields opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter cs -> content ++ cs
      NoComment -> content
renderField _ _ PrettyEmpty = Block NoMargin NoMargin mempty

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
  :: Applicative f
  => (FieldName -> [P.FieldLine ann] -> f PP.Doc)
  -- ^ transform field contents
  -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])
  -- ^ transform section arguments
  -> [P.Field ann]
  -> f [PrettyField ann]
genericFromParsecFields f g = goMany
  where
    goMany = traverse go

    go (P.Field (P.Name ann name) fls) = PrettyField ann name <$> f name fls
    go (P.Section (P.Name ann name) secargs fs) = PrettySection ann name <$> g name secargs <*> goMany fs

-- | Used in 'fromParsecFields'.
prettyFieldLines :: FieldName -> [P.FieldLine ann] -> PP.Doc
prettyFieldLines _ fls =
  PP.vcat
    [ PP.text $ fromUTF8BS bs
    | P.FieldLine _ bs <- fls
    ]

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = map $ \case
  P.SecArgName _ bs -> showToken $ fromUTF8BS bs
  P.SecArgStr _ bs -> showToken $ fromUTF8BS bs
  P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [PrettyField ann]
fromParsecFields =
  runIdentity
    . genericFromParsecFields
      (Identity .: prettyFieldLines)
      (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)
