{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Fields.Parser
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Fields.Parser
  ( -- * Types
    Field (..),
    Name (..),
    FieldLine (..),
    SectionArg (..),

    -- * Grammar and parsing
    -- $grammar
    readFields,
    readFields',
    readFieldsT,
  )
where

import Control.Monad.Trans.Writer.CPS
import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity
import Distribution.Compat.Prelude
import Distribution.Fields.Field
import Distribution.Fields.Lexer
import Distribution.Fields.LexerMonad
  ( Lex,
    LexState (..),
    LexWarning (..),
    LexWarningType (..),
    runLexer,
  )
import Distribution.Parsec.Position (Position (..), positionCol)
import Text.Parsec.Combinator hiding (eof, notFollowedBy)
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (many, (<|>))
import Prelude ()

-- $setup
-- >>> import Data.Either (isLeft)

data LexStream = LexStream !LexState (LToken, LexStream)

unfoldStream :: Lex LToken -> LexState -> LexStream
unfoldStream lexer s =
  case runLexer lexer s of
    (tok, s') -> LexStream s (tok, unfoldStream lexer s')

mkLexStream :: LexState -> LexStream
mkLexStream = unfoldStream lexToken

-- type Parser a = forall m. Monad m => ParsecT LexStream () m a
-- type Parser a = ParsecT LexStream () Identity a

instance Stream LexStream Identity LToken where
  uncons (LexStream _ (tok, stream)) =
    case tok of
      L _ _ EOF -> return Nothing
      -- FIXME: DEBUG: uncomment these lines to skip new tokens and restore old lexer behaviour
      -- L _ (Whitespace _) -> uncons stream
      -- L _ (Comment _) -> uncons stream
      -- L _ (TokSkip _) -> uncons stream
      -- FIXME: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      _ -> return (Just (tok, stream))

data AToken = AToken Int String Position Token deriving (Show)

type Logger = Writer [AToken]

type Parser a = ParsecT LexStream () Logger a

tellTok :: (Monad m) => LToken -> WriterT [AToken] m ()
tellTok (L c p t) = tell [AToken c (code2text c) p t]

code2text :: (IsString a) => Int -> a
code2text 0 = "start"
code2text st | st == bol_section = "bol_section"
code2text st | st == bol_field_braces = "bol_field_braces"
code2text st | st == bol_field_layout = "bol_field_layout"
code2text st | st == in_section = "in_section"
code2text st | st == in_field_braces = "in_field_braces"
code2text st | st == in_field_layout = "in_field_layout"
code2text n = error $ "code unknown: " ++ show n

instance Stream LexStream Logger LToken where
  uncons :: LexStream -> Logger (Maybe (LToken, LexStream))
  uncons (LexStream _ (tok, stream)) = do
    tellTok tok
    case tok of
      L _ _ EOF -> return Nothing
      L _ _ (Comment _) -> uncons stream
      L _ _ TokBom -> uncons stream
      L _ _ (TokSkip _) -> uncons stream
      L _ _ _ -> return $ Just (tok, stream)

-- | Get lexer warnings accumulated so far
getLexerWarnings :: Parser [LexWarning]
getLexerWarnings = do
  LexStream (LexState {warnings = ws}) _ <- getInput
  return ws

addLexerWarning :: LexWarning -> Parser ()
addLexerWarning w = do
  LexStream ls@LexState {warnings = ws} _ <- getInput
  setInput $! mkLexStream ls {warnings = w : ws}

modifyInput :: (Monad m) => (a -> a) -> ParsecT a u m ()
modifyInput f = getInput >>= setInput . f

-- | Set Alex code i.e. the mode "state" lexer is in.
setLexerMode :: Int -> Parser ()
setLexerMode code = do
  modifyInput $ \(LexStream s _) ->
    unfoldStream lexToken s {curCode = code}

getToken :: (Token -> Maybe a) -> Parser a
getToken getTok = getTokenWithPos (\(L _ _ t) -> getTok t)

getTokenWithPos :: (LToken -> Maybe a) -> Parser a
getTokenWithPos getTok = tokenPrim (\(L _ _ t) -> describeToken t) updatePos getTok
  where
    updatePos :: SourcePos -> LToken -> LexStream -> SourcePos
    updatePos pos (L _ (Position col line) _) _ = newPos (sourceName pos) col line

describeToken :: Token -> String
describeToken t = case t of
  TokSym s -> "symbol " ++ show s
  TokStr s -> "string " ++ show s
  TokOther s -> "operator " ++ show s
  Indent _ -> "new line"
  TokFieldLine _ -> "field content"
  Colon -> "\":\""
  OpenBrace -> "\"{\""
  CloseBrace -> "\"}\""
  -- SemiColon       -> "\";\""
  Comment s -> "comment " ++ show s
  EOF -> "end of file"
  LexicalError is -> "character in input " ++ show (B8.head is)
  TokSkip n -> "skip " ++ show n ++ " characters"
  TokBom -> "BOM"

tokSym :: Parser (Name Position)
tokSym = getTokenWithPos $ \case L _ pos (TokSym x) -> Just (mkName pos x); _ -> Nothing

tokSym' :: Parser (SectionArg Position)
tokSym' = getTokenWithPos $ \case L _ pos (TokSym x) -> Just (SecArgName pos x); _ -> Nothing

tokStr :: Parser (SectionArg Position)
tokStr = getTokenWithPos $ \case L _ pos (TokStr x) -> Just (SecArgStr pos x); _ -> Nothing

tokOther :: Parser (SectionArg Position)
tokOther = getTokenWithPos $ \case L _ pos (TokOther x) -> Just (SecArgOther pos x); _ -> Nothing

tokIndent :: Parser Int
tokIndent = getToken $ \case Indent x -> Just x; _ -> Nothing

tokColon :: Parser ()
tokColon = getToken $ \case Colon -> Just (); _ -> Nothing

tokOpenBrace :: Parser Position
tokOpenBrace = getTokenWithPos $ \case L _ pos OpenBrace -> Just pos; _ -> Nothing

tokCloseBrace :: Parser ()
tokCloseBrace = getToken $ \case CloseBrace -> Just (); _ -> Nothing

tokFieldLine :: Parser (FieldLine Position)
tokFieldLine = getTokenWithPos $ \case L _ pos (TokFieldLine s) -> Just (FieldLine pos s); _ -> Nothing

-- tokComment :: Parser B8.ByteString
-- tokComment = getToken (\case Comment s -> Just s; _ -> Nothing) *> tokWhitespace

colon, openBrace, closeBrace :: Parser ()
sectionArg :: Parser (SectionArg Position)
sectionArg = tokSym' <|> tokStr <|> tokOther <?> "section parameter"

fieldSecName :: Parser (Name Position)
fieldSecName = tokSym <?> "field or section name"

colon = tokColon <?> "\":\""

openBrace = do
  pos <- tokOpenBrace <?> "\"{\""
  addLexerWarning (LexWarning LexBraces pos)

closeBrace = tokCloseBrace <?> "\"}\""

fieldContent :: Parser (FieldLine Position)
fieldContent = tokFieldLine <?> "field contents"

newtype IndentLevel = IndentLevel Int

zeroIndentLevel :: IndentLevel
zeroIndentLevel = IndentLevel 0

incIndentLevel :: IndentLevel -> IndentLevel
incIndentLevel (IndentLevel i) = IndentLevel (succ i)

indentOfAtLeast :: IndentLevel -> Parser IndentLevel
indentOfAtLeast (IndentLevel i) = try $ do
  j <- tokIndent
  guard (j >= i) <?> "indentation of at least " ++ show i
  return (IndentLevel j)

newtype LexerMode = LexerMode Int

inLexerMode :: LexerMode -> Parser p -> Parser p
inLexerMode (LexerMode mode) p =
  do setLexerMode mode; x <- p; setLexerMode in_section; return x

-----------------------
-- Cabal file grammar
--

-- $grammar
--
-- @
-- CabalStyleFile ::= SecElems
--
-- SecElems       ::= SecElem* '\\n'?
-- SecElem        ::= '\\n' SecElemLayout | SecElemBraces
-- SecElemLayout  ::= FieldLayout | FieldBraces | SectionLayout | SectionBraces
-- SecElemBraces  ::= FieldInline | FieldBraces |                 SectionBraces
-- FieldLayout    ::= name ':' line? ('\\n' line)*
-- FieldBraces    ::= name ':' '\\n'? '{' content '}'
-- FieldInline    ::= name ':' content
-- SectionLayout  ::= name arg* SecElems
-- SectionBraces  ::= name arg* '\\n'? '{' SecElems '}'
-- @
--
-- and the same thing but left factored...
--
-- @
-- SecElems              ::= SecElem*
-- SecElem               ::= '\\n' name SecElemLayout
--                         |      name SecElemBraces
-- SecElemLayout         ::= ':'   FieldLayoutOrBraces
--                         | arg*  SectionLayoutOrBraces
-- FieldLayoutOrBraces   ::= '\\n'? '{' content '}'
--                         | line? ('\\n' line)*
-- SectionLayoutOrBraces ::= '\\n'? '{' SecElems '\\n'? '}'
--                         | SecElems
-- SecElemBraces         ::= ':' FieldInlineOrBraces
--                         | arg* '\\n'? '{' SecElems '\\n'? '}'
-- FieldInlineOrBraces   ::= '\\n'? '{' content '}'
--                         | content
-- @
--
-- Note how we have several productions with the sequence:
--
-- > '\\n'? '{'
--
-- That is, an optional newline (and indent) followed by a @{@ token.
-- In the @SectionLayoutOrBraces@ case you can see that this makes it
-- not fully left factored (because @SecElems@ can start with a @\\n@).
-- Fully left factoring here would be ugly, and though we could use a
-- lookahead of two tokens to resolve the alternatives, we can't
-- conveniently use Parsec's 'try' here to get a lookahead of only two.
-- So instead we deal with this case in the lexer by making a line
-- where the first non-space is @{@ lex as just the @{@ token, without
-- the usual indent token. Then in the parser we can resolve everything
-- with just one token of lookahead and so without using 'try'.

-- Top level of a file using cabal syntax
--
cabalStyleFile :: Parser [Field Position]
cabalStyleFile = do
  es <- elements zeroIndentLevel
  eof
  return es

-- Elements that live at the top level or inside a section, i.e. fields
-- and sections content
--
-- elements ::= element*
elements :: IndentLevel -> Parser [Field Position]
elements ilevel = many (element ilevel)

-- An individual element, ie a field or a section. These can either use
-- layout style or braces style. For layout style then it must start on
-- a line on its own (so that we know its indentation level).
--
-- element ::= '\\n' name elementInLayoutContext
--           |      name elementInNonLayoutContext
element :: IndentLevel -> Parser (Field Position)
element ilevel =
  ( do
      ilevel' <- indentOfAtLeast ilevel
      name <- fieldSecName
      elementInLayoutContext (incIndentLevel ilevel') name
  )
    <|> ( do
            name <- fieldSecName
            elementInNonLayoutContext name
        )

-- An element (field or section) that is valid in a layout context.
-- In a layout context we can have fields and sections that themselves
-- either use layout style or that use braces style.
--
-- elementInLayoutContext ::= ':'  fieldLayoutOrBraces
--                          | arg* sectionLayoutOrBraces
elementInLayoutContext :: IndentLevel -> Name Position -> Parser (Field Position)
elementInLayoutContext ilevel name =
  (do colon; fieldLayoutOrBraces ilevel name)
    <|> ( do
            args <- many sectionArg
            elems <- sectionLayoutOrBraces ilevel
            return (Section name args elems)
        )

-- An element (field or section) that is valid in a non-layout context.
-- In a non-layout context we can have only have fields and sections that
-- themselves use braces style, or inline style fields.
--
-- elementInNonLayoutContext ::= ':' FieldInlineOrBraces
--                             | arg* '\\n'? '{' elements '\\n'? '}'
elementInNonLayoutContext :: Name Position -> Parser (Field Position)
elementInNonLayoutContext name =
  (do colon; fieldInlineOrBraces name)
    <|> ( do
            args <- many sectionArg
            openBrace
            elems <- elements zeroIndentLevel
            optional tokIndent
            closeBrace
            return (Section name args elems)
        )

-- The body of a field, using either layout style or braces style.
--
-- fieldLayoutOrBraces   ::= '\\n'? '{' content '}'
--                         | line? ('\\n' line)*
fieldLayoutOrBraces :: IndentLevel -> Name Position -> Parser (Field Position)
fieldLayoutOrBraces ilevel name = braces <|> fieldLayout
  where
    braces = do
      openBrace
      ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
      closeBrace
      return (Field name ls)
    fieldLayout = inLexerMode (LexerMode in_field_layout) $ do
      l <- optionMaybe fieldContent
      ls <- many (do _ <- indentOfAtLeast ilevel; fieldContent)
      return $ case l of
        Nothing -> Field name ls
        Just l' -> Field name (l' : ls)

-- The body of a section, using either layout style or braces style.
--
-- sectionLayoutOrBraces ::= '\\n'? '{' elements \\n? '}'
--                         | elements
sectionLayoutOrBraces :: IndentLevel -> Parser [Field Position]
sectionLayoutOrBraces ilevel =
  ( do
      openBrace
      elems <- elements zeroIndentLevel
      optional tokIndent
      closeBrace
      return elems
  )
    <|> (elements ilevel)

-- The body of a field, using either inline style or braces.
--
-- fieldInlineOrBraces   ::= '\\n'? '{' content '}'
--                         | content
fieldInlineOrBraces :: Name Position -> Parser (Field Position)
fieldInlineOrBraces name =
  ( do
      openBrace
      ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
      closeBrace
      return (Field name ls)
  )
    <|> ( do
            ls <- inLexerMode (LexerMode in_field_braces) (option [] (fmap (\l -> [l]) fieldContent))
            return (Field name ls)
        )

-- | Parse cabal style 'B8.ByteString' into list of 'Field's, i.e. the cabal AST.
--
-- 'readFields' assumes that input 'B8.ByteString' is valid UTF8, specifically it doesn't validate that file is valid UTF8.
-- Therefore bytestrings inside returned 'Field' will be invalid as UTF8 if the input were.
--
-- >>> readFields "foo: \223"
-- Right [Field (Name (Position 1 1) "foo") [FieldLine (Position 1 6) "\223"]]
--
-- 'readFields' won't (necessarily) fail on invalid UTF8 data, but the reported positions may be off.
--
-- __You may get weird errors on non-UTF8 input__, for example 'readFields' will fail on latin1 encoded non-breaking space:
--
-- >>> isLeft (readFields "\xa0 foo: bar")
-- True
--
-- That is rejected because parser thinks @\\xa0@ is a section name,
-- and section arguments may not contain colon.
-- If there are just latin1 non-breaking spaces, they become part of the name:
--
-- >>> readFields "\xa0\&foo: bar"
-- Right [Field (Name (Position 1 1) "\160foo") [FieldLine (Position 1 7) "bar"]]
--
-- The UTF8 non-breaking space is accepted as an indentation character (but warned about by 'readFields'').
--
-- >>> readFields' "\xc2\xa0 foo: bar"
-- Right ([Field (Name (Position 1 3) "foo") [FieldLine (Position 1 8) "bar"]],[LexWarning LexWarningNBSP (Position 1 1)])
readFields :: B8.ByteString -> Either ParseError [Field Position]
readFields s = fmap fst (readFields' s)

-- -- | Like 'readFields' but also return lexer warnings.
readFields' :: B8.ByteString -> Either ParseError ([Field Position], [LexWarning])
readFields' s = fst (readFieldsT s)

-- readFields' s = do
--   parse parser "the input" lexSt
--   where
--     parser = do
--       fields <- cabalStyleFile
--       ws <- getLexerWarnings -- lexer accumulates warnings in reverse (consing them to the list)
--       pure (fields, reverse ws ++ checkIndentation fields [])
--
--     lexSt = mkLexStream (mkLexState s)

readFieldsT :: B8.ByteString -> (Either ParseError ([Field Position], [LexWarning]), [AToken])
readFieldsT s = do
  runWriter $ runParserT parser () "the input" lexSt
  where
    parser :: ParsecT LexStream () Logger ([Field Position], [LexWarning])
    parser = do
      fields <- cabalStyleFile
      ws <- getLexerWarnings -- lexer accumulates warnings in reverse (consing them to the list)
      pure (fields, reverse ws ++ checkIndentation fields [])

    lexSt = mkLexStream (mkLexState s)

-- | Check (recursively) that all fields inside a block are indented the same.
--
-- We have to do this as a post-processing check.
-- As the parser uses indentOfAtLeast approach, we don't know what is the "correct"
-- indentation for following fields.
--
-- To catch during parsing we would need to parse first field/section of a section
-- and then parse the following ones (softly) requiring the exactly the same indentation.
checkIndentation :: [Field Position] -> [LexWarning] -> [LexWarning]
checkIndentation [] = id
checkIndentation (Field name _ : fs') = checkIndentation' (nameAnn name) fs'
checkIndentation (Section name _ fs : fs') = checkIndentation fs . checkIndentation' (nameAnn name) fs'

-- | We compare adjacent fields to reduce the amount of reported indentation warnings.
checkIndentation' :: Position -> [Field Position] -> [LexWarning] -> [LexWarning]
checkIndentation' _ [] = id
checkIndentation' pos (Field name _ : fs') = checkIndentation'' pos (nameAnn name) . checkIndentation' (nameAnn name) fs'
checkIndentation' pos (Section name _ fs : fs') = checkIndentation'' pos (nameAnn name) . checkIndentation fs . checkIndentation' (nameAnn name) fs'

-- | Check that positions' columns are the same.
checkIndentation'' :: Position -> Position -> [LexWarning] -> [LexWarning]
checkIndentation'' a b
  | positionCol a == positionCol b = id
  | otherwise = (LexWarning LexInconsistentIndentation b :)

eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of file"
  where
    notFollowedBy :: Parser LToken -> Parser ()
    notFollowedBy p =
      try
        ( (do L _ _ t <- try p; unexpected (describeToken t))
            <|> return ()
        )
