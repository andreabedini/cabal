{
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Fields.Lexer
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Lexer for the cabal files.
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Distribution.Fields.Lexer
  ( ltest, lexToken, Token(..), LToken(..)
  , bol_section, in_section
  , bol_field_layout, in_field_layout
  , bol_field_braces, in_field_braces
  ,mkLexState) where

import Prelude ()
import qualified Prelude
import Distribution.Compat.Prelude

import Distribution.Fields.LexerMonad
import Distribution.Parsec.Position (Position (..), incPos, retPos)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B.Char8
import qualified Data.Word as Word
}

-- Various character classes

%encoding "latin1"

$space           = \          -- single space char
$ctlchar         = [\x0-\x1f \x7f]
$printable       = \x0-\xff # $ctlchar   -- so no \n \r
$symbol'         = [ \, \= \< \> \+ \* \& \| \! \$ \% \^ \@ \# \? \/ \\ \~ ]
$symbol          = [$symbol' \- \.]
$spacetab        = [$space \t]

$paren           = [ \( \) \[ \] ]
$field_layout    = [$printable \t]
$field_layout'   = [$printable] # [$space]
$field_braces    = [$printable \t] # [\{ \}]
$field_braces'   = [$printable] # [\{ \} $space]
$comment         = [$printable \t]
$namecore        = [$printable] # [$space \: \" \{ \} $paren $symbol']
$instr           = [$printable $space] # [\"]
$instresc        = $printable

@bom          = \xef \xbb \xbf
@nbsp         = \xc2 \xa0
@nbspspacetab = ($spacetab | @nbsp)
@nbspspace    = ($space | @nbsp)
@nl           = \n | \r\n | \r
@name         = $namecore+
@string       = \" ( $instr | \\ $instresc )* \"
@oplike       = $symbol+


tokens :-

<0> {
  @bom?  { \st pos len _ ->
            case len of
              0 -> do
                setPos pos -- reset position as if BOM didn't exist
                setStartCode bol_section
                lexToken
              _ -> do
                addWarningAt pos LexWarningBOM
                setStartCode bol_section
                return (L st pos TokBom)
         }
}

<bol_section, bol_field_layout, bol_field_braces> {
  @nbspspacetab* @nl         { \st pos len inp -> do
                                 _ <- checkWhitespace pos len inp
                                 adjustPos retPos
                                 toki TokSkip st pos len inp
                             }

  -- no @nl here to allow for comments on last line of the file with no trailing \n
  -- TODO: check the lack of @nl works here including counting line numbers
  $spacetab* "--" $comment*  { toki Comment }
}

<bol_section> {
  @nbspspacetab*   { \st pos len inp -> do
                       len' <- checkLeadingWhitespace pos len inp
                       -- len' is character whitespace length (counting nbsp as one)
                       if B.length inp == len
                         then return (L st pos EOF)
                         else do
                           -- Small hack: if char and byte length mismatch
                           -- subtract the difference, so lexToken will count position correctly.
                           -- Proper (and slower) fix is to count utf8 length in lexToken
                           when (len' /= len) $ adjustPos (incPos (len' - len))
                           setStartCode in_section
                           return (L st pos (Indent len'))
                   }

  -- FIXME: this whitespace needs to be captured
  -- NOTE: Commenting this out kinda still works but it adds an indent token before the brace.
  $spacetab* \{   { tok OpenBrace }
  $spacetab* \}   { tok CloseBrace }
}

<in_section> {
  $spacetab+     ; --TODO: don't allow tab as leading space

  "--" $comment* { toki Comment }

  @name          { toki TokSym }
                 -- NOTE: this is to remove the quotes surrounding the string
  @string        { \st pos len inp -> return $! L st pos (TokStr (B.take (len - 2) (B.tail inp))) }
  @oplike        { toki TokOther }
  $paren         { toki TokOther }
  \:             { tok  Colon }
  \{             { tok  OpenBrace }
  \}             { tok  CloseBrace }
  @nl            { \_ _ _ _ -> do
                     adjustPos retPos
                     setStartCode bol_section
                     lexToken
                 }
}

<bol_field_layout> {
  @nbspspacetab* { \st pos len inp -> do
                     len' <- checkLeadingWhitespace pos len inp
                     if B.length inp == len
                       then return (L st pos EOF)
                       else do
                         -- Small hack: if char and byte length mismatch
                         -- subtract the difference, so lexToken will count position correctly.
                         -- Proper (and slower) fix is to count utf8 length in lexToken
                         when (len' /= len) $ adjustPos (incPos (len' - len))
                         setStartCode in_field_layout
                         return (L st pos (Indent len'))
                 }
}

<in_field_layout> {
  $spacetab+                     ;
  $field_layout' $field_layout*  { toki TokFieldLine }
  @nl                            { \_ _ _ _ -> do
                                     adjustPos retPos
                                     setStartCode bol_field_layout
                                     lexToken
                                 }
}

<bol_field_braces> {
   ()                            { \_ _ _ _ -> do
                                     setStartCode in_field_braces
                                     lexToken
                                 }
}

<in_field_braces> {
  $spacetab+                     ;
  $field_braces' $field_braces*  { toki TokFieldLine }
  \{                             { tok OpenBrace  }
  \}                             { tok CloseBrace }
  @nl                            { \_ _ _ _ -> do
                                     adjustPos retPos
                                     setStartCode bol_field_braces
                                     lexToken
                                 }
}

{

-- | Tokens of outer cabal file structure. Field values are treated opaquely.
data Token = TokSym       !ByteString   -- ^ Haskell-like identifier, number or operator
           | TokStr       !ByteString   -- ^ String in quotes
           | TokOther     !ByteString   -- ^ Operators and parens
           | Indent       !Int          -- ^ Indentation token
           | TokFieldLine !ByteString   -- ^ Lines after @:@
           | TokSkip      !ByteString   -- ^ Skip some characters
           | TokBom
           | Colon
           | OpenBrace
           | CloseBrace
           | Comment      !ByteString
           | EOF
           | LexicalError InputStream --TODO: add separate string lexical error
  deriving Show

data LToken = L !StartCode !Position !Token
  deriving Show

toki :: (ByteString -> Token) -> StartCode -> Position -> Int -> ByteString -> Lex LToken
toki t st pos len input = return $! L st pos (t (B.take len input))

tok :: Token -> StartCode -> Position -> Int -> ByteString -> Lex LToken
tok t st pos _len _input = return $! L st pos t

checkLeadingWhitespace :: Position -> Int -> ByteString -> Lex Int
checkLeadingWhitespace pos len bs
    | B.any (== 9) (B.take len bs) = do
        addWarningAt pos LexWarningTab
        checkWhitespace pos len bs
    | otherwise = checkWhitespace pos len bs

checkWhitespace :: Position -> Int -> ByteString -> Lex Int
checkWhitespace pos len bs
    -- UTF8 NBSP is 194 160. This function is called on whitespace bytestrings,
    -- therefore counting 194 bytes is enough to count non-breaking spaces.
    -- We subtract the amount of 194 bytes to convert bytes length into char length
    | B.any (== 194) (B.take len bs) = do
        addWarningAt pos LexWarningNBSP
        return $ len - B.count 194 (B.take len bs)
    | otherwise = return len

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = InputStream

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetByte :: AlexInput -> Maybe (Word.Word8, AlexInput)
alexGetByte = B.uncons

lexicalError :: StartCode -> Position -> InputStream -> Lex LToken
lexicalError st pos inp = do
  setInput B.empty
  return $! L st pos (LexicalError inp)

lexToken :: Lex LToken
lexToken = do
  pos <- getPos
  inp <- getInput
  st  <- getStartCode
  case alexScan inp st of
    AlexEOF ->
        return (L st pos EOF)
    AlexError inp' ->
        let !len_bytes = B.length inp - B.length inp' in
            -- FIXME: we want len_chars here really
            -- need to decode utf8 up to this point
        lexicalError st (incPos len_bytes pos) inp'
    AlexSkip inp' len_chars -> do
        adjustPos (incPos len_chars)
        setInput inp'
        let !len_bytes = B.length inp - B.length inp'
        toki TokSkip st pos len_bytes inp
    AlexToken inp' len_chars action -> do
        adjustPos (incPos len_chars)
        setInput inp'
        let !len_bytes = B.length inp - B.length inp'
        t <- action st pos len_bytes inp
        return t

lexAll :: Lex [LToken]
lexAll = do
  t <- lexToken
  case t of
    L _ _ EOF -> return [t]
    _         -> do ts <- lexAll
                    return (t : ts)

ltest :: Int -> String -> Prelude.IO ()
ltest code s =
  let (ws, xs) = execLexer (setStartCode code >> lexAll) (B.Char8.pack s)
   in traverse_ print ws >> traverse_ print xs


mkLexState :: ByteString -> LexState
mkLexState input = LexState
  { curPos   = Position 1 1
  , curInput = input
  , curCode  = 0
  , warnings = []
  }

}
