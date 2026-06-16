-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Fields.LexerMonad
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The lexer's state monad and warning types. See @Cabal-fields/GRAMMAR.md@ for how the
-- lexer and its warnings fit into the overall grammar.
module Distribution.Fields.LexerMonad
  ( InputStream
  , LexState (..)
  , LexResult (..)
  , Lex (..)
  , execLexer
  , getPos
  , setPos
  , adjustPos
  , getInput
  , setInput
  , getStartCode
  , setStartCode
  , LexWarning (..)
  , LexWarningType (..)
  , addWarning
  , addWarningAt
  , toPWarnings
  ) where

import Control.Monad (ap, liftM)
import qualified Data.ByteString as B
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Distribution.Fields.Position (Position (..), positionRow, showPos)
import Distribution.Fields.Warning (PWarnType (..), PWarning (..))

-- | The lexer monad: a minimal, hand-written strict state monad over 'LexState'
-- (no @mtl@\/@transformers@). Lexer actions thread the current position, input,
-- start code, and accumulated warnings.
newtype Lex a = Lex {unLex :: LexState -> LexResult a}

instance Functor Lex where
  fmap = liftM

instance Applicative Lex where
  pure = returnLex
  (<*>) = ap

instance Monad Lex where
  return = pure
  (>>=) = thenLex

-- | The result of running a lexer step: the updated state and a value.
data LexResult a = LexResult {-# UNPACK #-} !LexState a

data LexWarningType
  = -- | Encountered non breaking space
    LexWarningNBSP
  | -- | BOM at the start of the cabal file
    LexWarningBOM
  | -- | Leading tags
    LexWarningTab
  | -- | indentation decreases
    LexInconsistentIndentation
  | -- | Brace syntax used
    LexBraces
  deriving (Eq, Ord, Show)

-- | A raw lexer warning: its kind and the position it occurred at. Converted to
-- a public 'PWarning' by 'toPWarnings'.
data LexWarning
  = LexWarning
      !LexWarningType
      {-# UNPACK #-} !Position
  deriving (Show)

-- | Convert raw lexer warnings into public 'PWarning's. Warnings of the same
-- type are coalesced into a single 'PWarning' listing all positions, and
-- 'LexBraces' is dropped entirely (it is informational only). Note that
-- 'Distribution.Fields.Parser.readFields'' returns the raw 'LexWarning's, /not/
-- the result of this conversion.
toPWarnings :: [LexWarning] -> [PWarning]
toPWarnings =
  mapMaybe (uncurry toWarning)
    . Map.toList
    . Map.fromListWith (flip (<>)) -- fromListWith gives existing element first.
    . map (\(LexWarning t p) -> (t, pure p))
  where
    toWarning LexWarningBOM poss =
      Just $ PWarning PWTLexBOM (NE.head poss) "Byte-order mark found at the beginning of the file"
    toWarning LexWarningNBSP poss =
      Just $ PWarning PWTLexNBSP (NE.head poss) $ "Non breaking spaces at " ++ intercalate ", " (NE.toList $ fmap showPos poss)
    toWarning LexWarningTab poss =
      Just $ PWarning PWTLexTab (NE.head poss) $ "Tabs used as indentation at " ++ intercalate ", " (NE.toList $ fmap showPos poss)
    toWarning LexInconsistentIndentation poss =
      Just $ PWarning PWTInconsistentIndentation (NE.head poss) $ "Inconsistent indentation. Indentation jumps at lines " ++ intercalate ", " (NE.toList $ fmap (show . positionRow) poss)
    -- LexBraces warning about using { } delimiters is not reported as parser warning.
    toWarning LexBraces _ =
      Nothing

{- FOURMOLU_DISABLE -}
data LexState = LexState
  { curPos :: {-# UNPACK #-} !Position
  -- ^ position at current input location
  , curInput :: {-# UNPACK #-} !InputStream
  -- ^ the current input
  , curCode :: {-# UNPACK #-} !StartCode
  -- ^ lexer code
  , warnings :: [LexWarning]
  }
{- FOURMOLU_ENABLE -}

-- TODO: check if we should cache the first token
-- since it looks like parsec's uncons can be called many times on the same input

type StartCode =
  Int
  -- ^ An @alex@ lexer start code

-- | The lexer input: a strict 'B.ByteString' (raw bytes; see the encoding notes
-- in @Cabal-fields/GRAMMAR.md@).
type InputStream = B.ByteString

-- | Execute the given lexer on the supplied input stream.
{- FOURMOLU_DISABLE -}
execLexer :: Lex a -> InputStream -> ([LexWarning], a)
execLexer (Lex lexer) input =
  case lexer initialState of
    LexResult LexState{warnings = ws} result -> (ws, result)
  where
    initialState =
      LexState
        { -- TODO: add 'startPosition'
          curPos = Position 1 1
        , curInput = input
        , curCode = 0
        , warnings = []
        }
{- FOURMOLU_ENABLE -}

{-# INLINE returnLex #-}
returnLex :: a -> Lex a
returnLex a = Lex $ \s -> LexResult s a

{-# INLINE thenLex #-}
thenLex :: Lex a -> (a -> Lex b) -> Lex b
(Lex m) `thenLex` k = Lex $ \s -> case m s of LexResult s' a -> unLex (k a) s'

-- | Set the current source position.
setPos :: Position -> Lex ()
setPos pos = Lex $ \s -> LexResult s{curPos = pos} ()

-- | Get the current source position.
getPos :: Lex Position
getPos = Lex $ \s@LexState{curPos = pos} -> LexResult s pos

-- | Modify the current source position with a function.
adjustPos :: (Position -> Position) -> Lex ()
adjustPos f = Lex $ \s@LexState{curPos = pos} -> LexResult s{curPos = f pos} ()

-- | Get the remaining (unconsumed) input.
getInput :: Lex InputStream
getInput = Lex $ \s@LexState{curInput = i} -> LexResult s i

-- | Replace the remaining input.
setInput :: InputStream -> Lex ()
setInput i = Lex $ \s -> LexResult s{curInput = i} ()

-- | Get the current Alex start code (lexer mode).
getStartCode :: Lex Int
getStartCode = Lex $ \s@LexState{curCode = c} -> LexResult s c

-- | Set the Alex start code (lexer mode). The parser uses this to switch the
-- lexer between section and field-value tokenisation.
setStartCode :: Int -> Lex ()
setStartCode c = Lex $ \s -> LexResult s{curCode = c} ()

-- | Add warning at the current position
addWarning :: LexWarningType -> Lex ()
addWarning wt = Lex $ \s@LexState{curPos = pos, warnings = ws} ->
  LexResult s{warnings = LexWarning wt pos : ws} ()

-- | Add warning at specific position
addWarningAt :: Position -> LexWarningType -> Lex ()
addWarningAt pos wt = Lex $ \s@LexState{warnings = ws} ->
  LexResult s{warnings = LexWarning wt pos : ws} ()
