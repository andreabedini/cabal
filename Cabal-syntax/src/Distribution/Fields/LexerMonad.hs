{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Fields.LexerMonad
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Fields.LexerMonad
  ( InputStream
  , LexState (..)
  , LexResult (..)
  , Lex (..)
  , StartCode
  , runLexer
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

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Distribution.Compat.Prelude
import Distribution.Parsec.Position (Position (..), positionRow, showPos)
import Distribution.Parsec.Warning (PWarnType (..), PWarning (..))
import Prelude ()

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

#ifdef CABAL_PARSEC_DEBUG
-- testing only:
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector        as V
#endif

-- simple state monad
newtype Lex a = Lex {unLex :: State LexState a}
  deriving (Functor, Applicative, Monad) via (State LexState)

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

data LexWarning
  = LexWarning
      !LexWarningType
      {-# UNPACK #-} !Position
  deriving (Show)

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
    -- LexBraces warning about using { } delimeters is not reported as parser warning.
    toWarning LexBraces _ =
      Nothing

data LexState = LexState
  { curPos :: {-# UNPACK #-} !Position
  -- ^ position at current input location
  , curInput :: {-# UNPACK #-} !InputStream
  -- ^ the current input
  , curCode :: {-# UNPACK #-} !StartCode
  -- ^ lexer code
  , warnings :: [LexWarning]
  }

-- TODO: check if we should cache the first token
-- since it looks like parsec's uncons can be called many times on the same input

type StartCode =
  Int
  -- ^ An @alex@ lexer start code

type InputStream = B.ByteString

runLexer :: Lex a -> LexState -> (a, LexState)
runLexer (Lex lexer) st = runState lexer st

-- | Execute the given lexer on the supplied input stream.
execLexer :: Lex a -> InputStream -> ([LexWarning], a)
execLexer (Lex lexer) input =
  case runState lexer initialState of
    (result, LexState{warnings = ws}) -> (ws, result)
  where
    initialState =
      LexState
        { -- TODO: add 'startPosition'
          curPos = Position 1 1
        , curInput = input
        , curCode = 0
        , warnings = []
        }

setPos :: Position -> Lex ()
setPos pos = Lex $ modify' $ \s -> s{curPos = pos}

getPos :: Lex Position
getPos = Lex $ gets curPos

adjustPos :: (Position -> Position) -> Lex ()
adjustPos f = Lex $ modify' $ \s@LexState{curPos = pos} -> s{curPos = f pos}

getInput :: Lex InputStream
getInput = Lex $ gets curInput

setInput :: InputStream -> Lex ()
setInput i = Lex $ modify' $ \s -> s{curInput = i}

getStartCode :: Lex Int
getStartCode = Lex $ gets curCode

setStartCode :: Int -> Lex ()
setStartCode c = Lex $ modify $ \s -> s{curCode = c}

-- | Add warning at the current position
addWarning :: LexWarningType -> Lex ()
addWarning wt = Lex $ modify $ \s@LexState{curPos = pos, warnings = ws} ->
  s{warnings = LexWarning wt pos : ws}

-- | Add warning at specific position
addWarningAt :: Position -> LexWarningType -> Lex ()
addWarningAt pos wt = Lex $ modify' $ \s@LexState{warnings = ws} ->
  s{warnings = LexWarning wt pos : ws}
