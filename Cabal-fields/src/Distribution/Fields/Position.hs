{-# LANGUAGE DeriveGeneric #-}

-- | Source positions used as the annotation on parsed 'Distribution.Fields.Field.Field's.
--
-- Positions are 1-indexed @(row, column)@ pairs. The lexer counts columns in
-- bytes (with a correction for leading non-breaking spaces), so for non-ASCII
-- input columns can be approximate — see @Cabal-fields/GRAMMAR.md@.
module Distribution.Fields.Position
  ( Position (..)
  , incPos
  , retPos
  , showPos
  , zeroPos
  , positionCol
  , positionRow
  ) where

import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | A 1-indexed @(row, column)@ position in a file. The first character of a
-- file is at @Position 1 1@.
data Position
  = Position
      {-# UNPACK #-} !Int -- row
      {-# UNPACK #-} !Int -- column
  deriving (Eq, Ord, Show, Generic)

instance Binary Position
instance NFData Position

-- | Shift a position @n@ columns to the right (same row).
--
-- >>> incPos 3 (Position 1 1)
-- Position 1 4
incPos :: Int -> Position -> Position
incPos n (Position row col) = Position row (col + n)

-- | Move a position to the beginning (column 1) of the next row.
--
-- >>> retPos (Position 4 9)
-- Position 5 1
retPos :: Position -> Position
retPos (Position row _col) = Position (row + 1) 1

-- | Render a position as @\"row:column\"@.
--
-- >>> showPos (Position 12 34)
-- "12:34"
showPos :: Position -> String
showPos (Position row col) = show row ++ ":" ++ show col

-- | The origin @Position 0 0@ (used as a placeholder where no real position is
-- available; note it is /not/ a valid 1-indexed source position).
zeroPos :: Position
zeroPos = Position 0 0

-- | The column of a position.
--
-- >>> positionCol (Position 3 7)
-- 7
--
-- @since 3.0.0.0
positionCol :: Position -> Int
positionCol (Position _ c) = c

-- | The row of a position.
--
-- >>> positionRow (Position 3 7)
-- 3
--
-- @since 3.0.0.0
positionRow :: Position -> Int
positionRow (Position r _) = r
