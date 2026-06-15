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
