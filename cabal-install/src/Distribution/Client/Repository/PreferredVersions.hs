{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Repository.PreferredVersions where

import Data.ByteString.Lazy (ByteString)
import Data.Either (rights)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import System.FilePath (takeFileName)
import Type.Reflection (Typeable)

import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Utils (fromUTF8LBS)
import Distribution.Types.Dependency (Dependency)

--
-- Filename and parsers for 'preferred-versions' file.
--

-- | Expected name of the 'preferred-versions' file.
--
-- Contains special constraints, such as a preferred version of a package
-- or deprecations of certain package versions.
--
-- Expected format:
--
-- @
-- binary > 0.9.0.0 || < 0.9.0.0
-- text == 1.2.1.0
-- @
preferredVersions :: FilePath
preferredVersions = "preferred-versions"

-- | Does the given filename match with the expected name of 'preferred-versions'?
isPreferredVersions :: FilePath -> Bool
isPreferredVersions = (== preferredVersions) . takeFileName

-- | Parse `preferred-versions` file, ignoring any parse failures.
--
-- To obtain parse errors, use 'parsePreferredVersionsWarnings'.
parsePreferredVersions :: ByteString -> [Dependency]
parsePreferredVersions = rights . parsePreferredVersionsWarnings

-- | Parser error of the `preferred-versions` file.
data PreferredVersionsParseError = PreferredVersionsParseError
  { preferredVersionsParsecError :: String
  -- ^ Parser error to show to a user.
  , preferredVersionsOriginalDependency :: String
  -- ^ Original input that produced the parser error.
  }
  deriving (Generic, Read, Show, Eq, Ord, Typeable)

-- | Parse `preferred-versions` file, collecting parse errors that can be shown
-- in error messages.
parsePreferredVersionsWarnings
  :: ByteString
  -> [Either PreferredVersionsParseError Dependency]
parsePreferredVersionsWarnings =
  map parsePreference
    . filter (not . isPrefixOf "--")
    . lines
    . fromUTF8LBS
  where
    parsePreference :: String -> Either PreferredVersionsParseError Dependency
    parsePreference s = case eitherParsec s of
      Left err ->
        Left $
          PreferredVersionsParseError
            { preferredVersionsParsecError = err
            , preferredVersionsOriginalDependency = s
            }
      Right dep -> Right dep
