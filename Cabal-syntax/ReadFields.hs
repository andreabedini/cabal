{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- FIXME either bytes or text

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (for_)
import Distribution.Fields (Field (..), FieldLine (..), Name (..))
import Distribution.Fields.Parser (readFieldsT)
import Distribution.Pretty
import System.Environment
import Text.PrettyPrint

main = do
  args <- getArgs
  for_ args $ \fn -> do
    bs <- B8.readFile fn
    let (res, tokens) = readFieldsT bs
    case res of
      Left e -> do
        putStrLn "--- ERROR ---"
        print e
      Right (fs, warnings) -> do
        putStrLn "--- FIELDS ---"
        print (vcat $ map pretty fs)
        unless (null warnings) $ do
          putStrLn "\n--- WARNINGS ---"
          print (vcat $ map (text . show) warnings)

    putStrLn "\n--- TOKENS ---"
    print (vcat $ map (text . show) tokens)

instance Show ann => Pretty (FieldLine ann) where
  pretty :: FieldLine ann -> Doc
  pretty (FieldLine ann line) =
    text "FieldLine"
      <+> parens (text (show ann))
      <+> text (show line)

instance Show ann => Pretty (Field ann) where
  pretty :: Field ann -> Doc
  pretty (Field name lines) =
    text "Field"
      $$ nest
        4
        ( text (show name)
            $$ vcat (map pretty lines)
        )
  pretty (Section name args fields) =
    text "Section"
      $$ nest
        4
        ( text (show name)
            $$ vcat (map (text . show) args)
            $$ vcat (map pretty fields)
        )
