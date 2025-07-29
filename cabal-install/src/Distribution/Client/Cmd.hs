{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Cmd where

import Data.Monoid (Endo (..))
import Distribution.Client.CmdConfigure (configureCommand)
import Distribution.ReadE (ReadE (..))
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (..)
  )
import Options.Applicative
import Options.Applicative.Help (Pretty (..))

-- | Translate a CommandUI to an optparse-applicative Parser
fromCommandUI :: CommandUI flags -> Parser flags
fromCommandUI CommandUI{..} =
  hsubparser $
    command commandName $
      info parser $
        progDesc commandSynopsis
          <> header "header"
          <> briefDesc
          <> footer "footer"
          <> foldMap (\d -> footer (d commandName)) commandDescription
          <> foldMap (\d -> header (d commandName)) commandNotes
          <> footer (commandUsage commandName)
  where
    parser =
      appEndo
        (foldMap (Endo . translateOptionField) (commandOptions ParseArgs))
        (pure commandDefaultFlags)

-- | Translate a single OptionField to a Parser modifier
translateOptionField :: OptionField a -> Parser a -> Parser a
translateOptionField OptionField{optionDescr} a =
  foldr (<*>) a $ map translateOptDescr optionDescr

-- | Translate a single OptDescr to a Parser modifier
translateOptDescr :: OptDescr flags -> Parser (flags -> flags)
translateOptDescr = \case
  ReqArg desc (shortNames, longNames) placeholder upd _get ->
    option
      (eitherReader (runReadE upd))
      (optionModifiers shortNames longNames desc placeholder)
  OptArg desc (shortNames, longNames) placeholder upd (_, def) _get ->
    flag' def (flagModifiers shortNames longNames desc <> style (<> ("[=" <> pretty placeholder <> "]")))
      <|> option
        (eitherReader (runReadE upd))
        (internal <> optionModifiers shortNames longNames desc placeholder)
  ChoiceOpt choices ->
    asum
      [ flag id upd (flagModifiers shortNames longNames desc)
      | (desc, (shortNames, longNames), upd, _get) <- choices
      ]
  BoolOpt desc (trueShortNames, trueLongNames) (falseShortNames, falseLongNames) upd _get ->
    enableFlag <|> disableFlag
    where
      enableFlag = flag id (upd True) (flagModifiers trueShortNames trueLongNames desc)
      disableFlag = flag id (upd False) (flagModifiers falseShortNames falseLongNames desc)

-- | Create flag modifiers for optparse-applicative
flagModifiers :: [Char] -> [String] -> String -> Mod FlagFields a
flagModifiers shortNames longNames helpText =
  help helpText
    <> foldMap short shortNames
    <> foldMap long longNames

-- | Create option modifiers for optparse-applicative
optionModifiers :: [Char] -> [String] -> String -> String -> Mod OptionFields a
optionModifiers shortNames longNames helpText placeholder =
  help helpText
    <> metavar placeholder
    <> foldMap short shortNames
    <> foldMap long longNames

main :: IO ()
main = do
  customExecParser p opts >>= print
  where
    p = prefs (noBacktrack <> showHelpOnEmpty <> showHelpOnError <> helpLongEquals)
    opts =
      info
        (parser <**> helper)
        briefDesc

    parser = fromCommandUI configureCommand