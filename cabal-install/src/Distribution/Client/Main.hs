{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
--
-- @since 3.10.0.0
module Distribution.Client.Main (main) where

import Distribution.Client.Setup
  ( GlobalFlags (..)
  , globalCommand
  )
import Distribution.Simple.Setup
  ( fromFlagOrDefault
  )

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Config
  ( defaultConfigFile
  )

import Distribution.Client.Signal
  ( installTerminationHandler
  )
import Distribution.Client.Utils (relaxEncodingErrors)
import Distribution.Client.Version (cabalInstallVersion)

import Distribution.Simple.Command
  ( CommandParse (..)
  , commandFromSpec
  , commandsRun
  )
import Distribution.Simple.Utils
  ( cabalVersion
  , dieNoVerbosity
  , topHandler
  )
import Distribution.Text (display)

import Control.Exception (AssertionFailed, assert)
import Distribution.Compat.ResponseFile
import System.Directory
  ( doesFileExist
  )
import System.Environment (getProgName)
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )

import qualified Distribution.Client.Main.Commands as Commands
import qualified Distribution.Client.Main.Legacy as Legacy
import qualified Distribution.Client.Main.V2 as V2
import qualified Distribution.Client.Main.V2.Run as CmdRun

-- | Entry point
main :: [String] -> IO ()
main args = do
  installTerminationHandler
  -- Enable line buffering so that we can get fast feedback even when piped.
  -- This is especially important for CI and build systems.
  hSetBuffering stdout LineBuffering

  -- If the locale encoding for CLI doesn't support all Unicode characters,
  -- printing to it may fail unless we relax the handling of encoding errors
  -- when writing to stderr and stdout.
  relaxEncodingErrors stdout
  relaxEncodingErrors stderr
  let (args0, args1) = break (== "--") args

  mainWorker =<< (++ args1) <$> expandResponse args0

-- | Check whether assertions are enabled and print a warning in that case.
warnIfAssertionsAreEnabled :: IO ()
warnIfAssertionsAreEnabled =
  assert False (return ())
    `catch` (\(_e :: AssertionFailed) -> hPutStrLn stderr assertionsEnabledMsg)
  where
    -- Andreas, 2022-12-30, issue #8654:
    -- The verbosity machinery is not in place at this point (option -v not parsed),
    -- so instead of using function @warn@, we print straight to stderr.

    assertionsEnabledMsg =
      "Warning: this is a debug build of cabal-install with assertions enabled."

mainWorker :: [String] -> IO ()
mainWorker args = do
  topHandler
    $ case commandsRun (globalCommand commands) commands args of
      CommandHelp help -> printGlobalHelp help
      CommandList opts -> printOptionsList opts
      CommandErrors errs -> printErrors errs
      CommandReadyToGo (globalFlags, commandParse) ->
        case commandParse of
          _
            | fromFlagOrDefault False (globalVersion globalFlags) ->
                printVersion
            | fromFlagOrDefault False (globalNumericVersion globalFlags) ->
                printNumericVersion
          CommandHelp help -> printCommandHelp help
          CommandList opts -> printOptionsList opts
          CommandErrors errs -> do
            -- Check whether cabal is called from a script, like #!/path/to/cabal.
            case args of
              [] -> printErrors errs
              script : scriptArgs ->
                CmdRun.validScript script >>= \case
                  False -> printErrors errs
                  True -> do
                    -- In main operation (not help, version etc.) print warning if assertions are on.
                    warnIfAssertionsAreEnabled
                    CmdRun.handleShebang script scriptArgs
          CommandReadyToGo action -> do
            -- In main operation (not help, version etc.) print warning if assertions are on.
            warnIfAssertionsAreEnabled
            action globalFlags
  where
    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      putStr
        $ "\nYou can edit the cabal configuration file to set defaults:\n"
        ++ "  "
        ++ configFile
        ++ "\n"
      exists <- doesFileExist configFile
      unless exists
        $ putStrLn
        $ "This file will be generated with sensible "
        ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = dieNoVerbosity $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display cabalInstallVersion
    printVersion =
      putStrLn
        $ "cabal-install version "
        ++ display cabalInstallVersion
        ++ "\ncompiled using version "
        ++ display cabalVersion
        ++ " of the Cabal library "

    commands = map commandFromSpec commandSpecs
    commandSpecs =
      Commands.commandSpecs
        ++ V2.commandSpecs
        ++ Legacy.legacyCommands
