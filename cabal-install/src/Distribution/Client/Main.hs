{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( ActAsSetupFlags (..)
  , ConfigFlags (..)
  , GlobalFlags (..)
  , InitFlags (initHcPath, initVerbosity)
  , UserConfigFlags (..)
  , actAsSetupCommand
  , configCompilerAux'
  , configPackageDB'
  
  , globalCommand
  , initCommand
  , manpageCommand
  
  , userConfigCommand
  , withRepoContext
  )
import Distribution.Simple.Setup
  ( fromFlag
  , fromFlagOrDefault
  )

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig (..)
  , createDefaultConfigFile
  , defaultConfigFile
  , getConfigFilePath
  , userConfigDiff
  , userConfigUpdate
  )

import qualified Distribution.Client.CmdBench as CmdBench
import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.CmdClean as CmdClean
import qualified Distribution.Client.CmdConfigure as CmdConfigure
import qualified Distribution.Client.CmdExec as CmdExec
import qualified Distribution.Client.CmdFreeze as CmdFreeze
import qualified Distribution.Client.CmdHaddock as CmdHaddock
import qualified Distribution.Client.CmdHaddockProject as CmdHaddockProject
import qualified Distribution.Client.CmdInstall as CmdInstall
import qualified Distribution.Client.CmdListBin as CmdListBin
import qualified Distribution.Client.CmdOutdated as CmdOutdated
import qualified Distribution.Client.CmdPath as CmdPath
import qualified Distribution.Client.CmdRepl as CmdRepl
import qualified Distribution.Client.CmdRun as CmdRun
import qualified Distribution.Client.CmdSdist as CmdSdist
import qualified Distribution.Client.CmdTarget as CmdTarget
import qualified Distribution.Client.CmdTest as CmdTest
import qualified Distribution.Client.CmdUpdate as CmdUpdate

import Distribution.Client.Init (initCmd)
import Distribution.Client.Manpage (manpageCmd)
import Distribution.Client.ManpageFlags (ManpageFlags (..))
import Distribution.Client.Sandbox
  ( loadConfigOrSandboxConfig
  
  )
import Distribution.Client.Signal
  ( installTerminationHandler
  )
import Distribution.Client.Utils
  ( relaxEncodingErrors
  )
import Distribution.Client.Version
  ( cabalInstallGitInfo
  , cabalInstallVersion
  )

import Distribution.PackageDescription
  ( BuildType (..)
  
  
  )

import Distribution.Client.Errors
import Distribution.Compat.ResponseFile
import qualified Distribution.Make as Make
import qualified Distribution.Simple as Simple
import Distribution.Simple.Command
  ( Command
  , CommandParse (..)
  , CommandSpec (..)
  , CommandType (..)
  , CommandUI (..)
  , commandAddAction
  , commandFromSpec
  
  , commandsRunWithFallback
  , defaultCommandFallback
  , hiddenCommand
  )
import Distribution.Simple.Compiler (interpretPackageDBStack)
import Distribution.Simple.Program
  ( defaultProgramSearchPath
  , findProgramOnSearchPath
  
  
  )
import Distribution.Simple.Utils
  ( cabalGitInfo
  , cabalVersion
  , createDirectoryIfMissingVerbose
  , dieNoVerbosity
  , dieWithException
  
  
  
  , topHandler
  
  
  )
import Distribution.Text
  ( display
  )
import Distribution.Verbosity as Verbosity
  ( normal
  )

import Control.Exception (AssertionFailed, assert, try)
import System.Directory
  ( doesFileExist
  , withCurrentDirectory
  )
import System.Environment (getEnvironment, getExecutablePath, getProgName)
import System.FilePath
  ( dropExtension
  
  , takeExtension
  
  
  )
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )
import System.Process (createProcess, env, proc, waitForProcess)

-- | Entry point
--
-- This does three things.
--
-- One, it initializes the program, providing support for termination
-- signals, preparing console linebuffering, and relaxing encoding errors.
--
-- Two, it processes (via an IO action) response
-- files, calling 'expandResponse' in Cabal/Distribution.Compat.ResponseFile
--
-- Note that here, it splits the arguments on a strict match to
-- "--", and won't parse response files after the split.
--
-- Three, it calls the 'mainWorker', which calls the argument parser,
-- producing 'CommandParse' data, which mainWorker pattern-matches
-- into IO actions for execution.
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

  -- Response files support.
  -- See 'expandResponse' documentation in Cabal/Distribution.Compat.ResponseFile
  -- for more information.
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

-- | Core worker, similar to 'defaultMainHelper' in Cabal/Distribution.Simple
--
-- With an exception-handler @topHandler@, mainWorker calls commandsRun
-- to parse arguments, then pattern-matches the CommandParse data
-- into IO actions for execution.
mainWorker :: [String] -> IO ()
mainWorker args = do
  topHandler $ do
    command <- commandsRunWithFallback (globalCommand commands) commands delegateToExternal args
    case command of
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
    delegateToExternal
      :: [Command Action]
      -> String
      -> [String]
      -> IO (CommandParse Action)
    delegateToExternal commands' name cmdArgs = do
      mCommand <- findProgramOnSearchPath normal defaultProgramSearchPath ("cabal-" <> name)
      case mCommand of
        Just (exec, _) -> return (CommandReadyToGo $ \_ -> callExternal exec name cmdArgs)
        Nothing -> defaultCommandFallback commands' name cmdArgs

    callExternal :: String -> String -> [String] -> IO ()
    callExternal exec name cmdArgs = do
      cur_env <- getEnvironment
      cabal_exe <- getExecutablePath
      let new_env = ("CABAL", cabal_exe) : cur_env
      result <- try $ createProcess ((proc exec (name : cmdArgs)){env = Just new_env})
      case result of
        Left ex -> printErrors ["Error executing external command: " ++ show (ex :: SomeException)]
        Right (_, _, _, ph) -> waitForProcess ph >>= exitWith

    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      -- Andreas Abel, 2024-01-28: https://github.com/haskell/cabal/pull/9614
      -- See cabal-testsuite/PackageTests/Help/HelpPrintsConfigFile/
      -- Third-party tools may rely on the specific wording
      -- to find the config file in the help text, so do not change!
      putStr $
        "\nYou can edit the cabal configuration file to set defaults:\n"
          ++ "  "
          ++ configFile
          ++ "\n"
      exists <- doesFileExist configFile
      unless exists $
        putStrLn $
          "This file will be generated with sensible "
            ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = dieNoVerbosity $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display cabalInstallVersion
    printVersion =
      putStrLn $
        "cabal-install version "
          ++ display cabalInstallVersion
          ++ " "
          ++ cabalInstallGitInfo
          ++ "\ncompiled using version "
          ++ display cabalVersion
          ++ " of the Cabal library "
          ++ cabalGitInfo'
      where
        cabalGitInfo'
          | cabalGitInfo == cabalInstallGitInfo = "(in-tree)"
          | otherwise = cabalGitInfo

    commands = map commandFromSpec commandSpecs
    commandSpecs =
      [ regularCmd initCommand initAction
      , regularCmd userConfigCommand userConfigAction
      , regularCmd CmdPath.pathCommand CmdPath.pathAction
      , hiddenCmd actAsSetupCommand actAsSetupAction
      , hiddenCmd manpageCommand (manpageAction commandSpecs)
      , regularCmd CmdListBin.listbinCommand CmdListBin.listbinAction
      , regularCmd CmdConfigure.configureCommand CmdConfigure.configureAction
      , regularCmd CmdUpdate.updateCommand CmdUpdate.updateAction
      , regularCmd CmdBuild.buildCommand CmdBuild.buildAction
      , regularCmd CmdRepl.replCommand CmdRepl.replAction
      , regularCmd CmdFreeze.freezeCommand CmdFreeze.freezeAction
      , regularCmd CmdHaddock.haddockCommand CmdHaddock.haddockAction
      , regularCmd CmdHaddockProject.haddockProjectCommand CmdHaddockProject.haddockProjectAction
      , regularCmd CmdInstall.installCommand CmdInstall.installAction
      , regularCmd CmdRun.runCommand CmdRun.runAction
      , regularCmd CmdTest.testCommand CmdTest.testAction
      , regularCmd CmdBench.benchCommand CmdBench.benchAction
      , regularCmd CmdExec.execCommand CmdExec.execAction
      , regularCmd CmdClean.cleanCommand CmdClean.cleanAction
      , regularCmd CmdSdist.sdistCommand CmdSdist.sdistAction
      , regularCmd CmdTarget.targetCommand CmdTarget.targetAction
      , regularCmd CmdOutdated.outdatedCommand CmdOutdated.outdatedAction
      ]

type Action = GlobalFlags -> IO ()

regularCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

hiddenCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
hiddenCmd ui action =
  CommandSpec
    ui
    (\ui' -> hiddenCommand (commandAddAction ui' action))
    HiddenCommand

initAction :: InitFlags -> [String] -> Action
initAction initFlags extraArgs globalFlags = do
  -- it takes the first value within extraArgs (if there's one)
  -- and uses it as the root directory for the new project
  case extraArgs of
    [] -> initAction'
    [projectDir] -> do
      createDirectoryIfMissingVerbose verbosity True projectDir
      withCurrentDirectory projectDir initAction'
    _ -> dieWithException verbosity InitAction
  where
    initAction' = do
      confFlags <- loadConfigOrSandboxConfig verbosity globalFlags
      -- override with `--with-compiler` from CLI if available
      let confFlags' = savedConfigureFlags confFlags `mappend` compFlags
          initFlags' = savedInitFlags confFlags `mappend` initFlags
          globalFlags' = savedGlobalFlags confFlags `mappend` globalFlags

      (comp, _, progdb) <- configCompilerAux' confFlags'

      withRepoContext verbosity globalFlags' $ \repoContext ->
        initCmd
          verbosity
          (interpretPackageDBStack Nothing (configPackageDB' confFlags'))
          repoContext
          comp
          progdb
          initFlags'

    verbosity = fromFlag (initVerbosity initFlags)
    compFlags = mempty{configHcPath = initHcPath initFlags}

userConfigAction :: UserConfigFlags -> [String] -> Action
userConfigAction ucflags extraArgs globalFlags = do
  let verbosity = fromFlag (userConfigVerbosity ucflags)
      frc = fromFlag (userConfigForce ucflags)
      extraLines = fromFlag (userConfigAppendLines ucflags)
  case extraArgs of
    ("init" : _) -> do
      path <- configFile
      fileExists <- doesFileExist path
      if (not fileExists || (fileExists && frc))
        then void $ createDefaultConfigFile verbosity extraLines path
        else dieWithException verbosity $ UserConfigAction path
    ("diff" : _) -> traverse_ putStrLn =<< userConfigDiff verbosity globalFlags extraLines
    ("update" : _) -> userConfigUpdate verbosity globalFlags extraLines
    -- Error handling.
    [] -> dieWithException verbosity SpecifySubcommand
    _ -> dieWithException verbosity $ UnknownUserConfigSubcommand extraArgs
  where
    configFile = getConfigFilePath (globalConfigFile globalFlags)

-- | Used as an entry point when cabal-install needs to invoke itself
-- as a setup script. This can happen e.g. when doing parallel builds.
actAsSetupAction :: ActAsSetupFlags -> [String] -> Action
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
   in case bt of
        Simple -> Simple.defaultMainArgs args
        Configure ->
          Simple.defaultMainWithSetupHooksArgs
            Simple.autoconfSetupHooks
            args
        Make -> Make.defaultMainArgs args
        Hooks -> error "actAsSetupAction Hooks"
        Custom -> error "actAsSetupAction Custom"

manpageAction :: [CommandSpec action] -> ManpageFlags -> [String] -> Action
manpageAction commands flags extraArgs _ = do
  let verbosity = fromFlag (manpageVerbosity flags)
  unless (null extraArgs) $
    dieWithException verbosity $
      ManpageAction extraArgs
  pname <- getProgName
  let cabalCmd =
        if takeExtension pname == ".exe"
          then dropExtension pname
          else pname
  manpageCmd cabalCmd commands flags
