{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- For HasVerbosity(verbosity)
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
module Distribution.Client.Main.Legacy
  ( legacyCommands
  -- This marked as legacyCmd but uploadAction (a regularCmd) depends on it
  , haddockAction
  ) where

import Distribution.Client.Setup
  ( BuildFlags (..)
  , ConfigExFlags (..)
  , ConfigFlags (..)
  , FreezeFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  , benchmarkCommand
  , buildCommand
  , cleanCommand
  , configCompilerAux'
  , configPackageDB'
  , configureExCommand
  , copyCommand
  , defaultConfigExFlags
  , defaultInstallFlags
  , freezeCommand
  , haddockCommand
  , installCommand
  , reconfigureCommand
  , registerCommand
  , replCommand
  , runCommand
  , testCommand
  , withRepoContext
  )
import Distribution.Simple.Setup
  ( BenchmarkFlags (..)
  , CleanFlags (..)
  , CopyFlags (..)
  , Flag (..)
  , HaddockFlags (..)
  , HaddockTarget (..)
  , RegisterFlags (..)
  , ReplFlags (..)
  , TestFlags (..)
  , configAbsolutePaths
  , defaultHaddockFlags
  , fromFlag
  , fromFlagOrDefault
  , toFlag
  )

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig (..)
  )
import Distribution.Client.SetupWrapper
  ( SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , setupWrapper
  )
import Distribution.Client.Targets
  ( readUserTargets
  )

import Distribution.Client.Configure (configure, writeConfigFlags)
import Distribution.Client.Freeze (freeze)
import Distribution.Client.Install (install)

import Distribution.Client.Nix
  ( nixInstantiate
  , nixShell
  )
import Distribution.Client.Reconfigure (Check (..), reconfigure)
import Distribution.Client.Run (run, splitRunArgs)
import Distribution.Client.Sandbox
  ( findSavedDistPref
  , loadConfigOrSandboxConfig
  , updateInstallDirs
  )
import Distribution.Client.Tar (createTarGzFile)
import Distribution.Client.Utils
  ( determineNumJobs
  )

import Distribution.Package (packageId)
import Distribution.PackageDescription
  ( Executable (..)
  , buildable
  )

import Distribution.Simple.Build
  ( startInterpreter
  )
import Distribution.Simple.Command
  ( Command
  , CommandSpec (..)
  , CommandType (..)
  , CommandUI (..)
  , commandAddAction
  , commandShowOptions
  )
import Distribution.Simple.Compiler (PackageDBStack)
import Distribution.Simple.Configure
  ( ConfigStateFileError (..)
  , configCompilerAuxEx
  , getPersistBuildConfig
  , interpretPackageDbFlags
  , tryGetPersistBuildConfig
  )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program.Db (configureAllKnownPrograms, defaultProgramDb, reconfigurePrograms)
import Distribution.Text
  ( display
  )
import qualified Distribution.Types.UnqualComponentName as Make
import Distribution.Verbosity as Verbosity
  ( normal
  )
import Distribution.Version
  ( Version
  , mkVersion
  , orLaterVersion
  )

import Control.Exception (try)
import Data.Monoid (Any (..))
import Distribution.Client.Errors
import qualified Distribution.Client.Setup as Client
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils (dieWithException, findPackageDesc, info, notice)
import Distribution.Utils.Generic (wrapText)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

legacyCommands :: [CommandSpec (GlobalFlags -> IO ())]
legacyCommands =
  concat
    [ legacyCmd configureExCommand configureAction
    , legacyCmd buildCommand buildAction
    , legacyCmd replCommand replAction
    , legacyCmd freezeCommand freezeAction
    , legacyCmd haddockCommand haddockAction
    , legacyCmd installCommand installAction
    , legacyCmd runCommand runAction
    , legacyCmd testCommand testAction
    , legacyCmd benchmarkCommand benchmarkAction
    , legacyCmd cleanCommand cleanAction
    , legacyWrapperCmd copyCommand copyVerbosity copyDistPref
    , legacyWrapperCmd registerCommand regVerbosity regDistPref
    , legacyCmd reconfigureCommand reconfigureAction
    ]

type Action = GlobalFlags -> IO ()

-- Duplicated in Distribution.Client.CmdLegacy. Any changes must be
-- reflected there, as well.
regularCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

wrapperCmd
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Flag Verbosity)
  -> (flags -> Flag String)
  -> CommandSpec Action
wrapperCmd ui verbosity distPref =
  CommandSpec ui (\ui' -> wrapperAction ui' verbosity distPref) NormalCommand

wrapperAction
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Flag Verbosity)
  -> (flags -> Flag String)
  -> Command Action
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction
    command
      { commandDefaultFlags = mempty
      }
    $ \flags extraArgs globalFlags -> do
      let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
      load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
      let config = either (\(SomeException _) -> mempty) id load
      distPref <- findSavedDistPref config (distPrefFlag flags)
      let setupScriptOptions = defaultSetupScriptOptions{useDistPref = distPref}
      setupWrapper
        verbosity
        setupScriptOptions
        Nothing
        command
        (const flags)
        (const extraArgs)

configureAction
  :: (ConfigFlags, ConfigExFlags)
  -> [String]
  -> Action
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <-
    updateInstallDirs (configUserInstall configFlags)
      <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  nixInstantiate verbosity distPref True globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let configFlags' = savedConfigureFlags config `mappend` configFlags
        configExFlags' = savedConfigureExFlags config `mappend` configExFlags
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAuxEx configFlags'

    writeConfigFlags verbosity distPref (configFlags', configExFlags')

    -- What package database(s) to use
    let packageDBs :: PackageDBStack
        packageDBs =
          interpretPackageDbFlags
            (fromFlag (configUserInstall configFlags'))
            (configPackageDBs configFlags')

    withRepoContext verbosity globalFlags' $ \repoContext ->
      configure
        verbosity
        packageDBs
        repoContext
        comp
        platform
        progdb
        configFlags'
        configExFlags'
        extraArgs

reconfigureAction
  :: (ConfigFlags, ConfigExFlags)
  -> [String]
  -> Action
reconfigureAction flags@(configFlags, _) _ globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <-
    updateInstallDirs (configUserInstall configFlags)
      <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  let checkFlags = Check $ \_ saved -> do
        let flags' = saved <> flags
        unless (saved == flags') $ info verbosity message
        pure (Any True, flags')
        where
          -- This message is correct, but not very specific: it will list all
          -- of the new flags, even if some have not actually changed. The
          -- \*minimal* set of changes is more difficult to determine.
          message =
            "flags changed: "
              ++ unwords (commandShowOptions configureExCommand flags)
  nixInstantiate verbosity distPref True globalFlags config
  _ <-
    reconfigure
      configureAction
      verbosity
      distPref
      NoFlag
      checkFlags
      []
      globalFlags
      config
  pure ()

buildAction :: BuildFlags -> [String] -> Action
buildAction buildFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  -- Calls 'configureAction' to do the real work, so nothing special has to be
  -- done to support sandboxes.
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags)
      mempty
      []
      globalFlags
      config
  nixShell verbosity distPref globalFlags config $ do
    build verbosity config' distPref buildFlags extraArgs

-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> SavedConfig -> FilePath -> BuildFlags -> [String] -> IO ()
build verbosity config distPref buildFlags extraArgs =
  setupWrapper
    verbosity
    setupOptions
    Nothing
    (Cabal.buildCommand progDb)
    mkBuildFlags
    (const extraArgs)
  where
    progDb = defaultProgramDb
    setupOptions = defaultSetupScriptOptions{useDistPref = distPref}

    mkBuildFlags version = filterBuildFlags version config buildFlags'
    buildFlags' =
      buildFlags
        { buildVerbosity = toFlag verbosity
        , buildDistPref = toFlag distPref
        }

-- | Make sure that we don't pass new flags to setup scripts compiled against
-- old versions of Cabal.
filterBuildFlags :: Version -> SavedConfig -> BuildFlags -> BuildFlags
filterBuildFlags version config buildFlags
  | version >= mkVersion [1, 19, 1] = buildFlags_latest
  -- Cabal < 1.19.1 doesn't support 'build -j'.
  | otherwise = buildFlags_pre_1_19_1
  where
    buildFlags_pre_1_19_1 =
      buildFlags
        { buildNumJobs = NoFlag
        }
    buildFlags_latest =
      buildFlags
        { -- Take the 'jobs' setting config file into account.
          buildNumJobs =
            Flag
              . Just
              . determineNumJobs
              $ (numJobsConfigFlag `mappend` numJobsCmdLineFlag)
        }
    numJobsConfigFlag = installNumJobs . savedInstallFlags $ config
    numJobsCmdLineFlag = buildNumJobs buildFlags

replAction :: ReplFlags -> [String] -> Action
replAction replFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (replVerbosity replFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (replDistPref replFlags)
  cwd <- getCurrentDirectory
  pkgDesc <- findPackageDesc cwd
  let
    -- There is a .cabal file in the current directory: start a REPL and load
    -- the project's modules.
    onPkgDesc = do
      -- Calls 'configureAction' to do the real work, so nothing special has to
      -- be done to support sandboxes.
      _ <-
        reconfigure
          configureAction
          verbosity
          distPref
          NoFlag
          mempty
          []
          globalFlags
          config
      let progDb = defaultProgramDb
          setupOptions =
            defaultSetupScriptOptions
              { useCabalVersion = orLaterVersion $ mkVersion [1, 18, 0]
              , useDistPref = distPref
              }
          replFlags' =
            replFlags
              { replVerbosity = toFlag verbosity
              , replDistPref = toFlag distPref
              }

      nixShell verbosity distPref globalFlags config
        $ setupWrapper verbosity setupOptions Nothing (Cabal.replCommand progDb) (const replFlags') (const extraArgs)

    -- No .cabal file in the current directory: just start the REPL (possibly
    -- using the sandbox package DB).
    onNoPkgDesc = do
      let configFlags = savedConfigureFlags config
      (comp, platform, programDb) <- configCompilerAux' configFlags
      programDb' <-
        reconfigurePrograms
          verbosity
          (replProgramPaths replFlags)
          (replProgramArgs replFlags)
          programDb
      nixShell verbosity distPref globalFlags config $ do
        startInterpreter
          verbosity
          programDb'
          comp
          platform
          (configPackageDB' configFlags)

  either (const onNoPkgDesc) (const onPkgDesc) pkgDesc

installAction
  :: ( ConfigFlags
     , ConfigExFlags
     , InstallFlags
     , HaddockFlags
     , TestFlags
     , BenchmarkFlags
     )
  -> [String]
  -> Action
installAction (configFlags, _, installFlags, _, _, _) _ globalFlags
  | fromFlagOrDefault False (installOnly installFlags) = do
      let verb = fromFlagOrDefault normal (configVerbosity configFlags)
      config <- loadConfigOrSandboxConfig verb globalFlags
      dist <- findSavedDistPref config (configDistPref configFlags)
      let setupOpts = defaultSetupScriptOptions{useDistPref = dist}
      setupWrapper
        verb
        setupOpts
        Nothing
        installCommand
        (const (mempty, mempty, mempty, mempty, mempty, mempty))
        (const [])
installAction
  ( configFlags
    , configExFlags
    , installFlags
    , haddockFlags
    , testFlags
    , benchmarkFlags
    )
  extraArgs
  globalFlags = do
    let verb = fromFlagOrDefault normal (configVerbosity configFlags)
    config <-
      updateInstallDirs (configUserInstall configFlags)
        <$> loadConfigOrSandboxConfig verb globalFlags

    dist <- findSavedDistPref config (configDistPref configFlags)

    do
      targets <- readUserTargets verb extraArgs

      let configFlags' =
            maybeForceTests installFlags'
              $ savedConfigureFlags config
              `mappend` configFlags{configDistPref = toFlag dist}
          configExFlags' =
            defaultConfigExFlags
              `mappend` savedConfigureExFlags config
              `mappend` configExFlags
          installFlags' =
            defaultInstallFlags
              `mappend` savedInstallFlags config
              `mappend` installFlags
          haddockFlags' =
            defaultHaddockFlags
              `mappend` savedHaddockFlags config
              `mappend` haddockFlags{haddockDistPref = toFlag dist}
          testFlags' =
            Cabal.defaultTestFlags
              `mappend` savedTestFlags config
              `mappend` testFlags{testDistPref = toFlag dist}
          benchmarkFlags' =
            Cabal.defaultBenchmarkFlags
              `mappend` savedBenchmarkFlags config
              `mappend` benchmarkFlags{benchmarkDistPref = toFlag dist}
          globalFlags' = savedGlobalFlags config `mappend` globalFlags
      (comp, platform, progdb) <- configCompilerAux' configFlags'

      -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the
      -- future.
      progdb' <- configureAllKnownPrograms verb progdb

      configFlags'' <- configAbsolutePaths configFlags'

      withRepoContext verb globalFlags' $ \repoContext ->
        install
          verb
          (configPackageDB' configFlags'')
          repoContext
          comp
          platform
          progdb'
          globalFlags'
          configFlags''
          configExFlags'
          installFlags'
          haddockFlags'
          testFlags'
          benchmarkFlags'
          targets
    where
      -- '--run-tests' implies '--enable-tests'.
      maybeForceTests installFlags' configFlags' =
        if fromFlagOrDefault False (installRunTests installFlags')
          then configFlags'{configTests = toFlag True}
          else configFlags'

testAction
  :: (BuildFlags, TestFlags)
  -> [String]
  -> GlobalFlags
  -> IO ()
testAction (buildFlags, testFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (testDistPref testFlags)
  let buildFlags' =
        buildFlags
          { buildVerbosity = testVerbosity testFlags
          }
      checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configTests configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable tests"
            let flags' =
                  ( configFlags{configTests = toFlag True}
                  , configExFlags
                  )
            pure (Any True, flags')

  _ <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags')
      checkFlags
      []
      globalFlags
      config
  nixShell verbosity distPref globalFlags config $ do
    let setupOptions = defaultSetupScriptOptions{useDistPref = distPref}
        testFlags' = testFlags{testDistPref = toFlag distPref}

    -- The package was just configured, so the LBI must be available.
    names <-
      componentNamesFromLBI
        verbosity
        distPref
        "test suites"
        (\c -> case c of LBI.CTest{} -> True; _ -> False)
    let extraArgs'
          | null extraArgs = case names of
              ComponentNamesUnknown -> []
              ComponentNames names' ->
                [ Make.unUnqualComponentName name
                | LBI.CTestName name <- names'
                ]
          | otherwise = extraArgs

    build verbosity config distPref buildFlags' extraArgs'
    setupWrapper verbosity setupOptions Nothing Cabal.testCommand (const testFlags') (const extraArgs')

data ComponentNames
  = ComponentNamesUnknown
  | ComponentNames [LBI.ComponentName]

-- | Return the names of all buildable components matching a given predicate.
componentNamesFromLBI
  :: Verbosity
  -> FilePath
  -> String
  -> (LBI.Component -> Bool)
  -> IO ComponentNames
componentNamesFromLBI verbosity distPref targetsDescr compPred = do
  eLBI <- tryGetPersistBuildConfig distPref
  case eLBI of
    Left err -> case err of
      -- Note: the build config could have been generated by a custom setup
      -- script built against a different Cabal version, so it's crucial that
      -- we ignore the bad version error here.
      ConfigStateFileBadVersion _ _ _ -> return ComponentNamesUnknown
      _ -> dieWithException verbosity $ ConfigStateFileException (show err)
    Right lbi -> do
      let pkgDescr = LBI.localPkgDescr lbi
          names =
            map LBI.componentName
              . filter (buildable . LBI.componentBuildInfo)
              . filter compPred
              $ LBI.pkgComponents pkgDescr
      if null names
        then do
          notice verbosity
            $ "Package has no buildable "
            ++ targetsDescr
            ++ "."
          exitSuccess -- See #3215.
        else return $! (ComponentNames names)

benchmarkAction
  :: (BuildFlags, BenchmarkFlags)
  -> [String]
  -> GlobalFlags
  -> IO ()
benchmarkAction
  (buildFlags, benchmarkFlags)
  extraArgs
  globalFlags = do
    let verbosity =
          fromFlagOrDefault
            normal
            (buildVerbosity buildFlags)

    config <- loadConfigOrSandboxConfig verbosity globalFlags
    distPref <- findSavedDistPref config (benchmarkDistPref benchmarkFlags)
    let buildFlags' =
          buildFlags
            { buildVerbosity = benchmarkVerbosity benchmarkFlags
            }

    let checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
          if fromFlagOrDefault False (configBenchmarks configFlags)
            then pure (mempty, flags)
            else do
              info verbosity "reconfiguring to enable benchmarks"
              let flags' =
                    ( configFlags{configBenchmarks = toFlag True}
                    , configExFlags
                    )
              pure (Any True, flags')

    config' <-
      reconfigure
        configureAction
        verbosity
        distPref
        (buildNumJobs buildFlags')
        checkFlags
        []
        globalFlags
        config
    nixShell verbosity distPref globalFlags config $ do
      let setupOptions = defaultSetupScriptOptions{useDistPref = distPref}
          benchmarkFlags' = benchmarkFlags{benchmarkDistPref = toFlag distPref}

      -- The package was just configured, so the LBI must be available.
      names <-
        componentNamesFromLBI
          verbosity
          distPref
          "benchmarks"
          (\c -> case c of LBI.CBench{} -> True; _ -> False)
      let extraArgs'
            | null extraArgs = case names of
                ComponentNamesUnknown -> []
                ComponentNames names' ->
                  [ Make.unUnqualComponentName name
                  | LBI.CBenchName name <- names'
                  ]
            | otherwise = extraArgs

      build verbosity config' distPref buildFlags' extraArgs'
      setupWrapper verbosity setupOptions Nothing Cabal.benchmarkCommand (const benchmarkFlags') (const extraArgs')

haddockAction :: HaddockFlags -> [String] -> Action
haddockAction haddockFlags extraArgs globalFlags = do
  let verbosity = fromFlag (haddockVerbosity haddockFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (haddockDistPref haddockFlags)
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      NoFlag
      mempty
      []
      globalFlags
      config
  nixShell verbosity distPref globalFlags config $ do
    let haddockFlags' =
          defaultHaddockFlags
            `mappend` savedHaddockFlags config'
            `mappend` haddockFlags{haddockDistPref = toFlag distPref}
        setupScriptOptions =
          defaultSetupScriptOptions
            { useDistPref = distPref
            }
    setupWrapper
      verbosity
      setupScriptOptions
      Nothing
      haddockCommand
      (const haddockFlags')
      (const extraArgs)
    when (haddockForHackage haddockFlags == Flag ForHackage) $ do
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      let dest = distPref </> name <.> "tar.gz"
          name = display (packageId pkg) ++ "-docs"
          docDir = distPref </> "doc" </> "html"
      createTarGzFile dest docDir name
      notice verbosity $ "Documentation tarball created: " ++ dest

cleanAction :: CleanFlags -> [String] -> Action
cleanAction cleanFlags extraArgs globalFlags = do
  load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
  let config = either (\(SomeException _) -> mempty) id load
  distPref <- findSavedDistPref config (cleanDistPref cleanFlags)
  let setupScriptOptions =
        defaultSetupScriptOptions
          { useDistPref = distPref
          , useWin32CleanHack = True
          }
      cleanFlags' = cleanFlags{cleanDistPref = toFlag distPref}
  setupWrapper
    verbosity
    setupScriptOptions
    Nothing
    cleanCommand
    (const cleanFlags')
    (const extraArgs)
  where
    verbosity = fromFlagOrDefault normal (cleanVerbosity cleanFlags)

freezeAction :: FreezeFlags -> [String] -> Action
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
      freeze
        verbosity
        (configPackageDB' configFlags)
        repoContext
        comp
        platform
        progdb
        globalFlags'
        freezeFlags

runAction :: BuildFlags -> [String] -> Action
runAction buildFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags)
      mempty
      []
      globalFlags
      config
  nixShell verbosity distPref globalFlags config $ do
    lbi <- getPersistBuildConfig distPref
    (exe, exeArgs) <- splitRunArgs verbosity lbi extraArgs

    build verbosity config' distPref buildFlags ["exe:" ++ display (exeName exe)]
    run verbosity lbi exe exeArgs

--
-- From Distribution.Client.CmdLegacy
--

-- Tweaked versions of code from Main.

class HasVerbosity a where
  verbosity :: a -> Verbosity

instance HasVerbosity (Cabal.Flag Verbosity) where
  verbosity = Cabal.fromFlagOrDefault normal

instance HasVerbosity a => HasVerbosity (a, b) where
  verbosity (a, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c) where
  verbosity (a, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d) where
  verbosity (a, _, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d, e) where
  verbosity (a, _, _, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d, e, f) where
  verbosity (a, _, _, _, _, _) = verbosity a

instance HasVerbosity Cabal.BuildFlags where
  verbosity = verbosity . Cabal.buildVerbosity

instance HasVerbosity Cabal.ConfigFlags where
  verbosity = verbosity . Cabal.configVerbosity

instance HasVerbosity Cabal.ReplFlags where
  verbosity = verbosity . Cabal.replVerbosity

instance HasVerbosity Client.FreezeFlags where
  verbosity = verbosity . Client.freezeVerbosity

instance HasVerbosity Cabal.HaddockFlags where
  verbosity = verbosity . Cabal.haddockVerbosity

instance HasVerbosity Client.UpdateFlags where
  verbosity = verbosity . Client.updateVerbosity

instance HasVerbosity Cabal.CleanFlags where
  verbosity = verbosity . Cabal.cleanVerbosity

legacyNote :: String -> String
legacyNote cmd =
  wrapText
    $ "The v1-"
    ++ cmd
    ++ " command is a part of the legacy v1 style of cabal usage.\n\n"
    ++ "It is a legacy feature and will be removed in a future release of cabal-install."
    ++ " Please file a bug if you cannot replicate a working v1- use case with the nix-style"
    ++ " commands.\n\n"
    ++ "For more information, see: https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html"

toLegacyCmd :: CommandSpec (globals -> IO action) -> [CommandSpec (globals -> IO action)]
toLegacyCmd mkSpec = [toLegacy mkSpec]
  where
    toLegacy (CommandSpec origUi@CommandUI{..} action type') = CommandSpec legUi action type'
      where
        legUi =
          origUi
            { commandName = "v1-" ++ commandName
            , commandNotes = Just $ \pname -> case commandNotes of
                Just notes -> notes pname ++ "\n" ++ legacyNote commandName
                Nothing -> legacyNote commandName
            }

legacyCmd :: HasVerbosity flags => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
legacyCmd ui action = toLegacyCmd (regularCmd ui action)

legacyWrapperCmd :: Monoid flags => CommandUI flags -> (flags -> Cabal.Flag Verbosity) -> (flags -> Cabal.Flag String) -> [CommandSpec (Client.GlobalFlags -> IO ())]
legacyWrapperCmd ui verbosity' distPref = toLegacyCmd (wrapperCmd ui verbosity' distPref)
