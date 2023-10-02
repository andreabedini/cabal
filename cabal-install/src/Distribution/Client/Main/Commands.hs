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
module Distribution.Client.Main.Commands
  ( commandSpecs
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Setup
  ( ActAsSetupFlags (..)
  , ConfigFlags (..)
  , FetchFlags (..)
  , FreezeFlags (..)
  , GetFlags (..)
  , GlobalFlags (..)
  , InfoFlags (..)
  , InitFlags (initHcPath, initVerbosity)
  , ListFlags (..)
  , ReportFlags (..)
  , UploadFlags (..)
  , UserConfigFlags (..)
  , actAsSetupCommand
  , checkCommand
  , configCompilerAux'
  , configPackageDB'
  , fetchCommand
  , formatCommand
  , genBoundsCommand
  , getCommand
  , infoCommand
  , initCommand
  , listCommand
  , listNeedsCompiler
  , manpageCommand
  , reportCommand
  , unpackCommand
  , uploadCommand
  , userConfigCommand
  , withRepoContext
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , HaddockFlags (..)
  , HaddockTarget (..)
  , HscolourFlags (..)
  , defaultHaddockFlags
  , flagToMaybe
  , fromFlag
  , fromFlagOrDefault
  , hscolourCommand
  )

import Distribution.Client.Check as Check (check)
import Distribution.Client.Config
  ( SavedConfig (..)
  , createDefaultConfigFile
  , getConfigFilePath
  , loadConfig
  , userConfigDiff
  , userConfigUpdate
  )
import Distribution.Client.Errors
import Distribution.Client.Fetch (fetch)
import Distribution.Client.GenBounds (genBounds)
import Distribution.Client.Get (get)
import Distribution.Client.Init (initCmd)
import qualified Distribution.Client.List as List
  ( info
  , list
  )
import qualified Distribution.Client.Main.Legacy as Legacy
import Distribution.Client.Manpage (manpageCmd)
import Distribution.Client.ManpageFlags (ManpageFlags (..))
import Distribution.Client.Nix (nixShell)
import Distribution.Client.Sandbox
  ( findSavedDistPref
  , loadConfigOrSandboxConfig
  )
import Distribution.Client.SetupWrapper
  ( SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , setupWrapper
  )
import Distribution.Client.Targets
  ( readUserTargets
  )
import Distribution.Client.Types.Credentials (Password (..))
import qualified Distribution.Client.Upload as Upload

import qualified Distribution.Make as Make
import Distribution.Package (Package (..))
import Distribution.PackageDescription (BuildType (..))
import Distribution.PackageDescription.PrettyPrint
  ( writeGenericPackageDescription
  )
import qualified Distribution.Simple as Simple
import Distribution.Simple.Command
  ( Command
  , CommandSpec (..)
  , CommandType (..)
  , CommandUI (..)
  , commandAddAction
  , hiddenCommand
  )
import Distribution.Simple.Configure (configCompilerAuxEx, getPersistBuildConfig)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Simple.Program (getProgramInvocationOutput)
import Distribution.Simple.Program.Run (simpleProgramInvocation)
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , notice
  , tryFindPackageDesc
  )
import Distribution.Text (display)
import Distribution.Verbosity as Verbosity (normal)

import Control.Exception (try)
import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , withCurrentDirectory
  )
import System.Environment (getProgName)
import System.FilePath
  ( dropExtension
  , splitExtension
  , takeExtension
  , (<.>)
  , (</>)
  )

commandSpecs :: [CommandSpec (GlobalFlags -> IO ())]
commandSpecs =
  [ regularCmd listCommand listAction
  , regularCmd infoCommand infoAction
  , regularCmd fetchCommand fetchAction
  , regularCmd getCommand getAction
  , regularCmd unpackCommand unpackAction
  , regularCmd checkCommand checkAction
  , regularCmd uploadCommand uploadAction
  , regularCmd reportCommand reportAction
  , regularCmd initCommand initAction
  , regularCmd userConfigCommand userConfigAction
  , regularCmd genBoundsCommand genBoundsAction
  , wrapperCmd hscolourCommand hscolourVerbosity hscolourDistPref
  , hiddenCmd formatCommand formatAction
  , hiddenCmd actAsSetupCommand actAsSetupAction
  , hiddenCmd manpageCommand (manpageAction commandSpecs)
  ]

-- Duplicated in Distribution.Client.CmdLegacy. Any changes must be
-- reflected there, as well.
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

wrapperCmd
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Flag Verbosity)
  -> (flags -> Flag String)
  -> CommandSpec (GlobalFlags -> IO ())
wrapperCmd ui verbosity distPref =
  CommandSpec ui (\ui' -> wrapperAction ui' verbosity distPref) NormalCommand

wrapperAction
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Flag Verbosity)
  -> (flags -> Flag String)
  -> Command (GlobalFlags -> IO ())
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

listAction :: ListFlags -> [String] -> (GlobalFlags -> IO ())
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags' = savedConfigureFlags config
      configFlags =
        configFlags'
          { configPackageDBs =
              configPackageDBs configFlags'
                `mappend` listPackageDBs listFlags
          , configHcPath = listHcPath listFlags
          }
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  compProgdb <-
    if listNeedsCompiler listFlags
      then do
        (comp, _, progdb) <- configCompilerAux' configFlags
        return (Just (comp, progdb))
      else return Nothing
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.list
      verbosity
      (configPackageDB' configFlags)
      repoContext
      compProgdb
      listFlags
      extraArgs

infoAction :: InfoFlags -> [String] -> (GlobalFlags -> IO ())
infoAction infoFlags extraArgs globalFlags = do
  let verbosity = fromFlag (infoVerbosity infoFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags' = savedConfigureFlags config
      configFlags =
        configFlags'
          { configPackageDBs =
              configPackageDBs configFlags'
                `mappend` infoPackageDBs infoFlags
          }
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAuxEx configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.info
      verbosity
      (configPackageDB' configFlags)
      repoContext
      comp
      progdb
      globalFlags'
      infoFlags
      targets

fetchAction :: FetchFlags -> [String] -> (GlobalFlags -> IO ())
fetchAction fetchFlags extraArgs globalFlags = do
  let verbosity = fromFlag (fetchVerbosity fetchFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let configFlags = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    fetch
      verbosity
      (configPackageDB' configFlags)
      repoContext
      comp
      platform
      progdb
      globalFlags'
      fetchFlags
      targets

genBoundsAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
genBoundsAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
      genBounds
        verbosity
        (configPackageDB' configFlags)
        repoContext
        comp
        platform
        progdb
        globalFlags'
        freezeFlags

uploadAction :: UploadFlags -> [String] -> (GlobalFlags -> IO ())
uploadAction uploadFlags extraArgs globalFlags = do
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles = extraArgs
  when (null tarfiles && not (fromFlag (uploadDoc uploadFlags')))
    $ dieWithException verbosity UploadAction
  checkTarFiles extraArgs
  maybe_password <-
    case uploadPasswordCmd uploadFlags' of
      Flag (xs : xss) ->
        Just
          . Password
          <$> getProgramInvocationOutput
            verbosity
            (simpleProgramInvocation xs xss)
      _ -> pure $ flagToMaybe $ uploadPassword uploadFlags'
  withRepoContext verbosity globalFlags' $ \repoContext -> do
    if fromFlag (uploadDoc uploadFlags')
      then do
        when (length tarfiles > 1)
          $ dieWithException verbosity UploadActionDocumentation
        tarfile <- maybe (generateDocTarball config) return $ listToMaybe tarfiles
        Upload.uploadDoc
          verbosity
          repoContext
          (flagToMaybe $ uploadToken uploadFlags')
          (flagToMaybe $ uploadUsername uploadFlags')
          maybe_password
          (fromFlag (uploadCandidate uploadFlags'))
          tarfile
      else do
        Upload.upload
          verbosity
          repoContext
          (flagToMaybe $ uploadToken uploadFlags')
          (flagToMaybe $ uploadUsername uploadFlags')
          maybe_password
          (fromFlag (uploadCandidate uploadFlags'))
          tarfiles
  where
    verbosity = fromFlag (uploadVerbosity uploadFlags)
    checkTarFiles tarfiles
      | not (null otherFiles) =
          dieWithException verbosity $ UploadActionOnlyArchives otherFiles
      | otherwise =
          sequence_
            [ do
              exists <- doesFileExist tarfile
              unless exists $ dieWithException verbosity $ FileNotFound tarfile
            | tarfile <- tarfiles
            ]
      where
        otherFiles = filter (not . isTarGzFile) tarfiles
        isTarGzFile file = case splitExtension file of
          (file', ".gz") -> takeExtension file' == ".tar"
          _ -> False
    generateDocTarball config = do
      notice verbosity
        $ "No documentation tarball specified. "
        ++ "Building a documentation tarball with default settings...\n"
        ++ "If you need to customise Haddock options, "
        ++ "run 'haddock --for-hackage' first "
        ++ "to generate a documentation tarball."
      Legacy.haddockAction
        (defaultHaddockFlags{haddockForHackage = Flag ForHackage})
        []
        globalFlags
      distPref <- findSavedDistPref config NoFlag
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      return $ distPref </> display (packageId pkg) ++ "-docs" <.> "tar.gz"

checkAction :: Flag Verbosity -> [String] -> (GlobalFlags -> IO ())
checkAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  unless (null extraArgs)
    $ dieWithException verbosity
    $ CheckAction extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure

formatAction :: Flag Verbosity -> [String] -> (GlobalFlags -> IO ())
formatAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  path <- case extraArgs of
    [] -> do
      cwd <- getCurrentDirectory
      tryFindPackageDesc verbosity cwd
    (p : _) -> return p
  pkgDesc <- readGenericPackageDescription verbosity path
  -- Uses 'writeFileAtomic' under the hood.
  writeGenericPackageDescription path pkgDesc

reportAction :: ReportFlags -> [String] -> (GlobalFlags -> IO ())
reportAction reportFlags extraArgs globalFlags = do
  let verbosity = fromFlag (reportVerbosity reportFlags)
  unless (null extraArgs)
    $ dieWithException verbosity
    $ ReportAction extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  withRepoContext verbosity globalFlags' $ \repoContext ->
    Upload.report
      verbosity
      repoContext
      (flagToMaybe $ reportToken reportFlags')
      (flagToMaybe $ reportUsername reportFlags')
      (flagToMaybe $ reportPassword reportFlags')

getAction :: GetFlags -> [String] -> (GlobalFlags -> IO ())
getAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  withRepoContext verbosity (savedGlobalFlags config) $ \repoContext ->
    get
      verbosity
      repoContext
      globalFlags'
      getFlags
      targets

unpackAction :: GetFlags -> [String] -> (GlobalFlags -> IO ())
unpackAction getFlags extraArgs globalFlags = do
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> (GlobalFlags -> IO ())
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
          (configPackageDB' confFlags')
          repoContext
          comp
          progdb
          initFlags'

    verbosity = fromFlag (initVerbosity initFlags)
    compFlags = mempty{configHcPath = initHcPath initFlags}

userConfigAction :: UserConfigFlags -> [String] -> (GlobalFlags -> IO ())
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
actAsSetupAction :: ActAsSetupFlags -> [String] -> (GlobalFlags -> IO ())
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
   in case bt of
        Simple -> Simple.defaultMainArgs args
        Configure ->
          Simple.defaultMainWithHooksArgs
            Simple.autoconfUserHooks
            args
        Make -> Make.defaultMainArgs args
        Custom -> error "actAsSetupAction Custom"

manpageAction :: [CommandSpec action] -> ManpageFlags -> [String] -> (GlobalFlags -> IO ())
manpageAction commands flags extraArgs _ = do
  let verbosity = fromFlag (manpageVerbosity flags)
  unless (null extraArgs)
    $ dieWithException verbosity
    $ ManpageAction extraArgs
  pname <- getProgName
  let cabalCmd =
        if takeExtension pname == ".exe"
          then dropExtension pname
          else pname
  manpageCmd cabalCmd commands flags
