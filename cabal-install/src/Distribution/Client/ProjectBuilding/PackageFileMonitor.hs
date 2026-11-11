{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.ProjectBuilding.PackageFileMonitor where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad

import Distribution.Client.DistDirLayout
import Distribution.Client.FileMonitor
import Distribution.Client.Types hiding
  ( BuildFailure (..)
  , BuildOutcome
  , BuildOutcomes
  , BuildResult (..)
  )

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  )
import Distribution.Simple.Utils (removeFileForcibly)
import Distribution.System (buildPlatform)

import qualified Data.Set as Set
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

-----------------------------
-- Package change detection
--

-- | As part of the dry run for local unpacked packages we have to check if the
-- package config or files have changed. That is the purpose of
-- 'PackageFileMonitor' and 'checkPackageFileMonitorChanged'.
--
-- When a package is (re)built, the monitor must be updated to reflect the new
-- state of the package. Because we sometimes build without reconfiguring the
-- state updates are split into two, one for package config changes and one
-- for other changes. This is the purpose of 'updatePackageConfigFileMonitor'
-- and 'updatePackageBuildFileMonitor'.
data PackageFileMonitor = PackageFileMonitor
  { pkgFileMonitorConfig :: FileMonitor ElaboratedConfiguredPackage ()
  , pkgFileMonitorBuild :: FileMonitor (Set ComponentName) BuildResultMisc
  , pkgFileMonitorReg :: FileMonitor () (Maybe InstalledPackageInfo)
  }

-- | This is all the components of the 'BuildResult' other than the
-- @['InstalledPackageInfo']@.
--
-- We have to split up the 'BuildResult' components since they get produced
-- at different times (or rather, when different things change).
type BuildResultMisc = (DocsResult, TestsResult)

newPackageFileMonitor
  :: ElaboratedSharedConfig
  -> DistDirLayout
  -> DistDirParams
  -> PackageFileMonitor
newPackageFileMonitor
  shared
  DistDirLayout{distPackageCacheFile}
  dparams =
    PackageFileMonitor
      { pkgFileMonitorConfig =
          FileMonitor
            { fileMonitorCacheFile = distPackageCacheFile dparams "config"
            , fileMonitorKeyValid = (==) `on` normaliseConfiguredPackage shared
            , fileMonitorCheckIfOnlyValueChanged = False
            }
      , pkgFileMonitorBuild =
          FileMonitor
            { fileMonitorCacheFile = distPackageCacheFile dparams "build"
            , fileMonitorKeyValid = \componentsToBuild componentsAlreadyBuilt ->
                componentsToBuild `Set.isSubsetOf` componentsAlreadyBuilt
            , fileMonitorCheckIfOnlyValueChanged = True
            }
      , pkgFileMonitorReg =
          newFileMonitor (distPackageCacheFile dparams "registration")
      }

-- | Helper function for 'checkPackageFileMonitorChanged',
-- 'updatePackageConfigFileMonitor' and 'updatePackageBuildFileMonitor'.
--
-- It selects the info from a 'ElaboratedConfiguredPackage' that are used by
-- the 'FileMonitor's (in the 'PackageFileMonitor') to detect value changes.
packageFileMonitorKeyValues
  :: ElaboratedConfiguredPackage
  -> (ElaboratedConfiguredPackage, Set ComponentName)
packageFileMonitorKeyValues elab =
  (elab_config, buildComponents)
  where
    -- The first part is the value used to guard (re)configuring the package.
    -- That is, if this value changes then we will reconfigure.
    -- The ElaboratedConfiguredPackage consists mostly (but not entirely) of
    -- information that affects the (re)configure step. But those parts that
    -- do not affect the configure step need to be nulled out. Those parts are
    -- the specific targets that we're going to build.
    --

    -- Additionally we null out the parts that don't affect the configure step because they're simply
    -- about how tests or benchmarks are run

    -- TODO there may be more things to null here too, in the future.

    elab_config :: ElaboratedConfiguredPackage
    elab_config =
      elab
        { elabBuildTargets = []
        , elabTestTargets = []
        , elabBenchTargets = []
        , elabReplTarget = []
        , elabHaddockTargets = []
        , elabBuildHaddocks = False
        , elabTestMachineLog = Nothing
        , elabTestHumanLog = Nothing
        , elabTestShowDetails = Nothing
        , elabTestKeepTix = False
        , elabTestTestOptions = []
        , elabBenchmarkOptions = []
        }

    -- The second part is the value used to guard the build step. So this is
    -- more or less the opposite of the first part, as it's just the info about
    -- what targets we're going to build.
    --
    buildComponents :: Set ComponentName
    buildComponents = elabBuildTargetWholeComponents elab

-- | Do all the checks on whether a package has changed and thus needs either
-- rebuilding or reconfiguring and rebuilding.
checkPackageFileMonitorChanged
  :: PackageFileMonitor
  -> ElaboratedConfiguredPackage
  -> FilePath
  -> Bool
  -- ^ Is the setup of the package going to be rebuilt?
  -> [BuildStatus]
  -> IO (Either BuildStatusRebuild BuildResult)
checkPackageFileMonitorChanged
  PackageFileMonitor{..}
  pkg
  srcdir
  setupRequiresBuild
  depsBuildStatus = do
    -- TODO: [nice to have] some debug-level message about file
    -- changes, like rerunIfChanged
    configChanged <-
      checkFileMonitorChanged
        pkgFileMonitorConfig
        srcdir
        pkgconfig
    case configChanged of
      MonitorChanged monitorReason ->
        return (Left (BuildStatusConfigure $ ConfigureMonitorChanged $ void monitorReason))
      MonitorUnchanged () _
        -- A package is configured by its setup, so a new setup means
        -- configuring again. The configuration monitor only notices once the
        -- new setup exists (see 'updatePackageConfigFileMonitor'), which is
        -- after this check.
        | setupRequiresBuild ->
            return (Left (BuildStatusConfigure ConfigureSetupRebuilt))
        -- The configChanged here includes the identity of the dependencies,
        -- so depsBuildStatus is just needed for the changes in the content
        -- of dependencies.
        | any buildStatusRequiresBuild depsBuildStatus -> do
            regChanged <- checkFileMonitorChanged pkgFileMonitorReg srcdir ()
            let mreg = changedToMaybe regChanged
            return (Left (BuildStatusBuild mreg BuildReasonDepsRebuilt))
        | otherwise -> do
            buildChanged <-
              checkFileMonitorChanged
                pkgFileMonitorBuild
                srcdir
                buildComponents
            regChanged <-
              checkFileMonitorChanged
                pkgFileMonitorReg
                srcdir
                ()
            let mreg = changedToMaybe regChanged
            case (buildChanged, regChanged) of
              (MonitorChanged (MonitoredValueChanged prevBuildComponents), _) ->
                return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonExtraTargets prevBuildComponents
              (MonitorChanged monitorReason, _) ->
                return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonFilesChanged $ void monitorReason
              (MonitorUnchanged _ _, MonitorChanged monitorReason) ->
                -- this should only happen if the file is corrupt or been
                -- manually deleted. We don't want to bother with another
                -- phase just for this, so we'll reregister by doing a build.
                return (Left (BuildStatusBuild Nothing buildReason))
                where
                  buildReason = BuildReasonFilesChanged $ void monitorReason
              (MonitorUnchanged _ _, MonitorUnchanged _ _)
                | pkgHasEphemeralBuildTargets pkg ->
                    return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonEphemeralTargets
              (MonitorUnchanged buildResult _, MonitorUnchanged _ _) ->
                return $
                  Right
                    BuildResult
                      { buildResultDocs = docsResult
                      , buildResultTests = testsResult
                      , buildResultLogFile = Nothing
                      , buildResultBenchmark = Nothing
                      }
                where
                  (docsResult, testsResult) = buildResult
    where
      (pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg
      changedToMaybe :: MonitorChanged a b -> Maybe b
      changedToMaybe (MonitorChanged _) = Nothing
      changedToMaybe (MonitorUnchanged x _) = Just x

updatePackageConfigFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> ElaboratedConfiguredPackage
  -> Maybe ElaboratedSetup
  -- ^ The setup the package is configured with, if it has one
  -> IO ()
updatePackageConfigFileMonitor
  PackageFileMonitor{pkgFileMonitorConfig}
  srcdir
  pkg
  mElabSetup = do
    -- The package has to be configured again when its setup changes. That
    -- holds even if this run stops before it gets to do so, hence a monitor
    -- on the setup that outlives the run.
    setupMonitors <- maybe (return []) setupExeMonitors mElabSetup
    updateFileMonitor
      pkgFileMonitorConfig
      srcdir
      Nothing
      setupMonitors
      pkgconfig
      ()
    where
      (pkgconfig, _buildComponents) = packageFileMonitorKeyValues pkg

updatePackageBuildFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> MonitorTimestamp
  -> ElaboratedConfiguredPackage
  -> BuildStatusRebuild
  -> [MonitorFilePath]
  -> BuildResultMisc
  -> IO ()
updatePackageBuildFileMonitor
  PackageFileMonitor{pkgFileMonitorBuild}
  srcdir
  timestamp
  pkg
  pkgBuildStatus
  monitors
  buildResult =
    updateFileMonitor
      pkgFileMonitorBuild
      srcdir
      (Just timestamp)
      monitors
      buildComponents'
      buildResult
    where
      (_pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg

      -- If the only thing that's changed is that we're now building extra
      -- components, then we can avoid later unnecessary rebuilds by saving the
      -- total set of components that have been built, namely the union of the
      -- existing ones plus the new ones. If files also changed this would be
      -- the wrong thing to do. Note that we rely on the
      -- fileMonitorCheckIfOnlyValueChanged = True mode to get this guarantee
      -- that it's /only/ the value that changed not any files that changed.
      buildComponents' =
        case pkgBuildStatus of
          BuildStatusBuild _ (BuildReasonExtraTargets prevBuildComponents) ->
            buildComponents `Set.union` prevBuildComponents
          _ -> buildComponents

updatePackageRegFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> Maybe InstalledPackageInfo
  -> IO ()
updatePackageRegFileMonitor
  PackageFileMonitor{pkgFileMonitorReg}
  srcdir
  mipkg =
    updateFileMonitor
      pkgFileMonitorReg
      srcdir
      Nothing
      []
      ()
      mipkg

invalidatePackageRegFileMonitor :: PackageFileMonitor -> IO ()
invalidatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg} =
  removeFileForcibly (fileMonitorCacheFile pkgFileMonitorReg)

--------------------------------------------------------------------------------

-- * The file monitor of a setup

--------------------------------------------------------------------------------

-- | Says whether a setup built in the project's build directory is up to
-- date. Its value is the setup it was built for; the files are the sources
-- of the setup, the executables built from them, and what tells that an
-- inplace dependency was rebuilt.
newSetupFileMonitor
  :: ElaboratedSharedConfig
  -> DistDirLayout
  -> ElaboratedSetup
  -> FileMonitor ElaboratedSetup ()
newSetupFileMonitor shared layout elabSetup =
  newFileMonitor (elabSetupMonitorFile layout shared elabSetup)

-- | Does a setup need rebuilding?
checkSetupFileMonitorChanged
  :: FileMonitor ElaboratedSetup ()
  -> ElaboratedSetup
  -> FilePath
  -> [BuildStatus]
  -> IO (Either BuildStatusRebuild BuildResult)
checkSetupFileMonitorChanged monitor elabSetup srcdir depsBuildStatus = do
  changed <- checkFileMonitorChanged monitor srcdir elabSetup
  -- The executable is not a monitored file: it is written during the build,
  -- which the monitor takes for a change.
  setupExeExists <- doesFileExist setupExe
  return $ case changed of
    MonitorChanged monitorReason ->
      Left (BuildStatusBuild Nothing (BuildReasonFilesChanged (void monitorReason)))
    MonitorUnchanged () _
      | any buildStatusRequiresBuild depsBuildStatus ->
          Left (BuildStatusBuild Nothing BuildReasonDepsRebuilt)
      | not setupExeExists ->
          Left (BuildStatusBuild Nothing (BuildReasonFilesChanged (MonitoredFileChanged setupExe)))
      | otherwise ->
          Right
            BuildResult
              { buildResultDocs = DocsNotTried
              , buildResultTests = TestsNotTried
              , buildResultLogFile = Nothing
              , buildResultBenchmark = Nothing
              }
  where
    setupExe = elabSetupExeDir elabSetup </> "setup" <.> exeExtension buildPlatform

updateSetupFileMonitor
  :: FileMonitor ElaboratedSetup ()
  -> FilePath
  -> MonitorTimestamp
  -> ElaboratedSetup
  -> [MonitorFilePath]
  -> IO ()
updateSetupFileMonitor monitor srcdir timestamp elabSetup monitors =
  updateFileMonitor monitor srcdir (Just timestamp) monitors elabSetup ()

-- | Monitors on the executables of a setup: they change when the setup is
-- built anew and comes out different.
setupExeMonitors :: ElaboratedSetup -> IO [MonitorFilePath]
setupExeMonitors elabSetup =
  for ["setup", "hooks"] $ \name -> do
    let exe = elabSetupExeDir elabSetup </> name <.> exeExtension buildPlatform
    -- A setup need not have a hooks executable.
    exists <- doesFileExist exe
    return $
      if exists
        then monitorFileHashed exe
        else monitorNonExistentFile exe
