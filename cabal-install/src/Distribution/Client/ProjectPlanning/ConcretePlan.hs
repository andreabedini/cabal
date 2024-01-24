{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Client.ProjectPlanning.ConcretePlan
  ( ConcreteInstallPlan
  , ConcreteSharedConfig (..)
  , ConcreteConfiguredPackage (..)
  , toConcrete
  , ConcretePlanPackage
  , ConcreteSetupConfig (..)
  , hasValidHaddockTargets
  , normaliseConfiguredPackage
  ) where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Client.InstallPlan (GenericInstallPlan (..), GenericPlanPackage (..))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.PackageHash (PackageHashInputs)
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types.ConfiguredId (ConfiguredId (..))
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Compat.Graph (IsNode (..), fromDistinctList)
import Distribution.Compat.Lens ((^.))
import Distribution.InstalledPackageInfo hiding (dataDir, installedUnitId)
import Distribution.Package (HasUnitId (..), Package (..), UnitId)
import Distribution.Simple (Compiler, OptimisationLevel, PackageDB, PackageDBStack)
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (haddockDirName)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program (ConfiguredProgram (..), Program (..), ProgramDb, addKnownPrograms, builtinPrograms, lookupKnownProgram, lookupProgram)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.System (Platform)
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.ComponentId (ComponentId)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageDescription
import Distribution.Types.PackageDescription.Lens (componentModules)
import Distribution.Types.PackageId (PackageId)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, thisVersion)
import GHC.Generics (Generic)

type ConcreteInstallPlan =
  GenericInstallPlan
    InstalledPackageInfo
    ConcreteConfiguredPackage

type ConcretePlanPackage =
  GenericPlanPackage
    InstalledPackageInfo
    ConcreteConfiguredPackage

-- | TODO: Completely unnecessary but I am following the pattern
data ConcreteSharedConfig = ConcreteSharedConfig
  { cscPlatform :: Platform
  , cscCompiler :: Compiler
  , cscCompilerProgs :: ProgramDb
  , cscReplOptions :: Cabal.ReplOptions
  }
  deriving (Show, Generic)

-- NOTE: Keep PackageDescription out of this!
--
data ConcreteConfiguredPackage = ConcreteConfiguredPackage
  { ccpUnitId :: UnitId
  , ccpPackageId :: PackageId
  , ccpComponentId :: ComponentId
  , ccpComponentName :: Maybe ComponentName
  , ccpPkgOrComp :: ElaboratedPackageOrComponent
  , ccpBuildTargets :: [ComponentTarget]
  , ccpReplTarget :: [ComponentTarget]
  , ccpTestTargets :: [ComponentTarget]
  , ccpBenchTargets :: [ComponentTarget]
  , ccpHaddockTargets :: [ComponentTarget]
  , --
    ccpBuildTargetWholeComponents :: Set ComponentName
  , ccpHasEphemeralBuildTargets :: Bool
  -- ^  FIXME: lots of duplication here!
  , ccpRequiresRegistration :: Bool
  , ccpBuildStyle :: BuildStyle
  , ccpOptimization :: OptimisationLevel
  , ccpOrderDependencies :: [UnitId]
  , ccpSourceLocation :: PackageLocation (Maybe FilePath)
  , ccpSetupConfig :: ConcreteSetupConfig
  , ccpConfigureFlags :: Cabal.ConfigFlags
  , ccpConfigureArgs :: [String]
  , ccpBuildFlags :: Cabal.BuildFlags
  , ccpBuildArgs :: [String]
  , ccpReplFlags :: Cabal.ReplFlags
  , ccpReplArgs :: [String]
  , ccpTestFlags :: Cabal.TestFlags
  , ccpTestArgs :: [String]
  , ccpBenchFlags :: Cabal.BenchmarkFlags
  , ccpBenchArgs :: [String]
  , ccpCopyFlags :: Cabal.CopyFlags
  , ccpRegisterFlags :: Cabal.RegisterFlags
  , ccpHaddockFlags :: Cabal.HaddockFlags
  , ccpHaddockArgs :: [String]
  , ccpPkgDescriptionOverride :: Maybe CabalFileText
  , ccpPackageHashInputs :: PackageHashInputs
  , ccpHaddockTarget :: Cabal.HaddockTarget
  , ccpHaddockDirName :: String
  , ccpBuildHaddocks :: Bool
  , ccpInstallDirs :: InstallDirs.InstallDirs FilePath
  , ccpDataDirEnvVar :: Maybe (String, Maybe FilePath)
  , ccpPackageDbs :: [Maybe PackageDB]
  , ccpSetupPackageDBStack :: PackageDBStack
  , ccpBuildPackageDBStack :: PackageDBStack
  , ccpRegisterPackageDBStack :: PackageDBStack
  , ccpInplaceSetupPackageDBStack :: PackageDBStack
  , ccpInplaceBuildPackageDBStack :: PackageDBStack
  , ccpInplaceRegisterPackageDBStack :: PackageDBStack
  , ccpHaddockExecutables :: Bool
  , ccpHaddockForeignLibs :: Bool
  , ccpHaddockTestSuites :: Bool
  , ccpHaddockBenchmarks :: Bool
  , ccpHaddockInternal :: Bool
  , ccpHasValidHaddockTargets :: Bool
  }
  deriving (Generic)

data ConcreteSetupConfig = ConcreteSetupConfig
  { csoCabalVersion :: VersionRange
  , csoCabalSpecVersion :: Version
  , csoBuildType :: BuildType
  , csoCompiler :: Compiler
  , csoPlatform :: Platform
  , csoProgramDb :: ProgramDb
  , csoPackageDb :: PackageDBStack
  , csoDependencies :: [(ComponentId, PackageId)]
  , csoVersionMacros :: Bool
  }

-- FIXME: csoExtraPathEnv :: [FilePath]

-- TODO:
--
-- still missing
--
-- pkgHasEphemeralBuildTargets
-- elabBuildTargets
-- elabTestTargets
-- elabBenchTargets
-- elabReplTarget
--
-- elabRegisterPackageDBStack
--
-- ## elabPkgDescription
--
-- how do I get rid of it?
--
-- it is used for buildType, setpWrapper and the like
--
-- monitoring is going to be a PITA, e.g. elabInplaceDependencyBuildCacheFiles

-- needElaboratedConfiguredPackage ??

-- TODO: do we need all these instances?

-- instance Binary ConcreteConfiguredPackage
-- instance Structured ConcreteConfiguredPackage

-- instance HasConfiguredId ConcreteConfiguredPackage where
--   configuredId = ccpConfiguredId
--
instance Package ConcreteConfiguredPackage where
  packageId = ccpPackageId

instance HasUnitId ConcreteConfiguredPackage where
  installedUnitId = ccpUnitId

instance IsNode ConcreteConfiguredPackage where
  type Key ConcreteConfiguredPackage = UnitId
  nodeKey = ccpUnitId
  nodeNeighbors = ccpOrderDependencies

toConcrete
  :: ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> (ConcreteInstallPlan, ConcreteSharedConfig)
toConcrete elaboratedPlan elaboratedSharedConfig =
  (concretePlan, concreteSharedConfig)
  where
    concretePlan =
      InstallPlan.new (planIndepGoals elaboratedPlan) $ fromDistinctList $ map convert $ InstallPlan.toList elaboratedPlan

    concreteSharedConfig =
      ConcreteSharedConfig
        { cscPlatform = pkgConfigPlatform elaboratedSharedConfig
        , cscCompiler = pkgConfigCompiler elaboratedSharedConfig
        , cscCompilerProgs = pkgConfigCompilerProgs elaboratedSharedConfig
        , cscReplOptions = pkgConfigReplOptions elaboratedSharedConfig
        }

    convert :: GenericPlanPackage InstalledPackageInfo ElaboratedConfiguredPackage -> GenericPlanPackage InstalledPackageInfo ConcreteConfiguredPackage
    convert (PreExisting ipkg) = PreExisting ipkg
    convert (Configured srcpkg) = Configured (convert' srcpkg)
    convert (Installed srcpkg) = Installed (convert' srcpkg)

    convert' :: ElaboratedConfiguredPackage -> ConcreteConfiguredPackage
    convert' pkg =
      ConcreteConfiguredPackage{..}
      where
        ccpUnitId = installedUnitId pkg
        ccpPackageId = packageId pkg
        ccpComponentId = elabComponentId pkg
        ccpComponentName = elabComponentName pkg -- The name of the component to be built. Nothing if it's a setup dep.
        ccpPkgOrComp = elabPkgOrComp pkg

        ccpPackageHashInputs = packageHashInputs elaboratedSharedConfig pkg
        ccpOrderDependencies = elabOrderDependencies pkg

        ccpRequiresRegistration = elabRequiresRegistration pkg
        ccpOptimization = elabOptimization pkg

        ccpSourceLocation = elabPkgSourceLocation pkg

        ccpBuildStyle = elabBuildStyle pkg
        ccpSetupConfig = concreteSetupConfig pkg elaboratedSharedConfig
        ccpPkgDescriptionOverride = elabPkgDescriptionOverride pkg

        ccpConfigureFlags = setupHsConfigureFlags pkg elaboratedSharedConfig
        ccpConfigureArgs = setupHsConfigureArgs pkg
        --
        ccpBuildFlags = setupHsBuildFlags pkg elaboratedSharedConfig
        ccpBuildArgs = setupHsBuildArgs pkg
        --
        ccpReplFlags = setupHsReplFlags pkg elaboratedSharedConfig
        ccpReplArgs = setupHsReplArgs pkg
        --
        ccpTestFlags = setupHsTestFlags pkg elaboratedSharedConfig
        ccpTestArgs = setupHsTestArgs pkg
        --
        ccpBenchFlags = setupHsBenchFlags pkg elaboratedSharedConfig
        ccpBenchArgs = setupHsBenchArgs pkg
        --
        ccpCopyFlags = setupHsCopyFlags pkg elaboratedSharedConfig
        --
        ccpRegisterFlags = setupHsRegisterFlags pkg elaboratedSharedConfig
        --
        ccpHaddockFlags = setupHsHaddockFlags pkg elaboratedSharedConfig
        ccpHaddockArgs = setupHsHaddockArgs pkg
        --
        --
        --

        --
        -- Targets
        --
        ccpBuildTargets = elabBuildTargets pkg
        ccpReplTarget = elabReplTarget pkg
        ccpTestTargets = elabTestTargets pkg
        ccpBenchTargets = elabBenchTargets pkg
        ccpHaddockTargets = elabHaddockTargets pkg

        ccpInstallDirs = elabInstallDirs pkg
        ccpDataDirEnvVar = ccpDataDirEnvVarForPackage pkg

        --
        -- PackageDBs
        --
        ccpPackageDbs = elabPackageDbs pkg
        ccpSetupPackageDBStack = elabSetupPackageDBStack pkg
        ccpBuildPackageDBStack = elabBuildPackageDBStack pkg
        ccpRegisterPackageDBStack = elabRegisterPackageDBStack pkg
        ccpInplaceSetupPackageDBStack = elabInplaceSetupPackageDBStack pkg
        ccpInplaceBuildPackageDBStack = elabInplaceBuildPackageDBStack pkg
        ccpInplaceRegisterPackageDBStack = elabInplaceRegisterPackageDBStack pkg

        --
        -- Haddock
        --
        ccpBuildHaddocks = elabBuildHaddocks pkg
        ccpHaddockBenchmarks = elabHaddockBenchmarks pkg
        ccpHaddockDirName = haddockDirName ccpHaddockTarget (elabPkgDescription pkg)
        ccpHaddockExecutables = elabHaddockExecutables pkg
        ccpHaddockForeignLibs = elabHaddockForeignLibs pkg
        ccpHaddockInternal = elabHaddockInternal pkg
        ccpHaddockTarget = elabHaddockForHackage pkg
        ccpHaddockTestSuites = elabHaddockTestSuites pkg
        ccpHasValidHaddockTargets = hasValidHaddockTargets pkg

        ccpHasEphemeralBuildTargets =
          (not . null) (elabReplTarget pkg)
            || (not . null) (elabTestTargets pkg)
            || (not . null) (elabBenchTargets pkg)
            || (not . null) (elabHaddockTargets pkg)
            || (not . null) [() | ComponentTarget _ subtarget <- elabBuildTargets pkg, subtarget /= WholeComponent]

        -- \| The components that we'll build all of, meaning that after they're built
        -- we can skip building them again (unlike with building just some modules or
        -- other files within a component).
        ccpBuildTargetWholeComponents =
          Set.fromList [cname | ComponentTarget cname WholeComponent <- elabBuildTargets pkg]

concreteSetupConfig :: ElaboratedConfiguredPackage -> ElaboratedSharedConfig -> ConcreteSetupConfig
concreteSetupConfig pkg pkgShared =
  ConcreteSetupConfig
    { csoCabalVersion = thisVersion (elabSetupScriptCliVersion pkg)
    , csoCabalSpecVersion = elabSetupScriptCliVersion pkg
    , csoBuildType = buildType (elabPkgDescription pkg)
    , csoCompiler = pkgConfigCompiler pkgShared
    , csoPlatform = pkgConfigPlatform pkgShared
    , csoProgramDb = pkgConfigCompilerProgs pkgShared
    , csoPackageDb = elabSetupPackageDBStack pkg
    , csoDependencies =
        [ (uid, srcid)
        | (ConfiguredId srcid (Just (CLibName LMainLibName)) uid, _) <-
            elabSetupDependencies pkg
        ]
    , csoVersionMacros = elabSetupScriptStyle pkg == SetupCustomExplicitDeps
    }

-- | Construct an environment variable that points
-- the package's datadir to its correct location.
--
-- This might be:
--
-- * 'Just' the package's source directory plus the data subdirectory
--   for inplace packages.
-- * 'Nothing' for packages installed in the store (the path was
--   already included in the package at install/build time).
--
-- NOTE: The second file is relative to the source path as described below.
ccpDataDirEnvVarForPackage
  :: ElaboratedConfiguredPackage
  -> Maybe (String, Maybe FilePath)
ccpDataDirEnvVarForPackage pkg =
  case elabBuildStyle pkg of
    BuildAndInstall ->
      Nothing
    BuildInplaceOnly{} ->
      Just
        ( pkgPathEnvVar (elabPkgDescription pkg) "datadir"
        , Just $ dataDir (elabPkgDescription pkg)
        )

hasValidHaddockTargets :: ElaboratedConfiguredPackage -> Bool
hasValidHaddockTargets ElaboratedConfiguredPackage{..}
  | not elabBuildHaddocks = False
  | otherwise = any componentHasHaddocks components
  where
    components :: [ComponentTarget]
    components =
      elabBuildTargets
        ++ elabTestTargets
        ++ elabBenchTargets
        ++ elabReplTarget
        ++ elabHaddockTargets

    componentHasHaddocks :: ComponentTarget -> Bool
    componentHasHaddocks (ComponentTarget name _) =
      case name of
        CLibName LMainLibName -> hasHaddocks
        CLibName (LSubLibName _) -> elabHaddockInternal && hasHaddocks
        CFLibName _ -> elabHaddockForeignLibs && hasHaddocks
        CExeName _ -> elabHaddockExecutables && hasHaddocks
        CTestName _ -> elabHaddockTestSuites && hasHaddocks
        CBenchName _ -> elabHaddockBenchmarks && hasHaddocks
      where
        hasHaddocks = not (null (elabPkgDescription ^. componentModules name))

-- NOTE: This is hard. Does GHC really need to know the package description to
-- figure out whether flags change the behaviour or not? GHC has its own
-- recompilation avoidance mechanism anyway.
normaliseConfiguredPackage
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> ElaboratedConfiguredPackage
normaliseConfiguredPackage ElaboratedSharedConfig{pkgConfigCompilerProgs} pkg =
  pkg{elabProgramArgs = Map.mapMaybeWithKey lookupFilter (elabProgramArgs pkg)}
  where
    knownProgramDb = addKnownPrograms builtinPrograms pkgConfigCompilerProgs

    pkgDesc :: PackageDescription
    pkgDesc = elabPkgDescription pkg

    removeEmpty :: [String] -> Maybe [String]
    removeEmpty [] = Nothing
    removeEmpty xs = Just xs

    lookupFilter :: String -> [String] -> Maybe [String]
    lookupFilter n args = removeEmpty $ case lookupKnownProgram n knownProgramDb of
      Just p -> programNormaliseArgs p (getVersion p) pkgDesc args
      Nothing -> args

    getVersion :: Program -> Maybe Version
    getVersion p = lookupProgram p knownProgramDb >>= programVersion
