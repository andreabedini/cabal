------------------------------------------------------------------------------- |
-- Module      :  Distribution.Client.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal fetch command
-----------------------------------------------------------------------------
module Distribution.Client.Fetch
  ( fetch
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Dependency
import Distribution.Client.FetchUtils hiding (fetchPackage)
import Distribution.Client.IndexUtils as IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import Distribution.Client.Setup
  ( FetchFlags (..)
  , GlobalFlags (..)
  , RepoContext (..)
  )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.Targets
import Distribution.Client.Types

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, readPkgConfigDb)
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage

import Distribution.Package
  ( packageId
  )
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBStack
  , compilerInfo
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
  ( ProgramDb
  )
import Distribution.Simple.Setup
  ( fromFlag
  , fromFlagOrDefault
  )
import Distribution.Simple.Utils
  ( debug
  , die'
  , notice
  )
import Distribution.System
  ( Platform
  )

-- ------------------------------------------------------------

-- * The fetch command

-- ------------------------------------------------------------

-- TODO:

-- * add fetch -o support

-- * support tarball URLs via ad-hoc download cache (or in -o mode?)

-- * suggest using --no-deps, unpack or fetch -o if deps cannot be satisfied

-- * Port various flags from install:

--   * --upgrade-dependencies
--   * --constraint and --preference
--   * --only-dependencies, but note it conflicts with --no-deps

-- | Fetch a list of packages and their dependencies.
fetch
  :: logger
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> Platform
  -> ProgramDb
  -> GlobalFlags
  -> FetchFlags
  -> [UserTarget]
  -> IO ()
fetch logger _ _ _ _ _ _ _ [] =
  notice logger "No packages requested. Nothing to do."
fetch
  logger
  packageDBs
  repoCtxt
  comp
  platform
  progdb
  _
  fetchFlags
  userTargets = do
    traverse_ (checkTarget logger) userTargets

    installedPkgIndex <- getInstalledPackages logger comp packageDBs progdb
    sourcePkgDb <- getSourcePackages logger repoCtxt
    pkgConfigDb <- readPkgConfigDb logger progdb

    pkgSpecifiers <-
      resolveUserTargets
        logger
        repoCtxt
        (packageIndex sourcePkgDb)
        userTargets

    pkgs <-
      planPackages
        logger
        comp
        platform
        fetchFlags
        installedPkgIndex
        sourcePkgDb
        pkgConfigDb
        pkgSpecifiers

    pkgs' <- filterM (fmap not . isFetched . srcpkgSource) pkgs
    if null pkgs'
      then -- TODO: when we add support for remote tarballs then this message
      -- will need to be changed because for remote tarballs we fetch them
      -- at the earlier phase.

        notice logger $
          "No packages need to be fetched. "
            ++ "All the requested packages are already local "
            ++ "or cached locally."
      else
        if dryRun
          then
            notice logger $
              unlines $
                "The following packages would be fetched:"
                  : map (prettyShow . packageId) pkgs'
          else traverse_ (fetchPackage logger repoCtxt . srcpkgSource) pkgs'
    where
      dryRun = fromFlag (fetchDryRun fetchFlags)

planPackages
  :: logger
  -> Compiler
  -> Platform
  -> FetchFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> PkgConfigDb
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> IO [UnresolvedSourcePackage]
planPackages
  logger
  comp
  platform
  fetchFlags
  installedPkgIndex
  sourcePkgDb
  pkgConfigDb
  pkgSpecifiers
    | includeDependencies = do
        solver <-
          chooseSolver
            logger
            (fromFlag (fetchSolver fetchFlags))
            (compilerInfo comp)
        notice logger "Resolving dependencies..."
        installPlan <-
          foldProgress logMsg (die' logger) return $
            resolveDependencies
              platform
              (compilerInfo comp)
              pkgConfigDb
              solver
              resolverParams

        -- The packages we want to fetch are those packages the 'InstallPlan'
        -- that are in the 'InstallPlan.Configured' state.
        return
          [ solverPkgSource cpkg
          | (SolverInstallPlan.Configured cpkg) <-
              SolverInstallPlan.toList installPlan
          ]
    | otherwise =
        either (die' logger . unlines . map show) return $
          resolveWithoutDependencies resolverParams
    where
      resolverParams :: DepResolverParams
      resolverParams =
        setMaxBackjumps
          ( if maxBackjumps < 0
              then Nothing
              else Just maxBackjumps
          )
          . setIndependentGoals independentGoals
          . setReorderGoals reorderGoals
          . setCountConflicts countConflicts
          . setFineGrainedConflicts fineGrainedConflicts
          . setMinimizeConflictSet minimizeConflictSet
          . setShadowPkgs shadowPkgs
          . setStrongFlags strongFlags
          . setAllowBootLibInstalls allowBootLibInstalls
          . setOnlyConstrained onlyConstrained
          . setSolverVerbosity logger
          . addConstraints
            [ let pc =
                    PackageConstraint
                      (scopeToplevel $ pkgSpecifierTarget pkgSpecifier)
                      (PackagePropertyStanzas stanzas)
               in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            | pkgSpecifier <- pkgSpecifiers
            ]
          -- Reinstall the targets given on the command line so that the dep
          -- resolver will decide that they need fetching, even if they're
          -- already installed. Since we want to get the source packages of
          -- things we might have installed (but not have the sources for).
          . reinstallTargets
          $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

      includeDependencies = fromFlag (fetchDeps fetchFlags)
      logMsg message rest = debug logger message >> rest

      stanzas =
        [TestStanzas | testsEnabled]
          ++ [BenchStanzas | benchmarksEnabled]
      testsEnabled = fromFlagOrDefault False $ fetchTests fetchFlags
      benchmarksEnabled = fromFlagOrDefault False $ fetchBenchmarks fetchFlags

      reorderGoals = fromFlag (fetchReorderGoals fetchFlags)
      countConflicts = fromFlag (fetchCountConflicts fetchFlags)
      fineGrainedConflicts = fromFlag (fetchFineGrainedConflicts fetchFlags)
      minimizeConflictSet = fromFlag (fetchMinimizeConflictSet fetchFlags)
      independentGoals = fromFlag (fetchIndependentGoals fetchFlags)
      shadowPkgs = fromFlag (fetchShadowPkgs fetchFlags)
      strongFlags = fromFlag (fetchStrongFlags fetchFlags)
      maxBackjumps = fromFlag (fetchMaxBackjumps fetchFlags)
      allowBootLibInstalls = fromFlag (fetchAllowBootLibInstalls fetchFlags)
      onlyConstrained = fromFlag (fetchOnlyConstrained fetchFlags)

checkTarget :: LogAction IO (Message String) UserTarget -> IO ()
checkTarget logger target = case target of
  UserTargetRemoteTarball _uri ->
    die' logger $
      "The 'fetch' command does not yet support remote tarballs. "
        ++ "In the meantime you can use the 'unpack' commands."
  _ -> return ()

fetchPackage :: LogAction IO (Message String) RepoContext -> PackageLocation a -> IO ()
fetchPackage logger repoCtxt pkgsrc = case pkgsrc of
  LocalUnpackedPackage _dir -> return ()
  LocalTarballPackage _file -> return ()
  RemoteTarballPackage _uri _ ->
    die' logger $
      "The 'fetch' command does not yet support remote tarballs. "
        ++ "In the meantime you can use the 'unpack' commands."
  RemoteSourceRepoPackage _repo _ ->
    die' logger $
      "The 'fetch' command does not yet support remote "
        ++ "source repositories."
  RepoTarballPackage repo pkgid _ -> do
    _ <- fetchRepoTarball logger repoCtxt repo pkgid
    return ()
