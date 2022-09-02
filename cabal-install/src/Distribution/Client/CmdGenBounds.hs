{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | cabal-install CLI command: gen-bounds
module Distribution.Client.CmdGenBounds
  ( genBoundsCommand,
    genBoundsAction,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.Client.Compat.Prelude
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..),
    defaultNixStyleFlags,
    nixStyleOptions,
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
  ( ConfigFlags (..),
    GlobalFlags,
  )
import Distribution.Client.Types.PackageSpecifier
import Distribution.Package
  ( packageName,
    packageVersion,
  )
import Distribution.PackageDescription
import Distribution.Simple.Command
  ( CommandUI (..),
    usageAlternatives,
  )
import Distribution.Simple.Flag
  ( fromFlagOrDefault,
  )
import Distribution.Simple.Utils
  ( die',
    wrapText,
  )
import Distribution.Solver.Types.SourcePackage
import Distribution.Verbosity
  ( normal,
  )
import Distribution.Version hiding (hasLowerBound)
import Text.PrettyPrint
import Prelude ()

genBoundsCommand :: CommandUI (NixStyleFlags ())
genBoundsCommand =
  CommandUI
    { commandName = "v2-gen-bounds",
      commandSynopsis = "Freeze dependencies.",
      commandUsage = usageAlternatives "v2-gen-bounds" ["[FLAGS]"],
      commandDescription = Just $ \_ ->
        wrapText $
          "The project configuration is frozen so that it will be reproducible "
            ++ "in future.\n\n"
            ++ "The precise dependency configuration for the project is written to "
            ++ "the 'cabal.project.genBounds' file (or '$project_file.genBounds' if "
            ++ "'--project-file' is specified). This file extends the configuration "
            ++ "from the 'cabal.project' file and thus is used as the project "
            ++ "configuration for all other commands (such as 'v2-build', "
            ++ "'v2-repl' etc).\n\n"
            ++ "The genBounds file can be kept in source control. To make small "
            ++ "adjustments it may be edited manually, or to make bigger changes "
            ++ "you may wish to delete the file and re-genBounds. For more control, "
            ++ "one approach is to try variations using 'v2-build --dry-run' with "
            ++ "solver flags such as '--constraint=\"pkg < 1.2\"' and once you have "
            ++ "a satisfactory solution to genBounds it using the 'v2-genBounds' command "
            ++ "with the same set of flags.",
      commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-gen-bounds\n"
          ++ "    Freeze the configuration of the current project\n\n"
          ++ "  "
          ++ pname
          ++ " v2-build --dry-run --constraint=\"aeson < 1\"\n"
          ++ "    Check what a solution with the given constraints would look like\n"
          ++ "  "
          ++ pname
          ++ " v2-gen-bounds --constraint=\"aeson < 1\"\n"
          ++ "    Freeze a solution using the given constraints\n",
      commandDefaultFlags = defaultNixStyleFlags (),
      commandOptions = nixStyleOptions (const [])
    }

-- | To a first approximation, the @genBounds@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- based on the install plan we write out a @cabal.project.genBounds@ config file.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
genBoundsAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
genBoundsAction flags@NixStyleFlags {configFlags} extraArgs globalFlags = do
  unless (null extraArgs) $
    die' verbosity $
      "'genBounds' doesn't take any extra arguments: "
        ++ unwords extraArgs

  ProjectBaseContext
    { distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages

  let versionsMap =
        Map.fromList $
          [ (packageName ipi, packageVersion ipi)
            | InstallPlan.PreExisting ipi <- InstallPlan.toList elaboratedPlan
          ]
            ++ [ (packageName ecp, packageVersion ecp)
                 | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan,
                   not $ elabLocalToProject ecp
               ]

  for_ localPackages $ \case
    np@(NamedPackage _pn _pps) ->
      putStrLn $ "\n TODO idk named package " ++ show np
    (SpecificSourcePackage ssp) -> do
      let SourcePackage {srcpkgPackageId = PackageIdentifier {pkgName}, srcpkgDescription} = ssp

      -- We only work with the accumulated constraints (for the given
      -- configuration)

      putStrLn $ "\nfor package " ++ unPackageName pkgName

      for_ (condLibrary srcpkgDescription) $ \ct -> do
        putStrLn "\nlibrary"
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)

      for_ (condSubLibraries srcpkgDescription) $ \(ucn, ct) -> do
        putStrLn $ "\nlibrary " ++ unUnqualComponentName ucn
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)

      for_ (condForeignLibs srcpkgDescription) $ \(ucn, ct) -> do
        putStrLn $ "\nforeign-lib " ++ unUnqualComponentName ucn
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)

      for_ (condExecutables srcpkgDescription) $ \(ucn, ct) -> do
        putStrLn $ "\nexecutable " ++ unUnqualComponentName ucn
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)

      for_ (condTestSuites srcpkgDescription) $ \(ucn, ct) -> do
        putStrLn $ "\ntest-suite " ++ unUnqualComponentName ucn
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)

      for_ (condBenchmarks srcpkgDescription) $ \(ucn, ct) -> do
        putStrLn $ "\nbenchmark " ++ unUnqualComponentName ucn
        print (nest 4 $ vcat $ map (message versionsMap) $ condTreeConstraints ct)
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty

message :: Map PackageName Version -> Dependency -> Doc
message versionsMap dep =
  let dep' = improveDepedency versionsMap dep
   in if dep' == dep
        then pretty dep
        else hsep [pretty dep', "was", pretty dep]

improveDepedency :: Map PackageName Version -> Dependency -> Dependency
improveDepedency versionsMap dep@(Dependency pn vr nes) =
  case Map.lookup pn versionsMap of
    Nothing -> dep
    Just v -> Dependency pn (improveVersionRange vr v) nes

improveVersionRange :: VersionRange -> Version -> VersionRange
improveVersionRange vr v =
  simplifyVersionRange . addMV v . addUB v . addLB v $ vr

addLB :: Version -> VersionRange -> VersionRange
addLB v vr
  | hasLowerBound vr = vr
  | otherwise = intersectVersionRanges vr (laterVersion v)

addUB :: Version -> VersionRange -> VersionRange
addUB v vr
  | hasUpperBound vr = vr
  | otherwise = intersectVersionRanges vr (earlierVersion $ majorUpperBound v)

addMV :: Version -> VersionRange -> VersionRange
addMV v vr = unionVersionRanges vr (majorBoundVersion v)

-- The documentation of Distribution.Types.VersioRange.hasLowerBound says
-- > Note: this function only considers the user-specified lower bounds,
-- > but not the implicit >=0 lower bound.
-- but hasLowerBound (orLaterVersion $ mkVersion [0]) = True
-- This function should give the right answer
hasLowerBound :: VersionRange -> Bool
hasLowerBound = not . withinRange (mkVersion [0])
