{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.PackageSpecifier
import Distribution.Compat.Lens
import Distribution.Package
  ( packageName,
    packageVersion,
  )
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple.Command
  ( CommandUI (..),
    usageAlternatives,
  )
import Distribution.Simple.Flag
  ( fromFlagOrDefault,
  )
import Distribution.Simple.Utils (die')
import Distribution.Solver.Types.SourcePackage
import Distribution.Types.GenericPackageDescription.Lens
import Distribution.Verbosity
  ( normal,
  )
import Distribution.Version hiding (hasLowerBound)
import System.FilePath
import Prelude ()

genBoundsCommand :: CommandUI (NixStyleFlags ())
genBoundsCommand =
  CommandUI
    { commandName = "v2-gen-bounds",
      commandSynopsis = "Fill in missing version bounds",
      commandUsage = usageAlternatives "v2-gen-bounds" ["[FLAGS]"],
      commandDescription = Nothing,
      commandNotes = Nothing,
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

  let improveDepedency dep@(Dependency pn vr nes) =
        case Map.lookup pn versionsMap of
          Nothing -> dep
          Just v -> Dependency pn (improveVersionRange vr v) nes

  for_ localPackages $ \case
    SpecificSourcePackage SourcePackage {srcpkgPackageId, srcpkgSource = LocalUnpackedPackage pkgPath, srcpkgDescription, srcpkgDescrOverride = Nothing} -> do
      let PackageIdentifier {pkgName} = srcpkgPackageId

      let fp = pkgPath </> unPackageName pkgName <.> "cabal.revised"
      putStrLn $ "Writing " ++ fp ++ " for " ++ prettyShow srcpkgPackageId

      writeGenericPackageDescription fp $
        srcpkgDescription & (\f -> allCondTrees $ traverseCondTreeC f) %~ map improveDepedency
    anyOtherCase ->
      putStrLn $ "Not handled" ++ show anyOtherCase
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty

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
