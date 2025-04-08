{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- The layout of the .\/dist\/ directory where cabal keeps all of its state
-- and build artifacts.
module Distribution.Client.DistDirLayout
  ( -- * 'DistDirLayout'
    DistDirLayout (..)
  , DistDirParams (..)
  , defaultDistDirLayout

    -- * 'ProjectRoot'
  , ProjectRoot (..)
  , defaultProjectFile

    -- * 'StoreDirLayout'
  , StoreDirLayout (..)
  , defaultStoreDirLayout

    -- * 'CabalDirLayout'
  , CabalDirLayout (..)
  , mkCabalDirLayout
  , defaultCabalDirLayout
  , ProjectPath
  , StorePath
  , interpretStorePath
  , interpretPackagePath
  , PackageDBH
  , PackageDBStackH
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Config
  ( defaultLogsDir
  , defaultStoreDir
  )
import Distribution.Compiler
import Distribution.Package
  ( ComponentId
  , PackageId
  , UnitId
  )
import Distribution.Simple.Compiler
  ( Compiler (..)
  , OptimisationLevel (..)
  , PackageDBX (..)
  , PackageDBS
  , PackageDBStackS
  )
import Distribution.System
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Distribution.Utils.Path

-- | Information which can be used to construct the path to
-- the build directory of a build.  This is LESS fine-grained
-- than what goes into the hashed 'InstalledPackageId',
-- and for good reason: we don't want this path to change if
-- the user, say, adds a dependency to their project.
data DistDirParams = DistDirParams
  { distParamUnitId :: UnitId
  , distParamPackageId :: PackageId
  , distParamComponentId :: ComponentId
  , distParamComponentName :: Maybe ComponentName
  , distParamCompilerId :: CompilerId
  , distParamPlatform :: Platform
  , distParamOptimization :: OptimisationLevel
  -- TODO (see #3343):
  --  Flag assignments
  --  Optimization
  }

data ProjectRootDir
type ProjectPath = SymbolicPath ProjectRootDir

-- | The layout of the project state directory. Traditionally this has been
-- called the @dist@ directory.
data DistDirLayout = DistDirLayout
  { distProjectRootDirectory :: AbsolutePath (Dir ProjectRootDir)
  -- ^ The root directory of the project. Many other files are relative to
  -- this location (e.g. the @cabal.project@ file).
  , distProjectFile :: String -> AbsolutePath File
  -- ^ The @cabal.project@ file and related like @cabal.project.freeze@.
  -- The parameter is for the extension, like \"freeze\", or \"\" for the
  -- main file.
  , distDirectory :: ProjectPath (Dir Dist)
  -- ^ The \"dist\" directory, which is the root of where cabal keeps all
  -- its state including the build artifacts from each package we build.
  , distBuildDirectory :: DistDirParams -> ProjectPath (Dir Build)
  -- ^ The directory under dist where we keep the build artifacts for a
  -- package we're building from a local directory.
  --
  -- This uses a 'UnitId' not just a 'PackageName' because technically
  -- we can have multiple instances of the same package in a solution
  -- (e.g. setup deps).
  , distBuildRootDirectory :: ProjectPath (Dir Build)
  , distDownloadSrcDirectory :: ProjectPath (Dir Source)
  -- ^ The directory under dist where we download tarballs and source
  -- control repos to.
  , distUnpackedSrcDirectory :: PackageId -> ProjectPath (Dir Source)
  -- ^ The directory under dist where we put the unpacked sources of
  -- packages, in those cases where it makes sense to keep the build
  -- artifacts to reduce rebuild times.
  , distUnpackedSrcRootDirectory :: ProjectPath (Dir OtherDir)
  , distProjectCacheFile :: String -> ProjectPath (Dir OtherDir)
  -- ^ The location for project-wide cache files (e.g. state used in
  -- incremental rebuilds).
  , distProjectCacheDirectory :: ProjectPath (Dir OtherDir)
  , distPackageCacheFile :: DistDirParams -> String -> ProjectPath File
  -- ^ The location for package-specific cache files (e.g. state used in
  -- incremental rebuilds).
  , distPackageCacheDirectory :: DistDirParams -> ProjectPath (Dir OtherDir)
  , distSdistFile :: PackageId -> (ProjectPath File)
  -- ^ The location that sdists are placed by default.
  , distSdistDirectory :: (ProjectPath (Dir OtherDir))
  , distTempDirectory :: ProjectPath (Dir Tmp)
  , distBinDirectory :: (ProjectPath (Dir OtherDir))
  , distPackageDB :: CompilerId -> PackageDBX (ProjectPath (Dir PkgDB))
  , distHaddockOutputDir :: Maybe (ProjectPath (Dir OtherDir))
  -- ^ Is needed when `--haddock-output-dir` flag is used.
  }

interpretPackagePath :: DistDirLayout -> ProjectPath a -> FilePath
interpretPackagePath DistDirLayout{..} path =
  interpretSymbolicPathAbsolute distProjectRootDirectory path

type PackageDBH  = PackageDBS HomeDir
type PackageDBStackH = PackageDBStackS HomeDir

-- | The layout of a cabal nix-style store.
data StoreDirLayout = StoreDirLayout
  { storeRootDirectory :: SymbolicPath HomeDir (Dir StoreDir)
  , storeCompilerDirectory :: Compiler -> StorePath (Dir OtherDir)
  -- ^ The root directory of the store, which is a directory
  , storePackageDirectory :: Compiler -> UnitId -> StorePath (Dir Pkg)
  
  , storePackageDBPath :: Compiler -> StorePath (Dir PkgDB)
  , storePackageDB :: Compiler -> PackageDBS StoreDir

  , storeIncomingDirectory :: Compiler -> StorePath (Dir OtherDir)
  , storeIncomingLock :: Compiler -> UnitId -> StorePath File
  }

interpretStorePath :: StoreDirLayout -> StorePath a -> FilePath
interpretStorePath StoreDirLayout{..} path =
  interpretSymbolicPath Nothing (storeRootDirectory </> path)

-- TODO: move to another module, e.g. CabalDirLayout?
-- or perhaps rename this module to DirLayouts.

-- | The layout of the user-wide cabal directory, that is the @~/.cabal@ dir
-- on unix, and equivalents on other systems.
--
-- At the moment this is just a partial specification, but the idea is
-- eventually to cover it all.
data CabalDirLayout = CabalDirLayout
  { cabalStoreDirLayout :: StoreDirLayout
  , cabalLogsDirectory :: FilePath
  }

-- | Information about the root directory of the project.
--
-- It can either be an implicit project root in the current dir if no
-- @cabal.project@ file is found, or an explicit root if either
-- the file is found or the project root directory was specified.
data ProjectRoot
  = -- | An implicit project root. It contains the absolute project
    -- root dir.
    ProjectRootImplicit (AbsolutePath (Dir ProjectRootDir))
  | -- | An explicit project root. It contains the absolute project
    -- root dir and the relative @cabal.project@ file (or explicit override)
    ProjectRootExplicit (AbsolutePath (Dir ProjectRootDir)) (RelativePath ProjectRootDir File)
  | -- | An explicit, absolute project root dir and an explicit, absolute
    -- @cabal.project@ file.
    ProjectRootExplicitAbsolute (AbsolutePath (Dir ProjectRootDir)) (AbsolutePath File)
  deriving (Eq, Show)

-- defaultProjectFilename :: FilePath
-- defaultProjectFilename = "cabal.project"

defaultProjectFile :: RelativePath ProjectRootDir File
defaultProjectFile = makeRelativePathEx "cabal.project"

-- | Make the default 'DistDirLayout' based on the project root dir and
-- optional overrides for the location of the @dist@ directory, the
-- @cabal.project@ file and the documentation directory.
defaultDistDirLayout
  :: ProjectRoot
  -- ^ the project root
  -> Maybe (ProjectPath (Dir Dist))
  -- ^ the @dist@ directory (relative to project root)
  -> Maybe (ProjectPath (Dir OtherDir))
  -- ^ the documentation directory
  -> DistDirLayout
defaultDistDirLayout projectRoot mdistDirectory haddockOutputDir =
  DistDirLayout {..}
  where
    (projectRootDir, projectFile) = case projectRoot of
      ProjectRootImplicit dir -> (dir, dir </> defaultProjectFile)
      ProjectRootExplicit dir file -> (dir, dir </> file)
      ProjectRootExplicitAbsolute dir file -> (dir, file)

    distProjectRootDirectory = projectRootDir

    distProjectFile ext = projectFile <.> ext

    distDirectory :: ProjectPath (Dir Dist)
    distDirectory = fromMaybe (makeSymbolicPath "dist-newstyle") mdistDirectory

    distBuildRootDirectory :: ProjectPath (Dir Build)
    distBuildRootDirectory = distDirectory </> makeRelativePathEx "build"

    distBuildDirectory :: DistDirParams -> ProjectPath (Dir Build)
    distBuildDirectory params =
      distBuildRootDirectory
        </> makeRelativePathEx (prettyShow (distParamPlatform params))
        </> makeRelativePathEx (prettyShow (distParamCompilerId params))
        </> makeRelativePathEx (prettyShow (distParamPackageId params))
        </> makeRelativePathEx ( case distParamComponentName params of
                Nothing -> ""
                Just (CLibName LMainLibName) -> ""
                Just (CLibName (LSubLibName name)) -> "l" </> prettyShow name
                Just (CFLibName name) -> "f" </> prettyShow name
                Just (CExeName name) -> "x" </> prettyShow name
                Just (CTestName name) -> "t" </> prettyShow name
                Just (CBenchName name) -> "b" </> prettyShow name
            )
        </> makeRelativePathEx ( case distParamOptimization params of
                NoOptimisation -> "noopt"
                NormalOptimisation -> ""
                MaximumOptimisation -> "opt"
            )
        </> makeRelativePathEx ( let uid_str = prettyShow (distParamUnitId params)
               in if uid_str == prettyShow (distParamComponentId params)
                    then ""
                    else uid_str
            )

    distUnpackedSrcRootDirectory = distDirectory </> makeRelativePathEx "src"

    distUnpackedSrcDirectory pkgid =
      distUnpackedSrcRootDirectory </> makeRelativePathEx (prettyShow pkgid)

    -- we shouldn't get name clashes so this should be fine:
    distDownloadSrcDirectory = distUnpackedSrcRootDirectory

    distProjectCacheDirectory = distDirectory </> makeRelativePathEx "cache"

    distProjectCacheFile name = distProjectCacheDirectory </> makeRelativePathEx name

    distPackageCacheDirectory params = distBuildDirectory params </> makeRelativePathEx "cache"

    distPackageCacheFile params name = distPackageCacheDirectory params </> makeRelativePathEx name

    distSdistFile pid = distSdistDirectory </> makeRelativePathEx (prettyShow pid) <.> "tar.gz"

    distSdistDirectory = distDirectory </> makeRelativePathEx "sdist"

    distTempDirectory = distDirectory </> makeRelativePathEx "tmp"

    distBinDirectory = distDirectory </> makeRelativePathEx "bin"

    distPackageDBPath compid = distDirectory </> makeRelativePathEx "packagedb" </> makeRelativePathEx (prettyShow compid)

    distPackageDB = SpecificPackageDB . distPackageDBPath

    distHaddockOutputDir = haddockOutputDir

data HomeDir
data StoreDir

type StorePath = RelativePath StoreDir

defaultStoreDirLayout :: SymbolicPath HomeDir (Dir StoreDir) -> StoreDirLayout
defaultStoreDirLayout storeRootDirectory =
  StoreDirLayout{..}
  where
    storeCompilerDirectory :: Compiler -> StorePath (Dir OtherDir)
    storeCompilerDirectory compiler =
      case compilerAbiTag compiler of
        NoAbiTag -> makeRelativePathEx (prettyShow (compilerId compiler))
        AbiTag tag -> makeRelativePathEx (prettyShow (compilerId compiler) <> "-" <> tag)

    storePackageDirectory compiler ipkgid =
      storeCompilerDirectory compiler </> makeRelativePathEx (prettyShow ipkgid)

    storePackageDBPath :: Compiler -> StorePath (Dir PkgDB)
    storePackageDBPath compiler =
      storeCompilerDirectory compiler </> makeRelativePathEx "package.db"

    storePackageDB :: Compiler -> PackageDBS StoreDir
    storePackageDB compiler =
      SpecificPackageDB (relativeSymbolicPath (storePackageDBPath compiler))

    storeIncomingDirectory :: Compiler -> StorePath (Dir OtherDir)
    storeIncomingDirectory compiler =
      storeCompilerDirectory compiler </> makeRelativePathEx "incoming"

    storeIncomingLock :: Compiler -> UnitId -> StorePath File
    storeIncomingLock compiler unitid =
      storeIncomingDirectory compiler </> makeRelativePathEx (prettyShow unitid) <.> "lock"

defaultCabalDirLayout :: IO CabalDirLayout
defaultCabalDirLayout =
  mkCabalDirLayout Nothing Nothing

mkCabalDirLayout
  :: Maybe FilePath
  -- ^ Store directory. Must be absolute
  -> Maybe FilePath
  -- ^ Log directory
  -> IO CabalDirLayout
mkCabalDirLayout mstoreDir mlogDir = do
  cabalStoreDirLayout <- defaultStoreDirLayout . makeSymbolicPath <$> do
    maybe defaultStoreDir pure mstoreDir
  cabalLogsDirectory <-
    maybe defaultLogsDir pure mlogDir
  pure $ CabalDirLayout{..}
