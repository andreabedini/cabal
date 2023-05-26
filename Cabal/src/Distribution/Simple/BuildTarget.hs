{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.BuildTargets
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified build targets
-----------------------------------------------------------------------------
module Distribution.Simple.BuildTarget (
    -- * Main interface
    readTargetInfos,
    readBuildTargets, -- in case you don't have LocalBuildInfo

    -- * Build targets
    BuildTarget(..),
    showBuildTarget,
    buildTargetComponentName,

    -- * Parsing user build targets
    UserBuildTarget,
    readUserBuildTargets,
    showUserBuildTarget,
    UserBuildTargetProblem(..),
    reportUserBuildTargetProblems,

    -- * Resolving build targets
    resolveBuildTargets,
    BuildTargetProblem(..),
    reportBuildTargetProblems,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.TargetInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.UnqualComponentName

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Pretty
import Distribution.Parsec
import Distribution.Simple.Utils
import Distribution.Verbosity

import qualified Distribution.Compat.CharParsing as P

-- | Take a list of 'String' build targets, and parse and validate them
-- into actual 'TargetInfo's to be built/registered/whatever.
readTargetInfos :: Verbosity -> PackageDescription -> LocalBuildInfo -> [String] -> IO [TargetInfo]
readTargetInfos verbosity pkg_descr lbi args = do
    build_targets <- readBuildTargets verbosity pkg_descr args
    checkBuildTargets verbosity pkg_descr lbi build_targets

-- ------------------------------------------------------------
-- * User build targets
-- ------------------------------------------------------------

-- | Various ways that a user may specify a build target.
--
data UserBuildTarget =
     UserBuildTargetComponent ComponentName
   | UserBuildTargetUnqualifiedComponent UnqualComponentName
  deriving (Show, Eq, Ord)

instance Parsec UserBuildTarget where
  parsec = P.try (UserBuildTargetComponent <$> parsec) <|> (UserBuildTargetUnqualifiedComponent <$> parsec)

-- ------------------------------------------------------------
-- * Resolved build targets
-- ------------------------------------------------------------

-- | A fully resolved build target.
--
data BuildTarget =

     -- | A specific component
     --
     BuildTargetComponent ComponentName
  deriving (Eq, Show, Generic)

instance Binary BuildTarget

buildTargetComponentName :: BuildTarget -> ComponentName
buildTargetComponentName (BuildTargetComponent cn)   = cn

-- | Read a list of user-supplied build target strings and resolve them to
-- 'BuildTarget's according to a 'PackageDescription'. If there are problems
-- with any of the targets e.g. they don't exist or are misformatted, throw an
-- 'IOException'.
readBuildTargets :: Verbosity -> PackageDescription -> [String] -> IO [BuildTarget]
readBuildTargets verbosity pkg targetStrs = do
    let (uproblems, utargets) = readUserBuildTargets targetStrs
    reportUserBuildTargetProblems verbosity uproblems

    let (bproblems, btargets) = resolveBuildTargets pkg utargets
    reportBuildTargetProblems verbosity bproblems

    return btargets


-- ------------------------------------------------------------
-- * Parsing user targets
-- ------------------------------------------------------------

readUserBuildTargets :: [String] -> ([UserBuildTargetProblem]
                                    ,[UserBuildTarget])
readUserBuildTargets = partitionEithers . map readUserBuildTarget

-- |
--
-- >>> readUserBuildTarget "comp"
-- WAS Right (UserBuildTargetSingle "comp")
-- NOW Right (UserBuildTargetUnqualifiedComponent (UnqualComponentName "comp"))
--
-- >>> readUserBuildTarget "lib:comp"
-- WAS Right (UserBuildTargetDouble "lib" "comp")
-- NOW Right (UserBuildTargetComponent (CLibName (LSubLibName (UnqualComponentName "comp"))))
--
-- >>> readUserBuildTarget "\"comp\""
-- WAS Right (UserBuildTargetSingle "comp")
-- NOW Left (UserBuildTargetUnrecognised "\"comp\"")
--
-- >>> readUserBuildTarget "lib:\"comp\""
-- WAS Right (UserBuildTargetDouble "lib" "comp")
-- NOW Left (UserBuildTargetUnrecognised "lib:\"comp\"")
--
-- >>> readUserBuildTarget "lib:comp:more"
-- Left (UserBuildTargetUnrecognised "lib:comp:more")
--
readUserBuildTarget :: String -> Either UserBuildTargetProblem
                                        UserBuildTarget
readUserBuildTarget targetstr =
    case eitherParsec targetstr of
      Left _    -> Left (UserBuildTargetUnrecognised targetstr)
      Right tgt -> Right tgt

data UserBuildTargetProblem
   = UserBuildTargetUnrecognised String
  deriving Show

reportUserBuildTargetProblems :: Verbosity -> [UserBuildTargetProblem] -> IO ()
reportUserBuildTargetProblems verbosity problems = do
    case [ target | UserBuildTargetUnrecognised target <- problems ] of
      []     -> return ()
      target ->
        die' verbosity $ unlines
                [ "Unrecognised build target '" ++ name ++ "'."
                | name <- target ]
           ++ "Examples:\n"
           ++ " - build foo          -- component name "
           ++ "(library, executable, test-suite or benchmark)\n"
           ++ " - build lib:foo exe:foo   -- component qualified by kind\n"

showUserBuildTarget :: UserBuildTarget -> String
showUserBuildTarget (UserBuildTargetComponent cn) = prettyShow cn
showUserBuildTarget (UserBuildTargetUnqualifiedComponent ucn) = prettyShow ucn

-- | Unambiguously render a 'BuildTarget', so that it can
-- be parsed in all situations.
showBuildTarget :: PackageId -> BuildTarget -> String
showBuildTarget pkgid (BuildTargetComponent cn) =
    intercalate ":" [dispKind cn, dispCName cn]
  where
    dispCName = componentStringName pkgid
    dispKind  = showComponentKindShort . componentKind

-- ------------------------------------------------------------
-- * Resolving user targets to build targets
-- ------------------------------------------------------------

-- TODO: Andrea
-- lib:bla is qualified by a kind
-- make that clear in UserBuildTarget (now it says qualified but
-- I am afraid there is a distintion between a component name and
-- a name qualified by kind)

-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to.
--
resolveBuildTargets :: PackageDescription
                    -> [UserBuildTarget]
                    -> ([BuildTargetProblem], [BuildTarget])
resolveBuildTargets pkg = partitionEithers
                        . map (resolveBuildTarget pkg)

resolveBuildTarget :: PackageDescription -> UserBuildTarget
                   -> Either BuildTargetProblem BuildTarget
resolveBuildTarget pkg ubt@(UserBuildTargetComponent cn) =
  case lookupComponent pkg (fixup cn) of
    Nothing -> Left  $ BuildTargetNoSuch ubt []
    Just c -> Right $ BuildTargetComponent (componentName c)
  where
    -- We allow the main library to be specified as a sublibrary with the same name as the package
    fixup (CLibName (LSubLibName ucn))
      | ucn == packageNameToUnqualComponentName (packageName pkg)
      = CLibName LMainLibName
    fixup cn' = cn'
resolveBuildTarget pkg ubt@(UserBuildTargetUnqualifiedComponent ucn) =
  let matches = [cn | cn <- map componentName (pkgComponents pkg), componentNameString cn == Just ucn]
  in case matches of
      []   -> Left  $ BuildTargetNoSuch ubt []
      [cn] -> Right $ BuildTargetComponent cn
      _cis  -> Left  $ BuildTargetAmbiguous ubt []

data BuildTargetProblem
   = BuildTargetExpected  UserBuildTarget [String]  String
     -- ^  [expected thing] (actually got)
   | BuildTargetNoSuch    UserBuildTarget [(String, String)]
     -- ^ [(no such thing,  actually got)]
   | BuildTargetAmbiguous UserBuildTarget [(UserBuildTarget, BuildTarget)]
  deriving Show

reportBuildTargetProblems :: Verbosity -> [BuildTargetProblem] -> IO ()
reportBuildTargetProblems verbosity problems = do

    case [ (t, e, g) | BuildTargetExpected t e g <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [    "Unrecognised build target '" ++ showUserBuildTarget target
            ++ "'.\n"
            ++ "Expected a " ++ intercalate " or " expected
            ++ ", rather than '" ++ got ++ "'."
          | (target, expected, got) <- targets ]

    case [ (t, e) | BuildTargetNoSuch t e <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [    "Unknown build target '" ++ showUserBuildTarget target
            ++ "'.\nThere is no "
            ++ intercalate " or " [ thing ++ " '" ++ got ++ "'"
                                  | (thing, got) <- nosuch ] ++ "."
          | (target, nosuch) <- targets ]

    case [ (t, ts) | BuildTargetAmbiguous t ts <- problems ] of
      []      -> return ()
      targets ->
        die' verbosity $ unlines
          [    "Ambiguous build target '" ++ showUserBuildTarget target
            ++ "'. It could be:\n "
            ++ unlines [ "   "++ showUserBuildTarget ut ++
                         " (" ++ showBuildTargetKind bt ++ ")"
                       | (ut, bt) <- amb ]
          | (target, amb) <- targets ]

  where
    showBuildTargetKind (BuildTargetComponent _  ) = "component"


----------------------------------
-- Top level BuildTarget matcher
--

type ComponentStringName = String

componentStringName :: Package pkg => pkg -> ComponentName -> ComponentStringName
componentStringName pkg (CLibName LMainLibName      ) = prettyShow (packageName pkg)
componentStringName _   (CLibName (LSubLibName name)) = unUnqualComponentName name
componentStringName _   (CFLibName  name) = unUnqualComponentName name
componentStringName _   (CExeName   name) = unUnqualComponentName name
componentStringName _   (CTestName  name) = unUnqualComponentName name
componentStringName _   (CBenchName name) = unUnqualComponentName name

data ComponentKind = LibKind | FLibKind | ExeKind | TestKind | BenchKind
  deriving (Eq, Ord, Show, Enum, Bounded)

componentKind :: ComponentName -> ComponentKind
componentKind (CLibName   _) = LibKind
componentKind (CFLibName  _) = FLibKind
componentKind (CExeName   _) = ExeKind
componentKind (CTestName  _) = TestKind
componentKind (CBenchName _) = BenchKind

showComponentKindShort :: ComponentKind -> String
showComponentKindShort LibKind   = "lib"
showComponentKindShort FLibKind  = "flib"
showComponentKindShort ExeKind   = "exe"
showComponentKindShort TestKind  = "test"
showComponentKindShort BenchKind = "bench"

-- | Check that the given build targets are valid in the current context.
--
-- Also swizzle into a more convenient form.
--
checkBuildTargets :: Verbosity -> PackageDescription -> LocalBuildInfo -> [BuildTarget]
                  -> IO [TargetInfo]
checkBuildTargets _ pkg_descr lbi []      =
    return (allTargetsInBuildOrder' pkg_descr lbi)

checkBuildTargets verbosity pkg_descr lbi targets = do

    let (enabled, disabled) =
          partitionEithers
            [ case componentDisabledReason (componentEnabledSpec lbi) comp of
                Nothing     -> Left  cname
                Just reason -> Right (cname, reason)
            | target <- targets
            , let cname = buildTargetComponentName target
            , let comp = getComponent pkg_descr cname ]

    case disabled of
      []                 -> return ()
      ((cname,reason):_) -> die' verbosity $ formatReason (showComponentName cname) reason

    -- Pick out the actual CLBIs for each of these cnames
    for enabled $ \cname -> do
        case componentNameTargets' pkg_descr lbi cname of
            [] -> error "checkBuildTargets: nothing enabled"
            [target] -> return target
            _targets -> error "checkBuildTargets: multiple copies enabled"

  where
    formatReason cn DisabledComponent =
        "Cannot process the " ++ cn ++ " because the component is marked "
     ++ "as disabled in the .cabal file."
    formatReason cn DisabledAllTests =
        "Cannot process the " ++ cn ++ " because test suites are not "
     ++ "enabled. Run configure with the flag --enable-tests"
    formatReason cn DisabledAllBenchmarks =
        "Cannot process the " ++ cn ++ " because benchmarks are not "
     ++ "enabled. Re-run configure with the flag --enable-benchmarks"
    formatReason cn (DisabledAllButOne cn') =
        "Cannot process the " ++ cn ++ " because this package was "
     ++ "configured only to build " ++ cn' ++ ". Re-run configure "
     ++ "with the argument " ++ cn
