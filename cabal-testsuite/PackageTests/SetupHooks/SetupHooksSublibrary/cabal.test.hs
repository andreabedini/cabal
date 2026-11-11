import Test.Cabal.Prelude

import Data.List (isInfixOf)

-- A package with build-type: Hooks and an internal library. cabal-install
-- builds internal libraries per component only, so this checks that a Hooks
-- package is built per component: the pre-build rule of SetupHooks.hs
-- generates the only module of the sublibrary, which the main library imports.
--
-- The components share one setup, which is compiled once. The package also
-- has an executable called "setup", which is a different thing.
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    -- As for BuildToolPaths: Cabal-hooks comes from the in-tree Cabal library.
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do
      r <- cabal' "v2-build" ["all"]
      assertOutputContains "setup-hooks-sublibrary-test-0.1 (setup) (first run)" r
      assertOutputContains "setup-hooks-sublibrary-test-0.1 (lib:sub) (first run)" r
      assertOutputContains "setup-hooks-sublibrary-test-0.1 (exe:setup) (first run)" r
      assertEqual
        "number of times SetupHooks.hs is compiled"
        1
        (length (filter ("Compiling SetupHooks" `isInfixOf`) (lines (resultOutput r))))
