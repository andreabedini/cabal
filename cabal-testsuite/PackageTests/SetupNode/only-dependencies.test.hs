import Test.Cabal.Prelude

-- The setup of a package is part of the package, not one of its
-- dependencies.
main = cabalTest . recordMode DoNotRecord $ do
  r <- cabal' "v2-build" ["all", "--only-dependencies"]
  assertOutputDoesNotContain "(setup)" r
