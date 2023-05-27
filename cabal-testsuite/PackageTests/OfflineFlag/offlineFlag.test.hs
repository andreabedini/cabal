import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    
    skipIfWindows
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        fails $ cabalG ["--store-dir=" ++ storeDir] "v2-build" ["current", "--offline"]
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["current"]
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["current", "--offline"]
