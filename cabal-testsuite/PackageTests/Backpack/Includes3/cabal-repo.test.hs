import Test.Cabal.Prelude
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    
    skipIfWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["exe"]
