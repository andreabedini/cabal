import Test.Cabal.Prelude

main = cabalTest $ do
    
    withProjectFile "cabal.internal.project" $ do
        cabal "v2-build" ["mylib"]
