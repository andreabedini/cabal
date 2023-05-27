import Test.Cabal.Prelude

main = cabalTest $ do
    
    withProjectFile "cabal.external.project" $ do
        cabal "v2-build" ["mylib"]
