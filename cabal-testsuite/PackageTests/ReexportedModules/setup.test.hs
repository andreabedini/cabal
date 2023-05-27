import Test.Cabal.Prelude
-- Test that reexported modules build correctly
main = setupAndCabalTest $ do
    
    withPackageDb $ do
        withDirectory "p" $ setup_install ["--cabal-file", "p.cabal"]
        withDirectory "q" $ setup_build []
