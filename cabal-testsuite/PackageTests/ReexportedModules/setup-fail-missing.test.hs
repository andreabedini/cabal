import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    withDirectory "p" $ do
        r <- fails $ setup' "configure" ["--cabal-file", "p.cabal.fail-missing"]
        assertOutputContains "Missing" r
