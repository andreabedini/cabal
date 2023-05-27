import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    r <- fails $ setup' "configure" ["--cabal-file", "Includes2.cabal.fail"]
    assertOutputContains "mysql" r
