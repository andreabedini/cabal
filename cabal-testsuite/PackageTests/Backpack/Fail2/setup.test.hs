import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    r <- fails $ setup' "configure" []
    assertOutputContains "non-existent" r
    return ()
