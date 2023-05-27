import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    r <- fails $ setup' "configure" []
    assertOutputContains "MissingReq" r
    return ()
