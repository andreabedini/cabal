import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    fails $ setup "configure" []
