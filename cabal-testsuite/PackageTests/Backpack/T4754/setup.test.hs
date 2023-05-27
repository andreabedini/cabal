import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    skipUnless "no profiling libs" =<< hasProfiledLibraries
    setup "configure" ["--enable-profiling"]
    setup "build" []
