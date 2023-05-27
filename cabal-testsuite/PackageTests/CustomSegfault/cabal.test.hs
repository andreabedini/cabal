import Test.Cabal.Prelude
main = cabalTest $ do
    -- TODO: this test ought to work on Windows too
    skipUnless "not Linux" =<< isLinux
    
    fails $ cabal' "v2-build" [] >>= assertOutputContains "SIGSEGV"
