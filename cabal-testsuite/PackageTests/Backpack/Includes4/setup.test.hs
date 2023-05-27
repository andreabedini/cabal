import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    withPackageDb $ do
      setup_install []
      runExe' "exe" [] >>= assertOutputContains "A (B (A (B"
