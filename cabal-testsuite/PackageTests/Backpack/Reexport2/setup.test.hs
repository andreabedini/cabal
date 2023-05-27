import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    
    fails (setup' "configure" [])
      >>= assertRegex "Expect problem with Asdf" "Asdf"
