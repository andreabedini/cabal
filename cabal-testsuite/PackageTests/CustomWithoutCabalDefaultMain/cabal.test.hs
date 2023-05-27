import Test.Cabal.Prelude
main = cabalTest $ do

    -- This package has explicit setup dependencies that do not include Cabal.
    -- Compilation should fail because Setup.hs imports Distribution.Simple.
    r <- fails $ cabal' "v2-build" ["custom-setup-without-cabal-defaultMain"]
    assertRegex "Should not have been able to import Cabal"
                "(Could not (find|load) module|Failed to load interface for).*Distribution\\.Simple" r
