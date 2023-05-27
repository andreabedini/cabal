import Test.Cabal.Prelude

main = setupTest $ do
    -- FIXME
    skipIf True
    setup "configure" []
    res <- setup' "build" []
    assertOutputContains "= Post common block elimination =" res
