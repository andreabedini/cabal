import Test.Cabal.Prelude
-- Test that Paths module is generated and usable when relocatable is turned on.

main = setupAndCabalTest $ do
  skipIfWindows
  -- FIXME: remove
  
  setup_build ["--enable-relocatable"]
