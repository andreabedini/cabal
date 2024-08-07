import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Invalid Windows filepath.
main = do
  -- A directory named like `n?ul` on Windows will make external
  -- tools like git — and hence the whole testsuite — error.
  skipIfWindows "uninteresting"
  cabalTest $ do
    cwd <- testCurrentDir <$> getTestEnv
    liftIO $ createDirectoryIfMissing False $ cwd </> "n?ul"
    liftIO $ writeFile (cwd </> "n?ul" </> "test.a") ""
    fails $ cabal "check" []
