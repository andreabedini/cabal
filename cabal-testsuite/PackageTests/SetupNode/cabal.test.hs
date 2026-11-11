import Test.Cabal.Prelude

-- The setup of a package is a node of the build plan. It is built once,
-- before the package, and when it is built again the package is configured
-- again with the new one. That goes for a change to Setup.hs and for a change
-- to a module it imports.
main = cabalTest . recordMode DoNotRecord $ do
  r1 <- cabal' "v2-build" ["all"]
  assertOutputContains "setup-node-1.0 (setup) (first run)" r1
  assertOutputContains "configured by helper one in Setup one" r1

  env <- getTestEnv
  assertFileDoesContain
    (testDistDir env </> "cache" </> "plan.json")
    "\"id\":\"setup-node-1.0-setup-inplace\""

  r2 <- cabal' "v2-build" ["all"]
  assertOutputContains "Up to date" r2

  edit "SetupHelper.hs" "helper one" "helper two"
  r3 <- cabal' "v2-build" ["all"]
  assertOutputContains "setup-node-1.0 (setup) (file SetupHelper.hs changed)" r3
  assertOutputContains "(setup rebuilt)" r3
  assertOutputContains "configured by helper two in Setup one" r3

  edit "Setup.hs" "Setup one" "Setup two"
  r4 <- cabal' "v2-build" ["all"]
  assertOutputContains "setup-node-1.0 (setup) (file Setup.hs changed)" r4
  assertOutputContains "(setup rebuilt)" r4
  assertOutputContains "configured by helper two in Setup two" r4
  where
    edit file old new = do
      env <- getTestEnv
      contents <- liftIO $ readFile (testCurrentDir env </> file)
      delay
      length contents `seq` writeSourceFile file (replace old new contents)

    replace old new s@(c : cs)
      | take (length old) s == old = new ++ replace old new (drop (length old) s)
      | otherwise = c : replace old new cs
    replace _ _ [] = []
