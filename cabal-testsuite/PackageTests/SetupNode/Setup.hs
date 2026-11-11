import Distribution.Simple

import SetupHelper (setupMessage)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postConf = \args flags pd lbi -> do
          putStrLn ("configured by " ++ setupMessage ++ " in Setup one")
          postConf simpleUserHooks args flags pd lbi
      }
