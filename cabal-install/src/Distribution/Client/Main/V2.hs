{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Main.V2 where

import Distribution.Simple.Command (CommandSpec (..), CommandType (..), CommandUI (..), commandAddAction)

import Distribution.Client.GlobalFlags (GlobalFlags)
import Distribution.Client.Main.V2.Bench
import Distribution.Client.Main.V2.Build
import Distribution.Client.Main.V2.Clean
import Distribution.Client.Main.V2.Configure
import Distribution.Client.Main.V2.Exec
import Distribution.Client.Main.V2.Freeze
import Distribution.Client.Main.V2.Haddock
import Distribution.Client.Main.V2.HaddockProject
import Distribution.Client.Main.V2.Install
import Distribution.Client.Main.V2.ListBin
import Distribution.Client.Main.V2.Outdated
import Distribution.Client.Main.V2.Repl
import Distribution.Client.Main.V2.Run
import Distribution.Client.Main.V2.Sdist
import Distribution.Client.Main.V2.Test
import Distribution.Client.Main.V2.Update

import qualified Data.Text as T

commandSpecs :: [CommandSpec (GlobalFlags -> IO ())]
commandSpecs =
  concat
    [ newCmd configureCommand configureAction
    , newCmd updateCommand updateAction
    , newCmd buildCommand buildAction
    , newCmd replCommand replAction
    , newCmd freezeCommand freezeAction
    , newCmd haddockCommand haddockAction
    , newCmd
        haddockProjectCommand
        haddockProjectAction
    , newCmd installCommand installAction
    , newCmd runCommand runAction
    , newCmd testCommand testAction
    , newCmd benchCommand benchAction
    , newCmd execCommand execAction
    , newCmd cleanCommand cleanAction
    , newCmd sdistCommand sdistAction
    ]
    ++ [ regularCmd listbinCommand listbinAction
       , regularCmd outdatedCommand outdatedAction
       ]

-- TODO: Duplicated
regularCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

newCmd :: CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
newCmd origUi@CommandUI{..} action = [cmd defaultUi, cmd newUi, cmd origUi]
  where
    cmd ui = CommandSpec ui (flip commandAddAction action) NormalCommand

    newMsg = T.unpack . T.replace "v2-" "new-" . T.pack
    newUi =
      origUi
        { commandName = newMsg commandName
        , commandUsage = newMsg . commandUsage
        , commandDescription = (newMsg .) <$> commandDescription
        , commandNotes = (newMsg .) <$> commandNotes
        }

    defaultMsg = T.unpack . T.replace "v2-" "" . T.pack
    defaultUi =
      origUi
        { commandName = defaultMsg commandName
        , commandUsage = defaultMsg . commandUsage
        , commandDescription = (defaultMsg .) <$> commandDescription
        , commandNotes = (defaultMsg .) <$> commandNotes
        }
