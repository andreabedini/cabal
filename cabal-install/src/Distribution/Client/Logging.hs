{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Client.Logging
  ( module Distribution.Simple.Utils
  , LogAction (..)
  , Message (..)
  , runLog
  , debug
  , info
  , notice
  , warn
  , warnError
  , die'
  , withPrettyLog
  , liftLogIO
  , captureWith
  , captureWithDebug
  )
where

import Colog.Core (LogAction (..), liftLogIO)
import Data.Functor.Contravariant (contramap)
import Distribution.Pretty (defaultStyle)
import Distribution.Simple.Utils
  ( annotateIO
  , chattyTry
  , debugNoWrap
  , dieNoVerbosity
  , dieNoWrap
  , dieWithException
  , dieWithLocation'
  , infoNoWrap
  , noticeNoWrap
  , setupMessage
  , topHandler
  , topHandlerWith
  , withOutputMarker
  )
import qualified Distribution.Simple.Utils as Cabal
import Distribution.Verbosity (Verbosity)
import System.IO.Silently (capture)
import qualified Text.PrettyPrint as Disp
import Prelude hiding (log)

data Severity
  = Notice
  | NoticeNoWrap
  | Info
  | InfoNoWrap
  | Warning
  | WarnError
  | Debug
  | DebugNoWrap
  | Die
  deriving (Eq, Show)

data Message content = Message !Severity !content
  deriving (Show, Functor)

debug :: LogAction m (Message content) -> content -> m ()
debug (LogAction log) msg = log (Message Debug msg)

info :: LogAction m (Message content) -> content -> m ()
info (LogAction log) msg = log (Message Info msg)

notice :: LogAction m (Message content) -> content -> m ()
notice (LogAction log) msg = log (Message Notice msg)

warn :: LogAction m (Message content) -> content -> m ()
warn (LogAction log) msg = log (Message Warning msg)

warnError :: LogAction m (Message content) -> content -> m ()
warnError (LogAction log) msg = log (Message WarnError msg)

die' :: LogAction m (Message content) -> content -> m ()
die' (LogAction log) msg = log (Message Die msg)

runLog :: Verbosity -> LogAction IO (Message String)
runLog verbosity = LogAction $ \case
  Message Debug msg -> Cabal.debug verbosity msg
  Message DebugNoWrap msg -> Cabal.debugNoWrap verbosity msg
  Message Info msg -> Cabal.info verbosity msg
  Message InfoNoWrap msg -> Cabal.infoNoWrap verbosity msg
  Message Notice msg -> Cabal.notice verbosity msg
  Message NoticeNoWrap msg -> Cabal.noticeNoWrap verbosity msg
  Message Warning msg -> Cabal.warn verbosity msg
  Message WarnError msg -> Cabal.warnError verbosity msg
  Message Die msg -> Cabal.die' verbosity msg

withPrettyLog :: LogAction m (Message String) -> LogAction m (Message Disp.Doc)
withPrettyLog = contramap $ fmap (Disp.renderStyle defaultStyle)

--- FIXME: captureStdoutAs
--- FIXME: captureStderrAs ?
captureWith :: (String -> IO ()) -> IO b -> IO b
captureWith log action = do
  (out, res) <- capture action
  log out
  return res

captureWithDebug :: LogAction IO (Message String) -> IO b -> IO b
captureWithDebug logger = captureWith (debug logger)
