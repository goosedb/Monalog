{-# LANGUAGE CPP #-}

module Vty where

import Graphics.Vty qualified as Vty
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Graphics.Vty.Platform.Windows qualified as Vty
import Graphics.Vty.Platform.Windows.Settings qualified as Vty
#else
import Graphics.Vty.Platform.Unix qualified as Vty
import Graphics.Vty.Platform.Unix.Settings qualified as Vty
import System.Posix.IO
#endif
import Config (Input (..))
import Control.Exception (throwIO)
import System.Environment (lookupEnv)
import System.IO

{- FOURMOLU_DISABLE -}
withVty :: Input -> (Vty.Vty -> IO a) -> IO a
withVty (Files _) action = Vty.defaultSettings >>= Vty.mkVtyWithSettings Vty.defaultConfig >>= action
withVty Stdin action = do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  -- https://github.com/microsoft/terminal/issues/16868
  withFile "\\\\.\\CONIN$" ReadWriteMode \tty -> do
    defSettings <- Vty.defaultSettings
#else
  withFile "/dev/tty" ReadWriteMode \ttyHandle -> do
    tty <- handleToFd ttyHandle
    defSettings <- lookupEnv "TERM" >>= maybe (throwIO Vty.MissingTermEnvVar) \term -> do
      pure Vty.UnixSettings 
        { settingVmin = 1  
        , settingVtime = 100
        , settingInputFd = tty
        , settingOutputFd = stdOutput
        , settingTermName = term
        }
#endif
    vty <- Vty.mkVtyWithSettings Vty.defaultConfig defSettings
    action vty
