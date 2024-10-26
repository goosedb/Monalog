{-# LANGUAGE CPP #-}

module Vty where

import Graphics.Vty qualified as Vty
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Graphics.Vty.Platform.Windows qualified as Vty
import Graphics.Vty.Platform.Windows.Settings qualified as Vty
#else
import Graphics.Vty.Platform.Unix qualified as Vty
import Graphics.Vty.Platform.Unix.Settings qualified as Vty
import System.Posix.IO (handleToFd)
#endif
import System.IO

-- | On posix platforms, suspend the program using the system STOP signal
-- (as control-z usually does in bash). On windows, does nothing.
#ifdef mingw32_HOST_OS
suspendSignal :: IO ()
suspendSignal = return ()
#else
import System.Posix.Signals
import Config (Input (..))
suspendSignal :: IO ()
suspendSignal = raiseSignal keyboardStop
#endif


{- FOURMOLU_DISABLE -}
makeVty :: Input -> IO Vty.Vty
makeVty (File _) = Vty.defaultSettings >>= Vty.mkVtyWithSettings Vty.defaultConfig 
makeVty Stdin = do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  -- https://github.com/microsoft/terminal/issues/16868
  withFile "\\\\.\\CONIN$" ReadWriteMode \tty -> do
#else
  withFile "/dev/tty" ReadWriteMode \ttyHandle -> do
    tty <- handleToFd ttyHandle
#endif
    defSettings <- Vty.defaultSettings
    Vty.mkVtyWithSettings Vty.defaultConfig defSettings{Vty.settingInputFd = tty}
