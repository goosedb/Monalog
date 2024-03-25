{-# LANGUAGE CPP #-}

module Copy.Native where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as Bytes.Lazy
import System.Info (os)
import System.Process.Typed (
  byteStringInput,
  runProcess,
  setStdin,
  shell,
 )

copy :: (MonadIO m) => Bytes.Lazy.ByteString -> m ()
copy bytes = void $ liftIO do
  let setInput = setStdin (byteStringInput bytes)
  let run cmd = void $ runProcess $ setInput $ shell cmd
  case os of
    "darwin" -> run "pbcopy"
    "mingw32" -> run "clip"
    "linux" -> run "xclip -sel clip"
    _ -> pure ()
