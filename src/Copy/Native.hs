{-# LANGUAGE CPP #-}

module Copy.Native where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import System (System (Linux, Macos, Windows), currentOs)
import System.Process.Typed

copy :: (MonadIO m) => Maybe String -> Bytes.Lazy.ByteString -> m (Either Text ())
copy userCmd bytes = liftIO do
  let setInput = setStdin (byteStringInput bytes)
      setErr = setStderr byteStringOutput
      setOutput = setStdout byteStringOutput
  let run cmd = do
        process <- startProcess $ setErr $ setOutput $ setInput $ shell cmd
        waitExitCode process
          >>= atomically . \case
            ExitSuccess -> pure (Right ())
            ExitFailure _ -> Left . Text.decodeUtf8 . Bytes.Lazy.toStrict <$> getStderr process
  let cmd =
        userCmd <|> case currentOs of
          Macos -> Just "pbcopy"
          Windows -> Just "clip"
          Linux -> Just "xclip -sel clip"
          _ -> Nothing
  maybe (pure $ Left "Native copy not supported") run cmd
