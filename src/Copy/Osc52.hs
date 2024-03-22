module Copy.Osc52 where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Base64 qualified as Bytes.Base64

copy :: (MonadIO m) => Bytes.ByteString -> m ()
copy bytes = liftIO do
  Bytes.putStr "\x1B]52;c;\x07"
  Bytes.putStr ("\x1B]52;c;" <> Bytes.Base64.encode bytes <> "\x07")
