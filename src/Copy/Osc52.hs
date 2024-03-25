module Copy.Osc52 where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Base64.Lazy qualified as Bytes.Base64
import Data.ByteString.Lazy qualified as Bytes.Lazy

copy :: (MonadIO m) => Bytes.Lazy.ByteString -> m ()
copy bytes = liftIO do
  Bytes.Lazy.putStr "\x1B]52;c;\x07"
  Bytes.Lazy.putStr ("\x1B]52;c;" <> Bytes.Base64.encode bytes <> "\x07")
