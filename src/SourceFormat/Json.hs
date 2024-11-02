module SourceFormat.Json (jsonLinesReader) where

import Config (Prefix (..))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Data.Time (defaultTimeLocale, parseTimeM)
import Effectful
import Effectful.State.Static.Local qualified as Eff
import SourceFormat.Utils
import Streaming (Of, Stream)
import Streaming qualified as S
import Streaming.ByteString qualified as BS
import Streaming.ByteString.Char8 qualified as BS8
import Streaming.Prelude qualified as S
import Type.Field (Path)
import Type.Log

jsonLinesReader :: forall es. (Eff.State Int :> es, IOE :> es) => Maybe Prefix -> Path -> BS.ByteStream IO () -> Stream (Of Log) (Eff es) ()
jsonLinesReader prefix defaultField stream =
  stream
    & BS8.lines
    & S.mapped BS.toLazy
    & S.hoist liftIO
    & S.mapM extractPrefix
    & S.map
      ( \(mbPrefix, logStr) ->
          let buildDefaultLog (k : ks) = J.object [Key.fromText k J..= buildDefaultLog ks]
              buildDefaultLog [] = J.String $ Text.Lazy.toStrict $ Text.Lazy.Encoding.decodeUtf8 logStr
              defaultLog = buildDefaultLog defaultField
              decodedValue = J.decode logStr
           in (mbPrefix, fromMaybe defaultLog decodedValue)
      )
    & S.mapM packLog
 where
  extractPrefix = case prefix of
    Just KubeTm -> \bs -> do
      let (tm, str) = Bytes.Lazy.break (32 ==) bs
      let parsedTm = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" $ Text.Lazy.unpack $ Text.Lazy.Encoding.decodeUtf8 tm
      pure (Timestamp <$> parsedTm, str)
    Nothing -> pure . (Nothing,)
