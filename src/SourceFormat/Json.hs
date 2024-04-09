module SourceFormat.Json (jsonLinesReader) where

import Conduit qualified as C
import Config (Prefix (..))
import Data.Aeson (Key)
import Data.Aeson qualified as J
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Conduit.Combinators qualified as C
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (defaultTimeLocale, parseTimeM)
import Effectful
import Effectful.State.Static.Local qualified as Eff
import SourceFormat.Utils
import System.IO (Handle)
import Type.Log

jsonLinesReader :: (Eff.State Int :> es, IOE :> es) => Maybe Prefix -> [Key] -> Handle -> C.ConduitT () Log (Eff es) ()
jsonLinesReader prefix defaultField handle =
  C.sourceHandle handle
    C..| C.linesUnboundedAscii
    C..| C.mapM extractPrefix
    C..| C.map
      ( \(mbPrefix, logStr) ->
          let buildDefaultLog (k : ks) = J.object [k J..= buildDefaultLog ks]
              buildDefaultLog [] = J.String $ Text.Encoding.decodeUtf8 logStr
              defaultLog = buildDefaultLog defaultField
              decodedValue = J.decode (Bytes.Lazy.fromStrict logStr)
           in (mbPrefix, fromMaybe defaultLog decodedValue)
      )
    C..| packLog
 where
  extractPrefix = case prefix of
    Just Empty -> pure . (Nothing,)
    Just KubeTm -> \bs -> do
      let (tm, str) = Bytes.break (32 ==) bs
      let parsedTm = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" $ Text.unpack $ Text.decodeUtf8 tm
      pure (Timestamp <$> parsedTm, str)
    Nothing -> pure . (Nothing,)
