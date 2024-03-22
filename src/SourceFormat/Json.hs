module SourceFormat.Json (jsonLinesReader) where

import Conduit qualified as C
import Data.Aeson (Key)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Conduit.Combinators qualified as C
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Effectful
import Effectful.State.Static.Local qualified as Eff
import SourceFormat.Utils
import System.IO (Handle)
import Type.Log

jsonLinesReader :: (Eff.State Int :> es, IOE :> es) => [Key] -> Handle -> C.ConduitT () Log (Eff es) ()
jsonLinesReader defaultField handle =
  C.sourceHandle handle
    C..| C.linesUnboundedAscii
    C..| C.map
      ( \logStr ->
          let buildDefaultLog (k : ks) = J.object [k J..= buildDefaultLog ks]
              buildDefaultLog [] = J.String $ Text.Encoding.decodeUtf8 logStr
              defaultLog = buildDefaultLog defaultField
              decodedValue = J.decode (Bytes.Lazy.fromStrict logStr)
           in fromMaybe defaultLog decodedValue
      )
    C..| packLog
