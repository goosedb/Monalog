module SourceFormat.Json (jsonReader) where

import Data.Aeson qualified as J
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Effectful
import Effectful qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import SourceFormat.Utils
import System.IO (Handle)
import Type.Log

jsonReader :: (Eff.State Int :> es, IOE :> es) => Handle -> C.ConduitT () Log (Eff es) ()
jsonReader handle = C.sourceHandle handle C..| readJson

readJson :: (Eff.IOE :> es, Eff.State Int :> es) => C.ConduitT Bytes.ByteString Log (Eff es) ()
readJson =
  C.linesUnboundedAscii
    C..| C.map
      ( \logStr ->
          let buildDefaultLog (k : ks) = J.object [k J..= buildDefaultLog ks]
              buildDefaultLog [] = J.String $ Text.Encoding.decodeUtf8 logStr
              defaultLog = buildDefaultLog ["message"]
              decodedValue = J.decode (Bytes.Lazy.fromStrict logStr)
           in fromMaybe defaultLog decodedValue
      )
    C..| packLog
