module SourceFormat.Csv (csvReader) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Data.Aeson qualified as J
import Data.Aeson.Decoding (decode)
import Data.Aeson.Key qualified as Key
import Data.Attoparsec.ByteString qualified as P
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Char (ord)
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Csv.Parser qualified as Csv.Parser
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word8)
import Effectful (Eff, (:>))
import Effectful qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import SourceFormat.Utils
import System.IO (Handle)
import Type.Log

csvReader :: (Eff.IOE :> es, Eff.State Int :> es) => Handle -> IO (C.ConduitT () Log (Eff es) ())
csvReader handle = do
  header <- Bytes.hGetLine handle >>= parseHeader delim
  pure $
    C.sourceHandle handle
      C..| C.linesUnboundedAscii
      C..| C.mapM
        ( \logStr -> do
            row <- runParseRecord logStr
            let toLazy = Bytes.Lazy.fromStrict
            let utf8 = Text.Encoding.decodeUtf8
            pure
              . J.object
              $ zipWith (\k v -> k J..= fromMaybe @J.Value (J.String $ utf8 v) (decode $ toLazy v)) header row
        )
      C..| packLog
 where
  runParseRecord i = either
    do const (pure [])
    do pure
    do runParseCsv delim Csv.Parser.record i

  delim = fromIntegral $ ord ','

parseHeader :: Word8 -> Bytes.ByteString -> IO [Key.Key]
parseHeader delim rawHeader = map (Key.fromText . Text.Encoding.decodeUtf8) <$> runParseHeader (rawHeader <> "\n")
 where
  errorText = "Invalid csv header"
  headerError = ErrorCall errorText
  runParseHeader i = either
    do const $ throwIO headerError
    do pure
    do runParseCsv delim Csv.Parser.header i

runParseCsv :: (Foldable t) => Word8 -> (Word8 -> P.Parser (t a)) -> Bytes.ByteString -> Either String [a]
runParseCsv delim p i = F.toList <$> P.parseOnly (p delim) i
