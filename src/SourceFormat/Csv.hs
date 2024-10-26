module SourceFormat.Csv (csvReader) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Data.Aeson qualified as J
import Data.Aeson.Decoding (decode)
import Data.Aeson.Key qualified as Key
import Data.Attoparsec.ByteString qualified as P
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
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
import SourceFormat.Utils (packLog)
import System.IO (Handle)
import Type.Log (Log)

type Delimiter = Word8

csvReader :: (Eff.IOE :> es, Eff.State Int :> es) => Delimiter -> Handle -> IO (C.ConduitT () Log (Eff es) ())
csvReader delim handle = do
  header <- Bytes.hGetLine handle >>= parseHeader delim
  pure $
    C.sourceHandle handle
      C..| C.linesUnboundedAscii
      C..| C.mapM
        ( \logStr -> do
            row <- runParseRecord logStr
            let toLazy = Bytes.Lazy.fromStrict
            let utf8Str = J.String . Text.Encoding.decodeUtf8
            pure
              . (Nothing,)
              . J.object
              $ zipWith
                do \k v -> k J..= fromMaybe (utf8Str v) (decode $ toLazy v)
                do header
                do row
        )
      C..| packLog
 where
  runParseRecord i = either
    do const (pure [])
    do pure
    do runParseCsv delim Csv.Parser.record i

parseHeader :: Word8 -> Bytes.ByteString -> IO [Key.Key]
parseHeader delim rawHeader = map (Key.fromText . Text.Encoding.decodeUtf8) <$> runParseHeader (rawHeader <> "\n")
 where
  errorText = "Invalid CSV header"
  headerError = ErrorCall errorText
  runParseHeader i = either
    do const $ throwIO headerError
    do pure
    do runParseCsv delim Csv.Parser.header i

runParseCsv :: (Foldable t) => Word8 -> (Word8 -> P.Parser (t a)) -> Bytes.ByteString -> Either String [a]
runParseCsv delim p i = F.toList <$> P.parseOnly (p delim) i
