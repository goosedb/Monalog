{-# LANGUAGE DataKinds #-}

module App where

import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Brick.Widgets.Skylighting qualified as B
import Conduit (MonadIO (..))
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Aeson (object)
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
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Effectful (Eff, (:>))
import Effectful qualified as Eff
import Effectful.Resource qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import Graphics.Vty qualified as V
import Handler (handleEvent)
import Skylighting qualified as S
import Type.AppState
import Type.Event
import Type.Log
import Type.Name
import Ui (drawUi)
import Vty (withVty)
import qualified Data.Text as Text
import Control.Exception (throwIO, ErrorCall (ErrorCall), finally)
import qualified Data.Text.IO as Text.IO
import GHC.IO (catchException)

data Input = Stdin | File FilePath
data Format = Json | Csv

data AppArguments = AppArguments
  { input :: Input
  , format :: Format
  }

app :: AppArguments -> IO ()
app AppArguments{..} = withVty \vty -> do
  ch <- newBChan maxBound
  let reader = case format of
        Json -> readJson
        Csv -> readCsv ch
  let logsStream = case input of
        Stdin -> readStdIn reader
        File path -> readFromFile path reader

  void
    $ forkIO
      . Eff.runEff
      . Eff.runResource
      . Eff.evalState @(Maybe Header) Nothing
      . Eff.evalState (1 :: Int)
      . C.runConduit
    $ logsStream
      C..| C.mapM (liftIO . writeBChan ch . NewLog)
      C..| C.sinkNull

  freshState <- initialState

  let run = finally 
        do void $ B.customMain
            do vty
            do pure vty
            do Just ch
            do brickApp ch
            do freshState
        do V.shutdown vty

  run `catchException` \case MkFatalError e -> Text.IO.putStrLn e


brickApp :: B.BChan Event -> B.App AppState Event Name
brickApp ch =
  B.App
    { appDraw = drawUi
    , appChooseCursor = const listToMaybe
    , appHandleEvent = handleEvent ch
    , appStartEvent = do
        vty <- B.getVtyHandle
        let output = V.outputIface vty
        liftIO $ V.setMode output V.Mouse True
    , appAttrMap = const $ B.attrMap V.defAttr (B.attrMappingsForStyle S.pygments)
    }

type Reader m = C.ConduitT Bytes.ByteString Log m ()

readFromFile :: (Eff.Resource :> es, Eff.IOE :> es, Eff.State Int :> es) => FilePath -> Reader (Eff es) -> C.ConduitT a Log (Eff es) ()
readFromFile fn reader = C.sourceFile fn C..| reader

readStdIn :: (Eff.IOE :> es, Eff.State Int :> es) => Reader (Eff es) -> C.ConduitT a Log (Eff es) ()
readStdIn reader = C.stdin C..| reader

type Header = [Key.Key]

readCsv :: B.BChan Event -> (Eff.IOE :> es, Eff.State Int :> es, Eff.State (Maybe Header) :> es) => Reader (Eff es)
readCsv ch = do
  let delim = fromIntegral $ ord ','
  let errorText = "Invalid csv header"
  let headerError = ErrorCall errorText
  let mkFatalError  = MkFatalError . Text.pack . (errorText <>) . (": " <>)
  let runParse p i = F.toList <$> P.parseOnly (p delim) i
  let runParseHeader i = either 
        do liftIO . (>> throwIO headerError) . B.writeBChan ch . FatalError . mkFatalError
        do pure 
        do runParse Csv.Parser.header i
  let runParseRecord i = either 
        do const (pure [])
        do pure 
        do runParse Csv.Parser.record i
  C.linesUnboundedAscii
    C..| C.mapM
      ( \bs ->
          Eff.get @(Maybe Header) >>= \case
            Just h -> pure $ Just (h, bs)
            Nothing -> do
              header <- map (Key.fromText . Text.Encoding.decodeUtf8) <$> runParseHeader (bs <> "\n")
              Eff.put (Just header)
              pure Nothing
      )
    C..| C.filter isJust
    C..| C.map fromJust
    C..| C.mapM
      ( \(header, logStr) -> do
          row <- runParseRecord logStr
          let toLazy = Bytes.Lazy.fromStrict
          let utf8 = Text.Encoding.decodeUtf8
          pure
            . J.object
            $ zipWith (\k v -> k J..= fromMaybe @J.Value (J.String $ utf8 v) (decode $ toLazy v)) header row
      )
    C..| packLog

readJson :: (Eff.IOE :> es, Eff.State Int :> es) => Reader (Eff es)
readJson =
  C.linesUnboundedAscii
    C..| C.map
      ( \logStr ->
          let buildDefaultLog (k : ks) = object [k J..= buildDefaultLog ks]
              buildDefaultLog [] = J.String $ Text.Encoding.decodeUtf8 logStr
              defaultLog = buildDefaultLog $ Nel.toList (Nel.singleton "message")
              decodedValue = decode (Bytes.Lazy.fromStrict logStr)
           in fromMaybe defaultLog decodedValue
      )
    C..| packLog

packLog :: (Eff.IOE :> es, Eff.State Int :> es) => C.ConduitT J.Value Log (Eff es) ()
packLog =
  C.mapM
    ( \json -> do
        idx <- Eff.get
        now <- liftIO getCurrentTime
        Eff.modify @Int (+ 1)
        pure $ Log idx now json
    )
