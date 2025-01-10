{-# LANGUAGE DataKinds #-}

module App where

import AttrMap qualified
import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Buffer (Buffer (..), makeBuffer)
import Column (valueColumns)
import Config
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.DeepSeq (force)
import Control.Exception (evaluate, finally, throwIO)
import Control.Monad (foldM, forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson.Parser qualified as P
import Data.Attoparsec.ByteString qualified as P
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Last (getLast))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Word (Word8)
import Effectful qualified as Eff
import Effectful.Resource qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import GHC.IO (catchException)
import GHC.IO.Handle (hFlushAll, hGetLine)
import Graphics.Vty qualified as V
import Handler (handleEvent)
import SourceFormat.Csv qualified as Csv
import SourceFormat.Json qualified as Json
import Streaming qualified as S
import Streaming.ByteString qualified as BS
import Streaming.ByteString.Internal as SI
import Streaming.Prelude qualified as S
import System.FilePath qualified as Path
import System.IO (IOMode (ReadMode), stdin, withFile)
import Type.AppState
import Type.Event
import Type.Field
import Type.Log qualified
import Type.LogViewPosition (LogViewPosition (LogViewPositionRight))
import Type.Name
import Ui (drawUi)
import Vty (withVty)
import Widgets.Fields.Types (FieldsViewLayout (Flatten))

data AppArguments = AppArguments
  { input :: Input
  , format :: Maybe Format
  , csvDelimiter :: Word8
  , defaultField :: Maybe Text
  , configPath :: Maybe FilePath
  , ignoreConfig :: Maybe ConfigType
  , prefix :: Maybe Prefix
  }
  deriving (Show)

app :: AppArguments -> IO ()
app AppArguments{..} = do
  let run = do
        config <- loadConfig configPath ignoreConfig
        defaultFieldParsed <- parseDefaultField (defaultField <|> fromConfig config (.defaultField))

        ch <- newBChan maxBound
        let defaultFields = coerce <$> fromConfig config (.columns)
        let formatType = reifyFormat (format <|> fromConfig config (.format)) input
            dataStream stream = case formatType of
              Jsonl -> pure $ Json.jsonLinesReader (prefix <|> fromConfig config (.prefix)) defaultFieldParsed stream
              Csv -> Csv.csvReader (maybe (B.writeBChan ch . SelectFields) (\_ _ -> pure ()) defaultFields) csvDelimiter stream
        let withLogsHandle action = case input of
              Stdin -> dataStream BS.stdin >>= action
              Files paths -> do
                let go acc [] = foldM (\s path -> (s <>) <$> dataStream path) mempty (reverse acc) >>= action
                    go acc (p : ps) = withFile p ReadMode \h -> go (BS.hGetContents h : acc) ps
                go [] $ Nel.toList paths

        withLogsHandle \stream -> do
          let runStream =
                Eff.runEff
                  . Eff.runResource
                  . Eff.evalState (1 :: Int)
                  . Eff.evalState (P.parse P.json)
                  . S.mapsM_ (pure . snd . S.lazily)

          Buffer{..} <- makeBuffer (writeBChan ch . NewLogs)

          stream
            & S.map (\l -> NewLog l (valueColumns l.value))
            & S.mapM (liftIO . evaluate . force)
            & S.mapM (liftIO . pushBuffer)
            & runStream
            & forkIO
            & void

          putStrLn "2"

          freshState <-
            initialState
              (fromMaybe False $ fromConfig config (.textWrap))
              (fromMaybe LogViewPositionRight $ fromConfig config (.logViewSide))
              (fromMaybe Flatten $ fromConfig config (.fieldsLayout))
              input
              (fromConfig config (.copyCommand))
              (fromConfig config (.copyMethod))
              defaultFields

          withVty input \vty -> finally
            do
              void $ B.customMain
                do vty
                do pure vty
                do Just ch
                do brickApp ch
                do freshState
            do
              V.shutdown vty >> killBuffer

  run `catchException` \case MkFatalError e -> Text.IO.putStrLn e
 where
  fromConfig :: AppConfig -> (AppConfig -> Last a) -> Maybe a
  fromConfig cfg v = getLast . v $ cfg

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
    , appAttrMap = const AttrMap.attrs
    }

reifyFormat :: Maybe Format -> Input -> Format
reifyFormat format input = case format of
  Just fmt -> fmt
  Nothing | Files (path :| _) <- input ->
    case Path.takeExtension path of
      ".csv" -> Csv
      _ -> Jsonl
  _ -> Jsonl

parseDefaultField :: Maybe Text -> IO Path
parseDefaultField defaultField = maybe
  do pure ["message"]
  do
    \k -> maybe
      do throwIO $ MkFatalError "Failed to parse json key supposed to be default field"
      do pure
      do parsePath k
  do defaultField

parseField :: Text -> Maybe Field
parseField "@timestamp" = Just Timestamp
parseField k = Field <$> parsePath k

parsePath :: Text -> Maybe [Text]
parsePath = fmap Nel.toList . Nel.nonEmpty . filter (not . Text.null) . Text.splitOn "."
