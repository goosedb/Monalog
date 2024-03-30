{-# LANGUAGE DataKinds #-}

module App where

import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Brick.Widgets.Skylighting qualified as B
import Buffer (Buffer (..), makeBuffer)
import Conduit (MonadIO (..))
import Config
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (finally, throwIO)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Parser qualified as P
import Data.Attoparsec.ByteString qualified as P
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (listToMaybe)
import Data.Monoid (Last (getLast))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Traversable (forM)
import Effectful qualified as Eff
import Effectful.Resource qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import GHC.IO (catchException)
import Graphics.Vty qualified as V
import Handler (handleEvent)
import Skylighting qualified as S
import SourceFormat.Csv qualified as Csv
import SourceFormat.Json qualified as Json
import System.FilePath qualified as Path
import System.IO (IOMode (ReadMode), stdin, withFile)
import Type.AppState
import Type.Event
import Type.Field
import Type.Name
import Ui (drawUi)
import Vty (withVty)

data AppArguments = AppArguments
  { input :: Input
  , format :: Maybe Format
  , mbDefaultField :: Maybe Text
  , configPath :: Maybe FilePath
  , ignoreConfig :: Maybe ConfigType
  }

app :: AppArguments -> IO ()
app AppArguments{..} = do
  let run = do
        config <- loadConfig configPath ignoreConfig
        defaultField <- parseDefaultField (mbDefaultField <|> fromConfig config (.defaultField))
        withVty \vty -> do
          ch <- newBChan maxBound
          let withStream handle action = case reifyFormat (format <|> fromConfig config (.format)) input of
                Json -> action (Json.jsonLinesReader defaultField handle)
                Csv -> Csv.csvReader handle >>= action
          let withLogsHandle action = case input of
                Stdin -> action stdin
                File path -> withFile path ReadMode action

          withLogsHandle \handle -> do
            withStream handle \logsStream -> do
              let runConduit =
                    Eff.runEff
                      . Eff.runResource
                      . Eff.evalState (1 :: Int)
                      . Eff.evalState (P.parse P.json)
                      . C.runConduit

              Buffer{..} <- makeBuffer (writeBChan ch . NewLogs)

              void
                . forkIO
                . runConduit
                $ logsStream
                  C..| C.mapM (liftIO . pushBuffer)
                  C..| C.sinkNull

              defaultFields <- forM (fromConfig config (.fields)) do
                traverse (maybe (throwIO $ MkFatalError "Failed to parse default fields from config") pure . parseField)

              freshState <-
                initialState
                  (fromConfig config (.copyCommand))
                  (fromConfig config (.copyMethod))
                  defaultFields

              finally
                do
                  void $ B.customMain
                    do vty
                    do pure vty
                    do Just ch
                    do brickApp ch
                    do freshState
                do
                  V.shutdown vty
                  killBuffer

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
    , appAttrMap = const $ B.attrMap V.defAttr (B.attrMappingsForStyle S.pygments)
    }

reifyFormat :: Maybe Format -> Input -> Format
reifyFormat format input = case format of
  Just fmt -> fmt
  Nothing | File path <- input ->
    case Path.takeExtension path of
      ".csv" -> Csv
      _ -> Json
  _ -> Json

parseDefaultField :: Maybe Text -> IO [Key.Key]
parseDefaultField mbDefaultField = maybe
  do pure ["message"]
  do
    \k -> maybe
      do throwIO $ MkFatalError "Failed to parse json key supposed to be default field"
      do pure
      do parsePath k
  do mbDefaultField

parseField :: Text -> Maybe Field
parseField "@timestamp" = Just Timestamp
parseField "@raw" = Just Raw
parseField k = Field <$> parsePath k

parsePath :: Text -> Maybe [Key]
parsePath = fmap Nel.toList . Nel.nonEmpty . map Key.fromText . filter (not . Text.null) . Text.splitOn "."
