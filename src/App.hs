{-# LANGUAGE DataKinds #-}

module App where

import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Brick.Widgets.Skylighting qualified as B
import Conduit (MonadIO (..))
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (void, forever)
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Generics.Labels ()
import Data.Maybe (listToMaybe)
import Data.Text.IO qualified as Text.IO
import Effectful qualified as Eff
import Effectful.Resource qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import GHC.IO (catchException)
import GHC.Conc.IO (threadDelay)
import Graphics.Vty qualified as V
import Handler (handleEvent)
import Skylighting qualified as S
import SourceFormat.Csv qualified as Csv
import SourceFormat.Json qualified as Json
import System.FilePath qualified as Path
import System.IO (IOMode (ReadMode), stdin, withFile)
import Type.AppState
import Type.Event
import Type.Name
import Ui (drawUi)
import Data.IORef
import Vty (withVty)

data Input = Stdin | File FilePath
data Format = Json | Csv

data AppArguments = AppArguments
  { input :: Input
  , format :: Maybe Format
  }

app :: AppArguments -> IO ()
app AppArguments{..} = withVty \vty -> do
  ch <- newBChan maxBound
  let withStream handle action = case reifyFormat format input of
        Json -> action (Json.jsonReader handle)
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
              . C.runConduit

      buffer <- newIORef []
      
      void $ forkIO $ forever do 
        ls <- atomicModifyIORef buffer (\ls -> ([], ls))
        liftIO . writeBChan ch . NewLog $ (reverse ls) 
        threadDelay 250000

      void
        . forkIO
        . runConduit
        $ logsStream
          C..| C.mapM (\l -> liftIO $ atomicModifyIORef buffer (\ls -> (l:ls, ())))
          C..| C.sinkNull

      freshState <- initialState

      let run = finally
            do
              void $ B.customMain
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

reifyFormat :: Maybe Format -> Input -> Format
reifyFormat format input = case format of
  Just fmt -> fmt
  Nothing | File path <- input ->
    case Path.takeExtension path of
      ".csv" -> Csv
      _ -> Json
  _ -> Json
