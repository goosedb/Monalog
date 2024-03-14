{-# LANGUAGE DataKinds #-}

module App where

import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Brick.Widgets.Skylighting qualified as B
import Conduit (MonadIO (..))
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (void)
import Data.ByteString qualified as Bytes
import Data.Char (ord)
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Generics.Labels ()
import Data.Maybe (listToMaybe)
import Data.Text.IO qualified as Text.IO
import Effectful qualified as Eff
import Effectful.Resource qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import GHC.IO (catchException)
import Graphics.Vty qualified as V
import Handler (handleEvent)
import Skylighting qualified as S
import SourceFormat.Csv qualified as Csv
import SourceFormat.Json qualified as Json
import System.IO (IOMode (ReadMode), stdin, withFile)
import Type.AppState
import Type.Event
import Type.Name
import Ui (drawUi)
import Vty (withVty)

data Input = Stdin | File FilePath
data Format = Json | Csv

data AppArguments = AppArguments
  { input :: Input
  , format :: Format
  }

app :: AppArguments -> IO ()
app AppArguments{..} = withVty \vty -> do
  ch <- newBChan maxBound
  let withStream handle action = case format of
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
      void $
        forkIO . runConduit $
          logsStream
            C..| C.mapM (liftIO . writeBChan ch . NewLog)
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
