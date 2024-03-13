module App where

import Brick qualified as B
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified as B
import Brick.Widgets.Skylighting qualified as B
import Conduit (MonadIO (..), MonadResource, runResourceT)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.State (MonadState (..), modify')
import Control.Monad.State.Strict (evalStateT)
import Data.Aeson (object)
import Data.Aeson qualified as J
import Data.Aeson.Decoding (decode)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Graphics.Vty qualified as V
import Handler (handleEvent)
import Skylighting qualified as S
import Type.AppState
import Type.Event
import Type.Log
import Type.Name
import Ui (drawUi)
import Vty (withVty)

data Input = Stdin | File FilePath

newtype AppArguments = AppArguments
  { input :: Input
  }

app :: AppArguments -> IO ()
app AppArguments{..} = withVty \vty -> do
  let logsStream = case input of
        Stdin -> readStdIn
        File path -> readFromFile path
  ch <- newBChan maxBound
  void $ forkIO $ runResourceT do
    flip evalStateT 1 $
      C.runConduit $
        logsStream
          C..| C.mapM (liftIO . writeBChan ch . NewLog)
          C..| C.sinkNull

  freshState <- initialState

  void $ B.customMain
    do vty
    do pure vty
    do Just ch
    do brickApp ch
    do freshState
  V.shutdown vty

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

readFromFile :: (MonadResource m, MonadState Int m) => FilePath -> C.ConduitT a Log m ()
readFromFile fn = C.sourceFile fn C..| readJson

readStdIn :: (MonadIO m, MonadState Int m) => C.ConduitT a Log m ()
readStdIn = C.stdin C..| readJson

readJson :: (MonadIO m, MonadState Int m) => C.ConduitT Bytes.ByteString Log m ()
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
    C..| C.mapM
      ( \json -> do
          idx <- get
          now <- liftIO getCurrentTime
          modify' (+ 1)
          pure $ Log idx now json
      )
