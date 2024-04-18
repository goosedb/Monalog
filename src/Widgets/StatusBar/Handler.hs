module Widgets.StatusBar.Handler where

import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isDigit)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Graphics.Vty qualified as V
import Text.Read (readMaybe)
import Type.Event qualified as E
import Type.LogViewPosition (LogViewPosition (..))
import Type.Name
import Widgets.StatusBar.Types
import Data.Bool (bool)

statusBarWidgetHandleEvent ::
  B.BChan E.Event ->
  Lens' s StatusBarWidget ->
  StatusBarWidgetCallbacks s ->
  StatusBarWidgetEvent ->
  B.EventM Name s ()
statusBarWidgetHandleEvent ch widgetState StatusBarWidgetCallbacks{..} = \case
  NewLog -> widgetState . #totalLines += 1
  ChangeTopLine i -> B.zoom widgetState do
    #topLine .= i
    #topLineEditor .= newEditor (Text.pack $ show i)
  ChangeTotalLines i -> widgetState . #totalLines .= i
  Click n -> do
    widgetState . #isEditorActive .= False
    case n of
      StatusBarWidgetLogSide ->
        widgetState . #logViewPosition %= \case
          LogViewPositionBottom -> LogViewPositionRight
          LogViewPositionRight -> LogViewPositionBottom
      StatusBarWidgetGoToTop -> do
        goToTop
        widgetState . #followLogs .= False
      StatusBarWidgetGoToBottom -> do
        goToBottom
        widgetState . #followLogs .= False
      StatusBarWidgetEditor -> widgetState . #isEditorActive .= True
      StatusBarWidgetFollow -> do
        widgetState . #followLogs %= not
        use (widgetState . #followLogs) >>= changeFollowLogs
    B.invalidateCache
  ActivateEditor -> B.zoom widgetState do
    #isEditorActive .= True
    #topLineEditor .= newEditor ""
  ResetFollow -> do
    widgetState . #followLogs .= False
  SetCopied -> do
    threadId <- scheduleStatusResetAfter2Seconds
    widgetState . #status .= JustCopied threadId
  ResetStatus tId -> do
    let setIdle = widgetState . #status .= Idle
    use (widgetState . #status) >>= 
      bool (pure ()) setIdle . (Just tId ==) . getStatusThreadId
  ConfigSaved -> do
    threadId <- scheduleStatusResetAfter2Seconds
    widgetState . #status .= JustSavedConfig threadId
  Key key mods -> do
    use (widgetState . #isEditorActive) >>= \case
      True -> case key of
        V.KEnter -> do
          use (widgetState . #topLineEditor . to B.getEditContents)
            >>= goTo . max 1 . fromMaybe 1 . readMaybe . Text.unpack . Text.concat
          widgetState . #isEditorActive .= False
        V.KChar c
          | isDigit c -> do
              B.zoom (widgetState . #topLineEditor) do
                B.handleEditorEvent (B.VtyEvent (V.EvKey (V.KChar c) []))
          | otherwise -> pure ()
        k -> B.zoom (widgetState . #topLineEditor) do
          B.handleEditorEvent (B.VtyEvent (V.EvKey k mods))
      False -> pure ()
 where
  scheduleStatusResetAfter2Seconds = do
    liftIO $ forkIO do
      threadDelay 2000000
      myThreadId >>= B.writeBChan ch . E.ResetStatus

  getStatusThreadId = \case
    JustSavedConfig tid -> Just tid
    JustCopied tid -> Just tid
    Idle -> Nothing

newEditor :: Text.Text -> B.Editor Text.Text Name
newEditor = B.editorText (mkName StatusBarWidgetEditor) (Just 1)
