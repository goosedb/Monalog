module Widgets.StatusBar.Handler where

import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Concurrent (forkIO, threadDelay)
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
    #isEditorActive .= False
    #topLineEditor .= newEditor ""
  ResetFollow -> do
    widgetState . #followLogs .= False
  SetCopied -> do
    _ <- liftIO $ forkIO do
      threadDelay 2000000
      B.writeBChan ch E.ResetCopied
    widgetState . #copied .= True
  ResetCopied -> do
    widgetState . #copied .= False
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

newEditor :: Text.Text -> B.Editor Text.Text Name
newEditor = B.editorText (mkName StatusBarWidgetEditor) (Just 1)
