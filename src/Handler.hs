module Handler (handleEvent) where

import Brick qualified as B
import Brick.BChan qualified as B
import Conduit (MonadIO (..))
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad (forM_, when)
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Graphics.Vty qualified as V
import Type.AppState
import Type.AppState qualified as AS
import Type.Event
import Type.Field (Field (..))
import Type.Name
import Type.Name qualified as N
import Widgets.Error.Handler qualified as Error
import Widgets.Error.Types qualified as Error
import Widgets.Fields.Handler qualified as Fields
import Widgets.Fields.Types qualified as Fields
import Widgets.LogView.Handler qualified as LogView
import Widgets.LogView.Types qualified as LogView
import Widgets.LogsView.Handler qualified as LogsView
import Widgets.LogsView.Types qualified as LogsView
import Widgets.Query.Handler qualified as Query
import Widgets.Query.Types qualified as Query
import Widgets.StatusBar.Handler qualified as StatusBar
import Widgets.StatusBar.Types qualified as StatusBar
import Widgets.Types (PackedLens' (PackedLens'))

handleEvent :: B.BChan Event -> B.BrickEvent Name Event -> B.EventM Name AppState ()
handleEvent ch e = do
  ms <- use #mouseState

  activeWidget <- use #activeWidget
  logView <- use #logView
  case e of
    B.AppEvent ae -> case ae of
      FatalError txt -> do
        B.halt
        liftIO $ throwIO txt
      NewLogs ls -> forM_ ls \l -> do
        callFieldsWidget (Fields.NewLog l)
        callLogsViewWidget (LogsView.NewLog l)
      FilteredLogs ls -> forM_ ls \l -> do
        callLogsViewWidget (LogsView.FilteredLog l)
      _ -> pure ()
    B.MouseDown (WidgetName name) V.BLeft _ loc | ms == Up -> do
      let canClick = case activeWidget of
            aw : _ -> not (isActiveWidgetError aw) || isErrorWidget name
            _ -> False
      when canClick do
        case name of
          N.QueryWidgetName qn -> do
            #activeWidget .= [AS.QueryWidgetName]
            callQueryWidget (Query.Click qn loc)
          N.LogsViewWidgetName ln -> do
            #activeWidget .= [AS.LogsWidgetName]
            callLogsViewWidget (LogsView.Click ln)
          N.LogViewWidgetName _ ->
            #activeWidget .= [AS.LogWidgetName]
          N.FieldsWidgetName fn -> do
            #activeWidget .= [AS.FieldWidgetName]
            callFieldsWidget (Fields.Click fn)
          N.ErrorWidgetName en -> do
            callErrorWidget (Error.Click en)
          N.StatusBarWidgetName sbn -> do
            #activeWidget .= [AS.ScrollBarWidgetName]
            callStatusBarWidget (StatusBar.Click sbn)
      #mouseState .= Down
    B.MouseDown (WidgetName name) V.BScrollDown mods _ -> handleScroll name mods 1
    B.MouseDown (WidgetName name) V.BScrollUp mods _ -> handleScroll name mods (-1)
    B.MouseUp{} -> #mouseState .= Up
    B.VtyEvent V.EvMouseUp{} -> #mouseState .= Up
    B.VtyEvent V.EvResize{} -> B.invalidateCache
    B.VtyEvent (V.EvKey V.KEsc _) | Just _ <- logView -> do
      B.invalidateCache
      #logView .= Nothing
    B.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> B.halt
    _ -> case activeWidget of
      AS.QueryWidgetName : _ -> do
        let mbEv = case e of
              B.VtyEvent (V.EvKey k mods) -> Just $ Query.Key k mods
              _ -> Nothing
        maybe (pure ()) callQueryWidget mbEv
      AS.LogsWidgetName : _ -> do
        let mbEv = case e of
              B.VtyEvent (V.EvMouseDown _ _ V.BScrollDown mods) -> Just $ logsScrollEvent 1 mods
              B.VtyEvent (V.EvMouseDown _ _ V.BScrollUp mods) -> Just $ logsScrollEvent (-1) mods
              B.VtyEvent (V.EvKey k mods) -> Just $ LogsView.Key k mods
              _ -> Nothing
        maybe (pure ()) callLogsViewWidget mbEv
      AS.LogWidgetName : _ -> do
        let mbEv = case e of
              B.VtyEvent (V.EvMouseDown _ _ V.BScrollDown _) -> Just logScrollDownEvent
              B.VtyEvent (V.EvMouseDown _ _ V.BScrollUp _) -> Just logScrollUpEvent
              _ -> Nothing
        maybe (pure ()) callLogViewWidget mbEv
      AS.ScrollBarWidgetName : _ -> do
        let mbEv = case e of
              B.VtyEvent (V.EvKey k mods) -> Just $ StatusBar.Key k mods
              _ -> Nothing
        maybe (pure ()) callStatusBarWidget mbEv
      AS.FieldWidgetName : _ -> pure ()
      AS.ErrorWidgetName _ : _ -> pure ()
      _ -> error "impossible!"
 where
  handleScroll name mods i = case name of
    N.QueryWidgetName _ -> pure ()
    N.LogsViewWidgetName _ -> callLogsViewWidget (logsScrollEvent i mods)
    N.LogViewWidgetName _ -> callLogViewWidget (LogView.Scroll i)
    N.FieldsWidgetName _ -> callFieldsWidget (Fields.Scroll i)
    N.ErrorWidgetName _ -> pure ()
    N.StatusBarWidgetName _ -> pure ()

  logsScrollEvent i mods
    | V.MCtrl `elem` mods = LogsView.AltScroll i
    | otherwise = LogsView.Scroll i

  logScrollUpEvent = LogView.Scroll (-1)
  logScrollDownEvent = LogView.Scroll 1

  callQueryWidget = Query.queryWidgetHandleEvent
    do #queryView
    do queryWidgetCallbacks

  callLogViewWidget = LogView.logViewWidgetHandleEvent (#logView . _Just)

  callStatusBarWidget =
    StatusBar.statusBarWidgetHandleEvent
      #statusBar
      StatusBar.StatusBarWidgetCallbacks
        { goToTop = do
            activateLogsView
            callLogsViewWidget LogsView.GoToTop
        , goToBottom = do
            activateLogsView
            callLogsViewWidget LogsView.GoToBottom
        , goTo = \l -> do
            activateLogsView
            callLogsViewWidget $ LogsView.GoTo l
        , changeFollowLogs = \f -> do
            activateLogsView
            callLogsViewWidget $ LogsView.FollowLogs f
        }

  callFieldsWidget = Fields.fieldsWidgetHandleEvent
    do #fieldsView
    do fieldsWidgetCallbacks

  callLogsViewWidget =
    let ?callbacks =
          LogsView.LogsViewWidgetCallbacks
            { topLineChanged = callStatusBarWidget . StatusBar.ChangeTopLine
            , totalLinesChanged = callStatusBarWidget . StatusBar.ChangeTotalLines
            , selectedLogChanged = \_ -> pure ()
            , logAddedToView = callStatusBarWidget StatusBar.NewLog
            , selectedLog = \l -> do
                B.invalidateCache
                #logView .= (LogView.LogViewWidget <$> l)
                maybe (pure ()) (callLogViewWidget . LogView.LogSelected) l
            , resetFollow = callStatusBarWidget StatusBar.ResetFollow
            }
        ?widgetState = PackedLens' #logsView
     in LogsView.logsViewWidgetHandleEvent

  callErrorWidget = Error.errorWidgetHandleEvent
    do errorWidgetCallbacks

  isActiveWidgetError = \case
    AS.ErrorWidgetName _ -> True
    _ -> False

  isErrorWidget = \case
    N.ErrorWidgetName{} -> True
    _ -> False

  queryWidgetCallbacks =
    Query.QueryWidgetCallbacks
      { Query.execFilter = \q -> do
          activateLogsView
          callLogsViewWidget $ LogsView.RunFilter ch q
      , Query.clearFilter = callLogsViewWidget LogsView.ClearFilter
      , Query.showError = \txt -> do
          #activeWidget %= (AS.ErrorWidgetName (Error.ErrorWidget txt) :)
      }

  fieldsWidgetCallbacks =
    Fields.FieldsWidgetCallbacks
      { Fields.fieldSelected = \width field -> do
          B.invalidateCache
          #logsView . #selectedFields %= (<> [LogsView.SelectedField{..}])
      , Fields.fieldUnselected = \field' -> do
          B.invalidateCache
          #logsView . #selectedFields %= filter
            \LogsView.SelectedField{..} -> field /= field'
      , Fields.fieldsChangedMaxSize = \paths -> do
          B.invalidateCache
          #logsView . #selectedFields %= map \f@LogsView.SelectedField{..} -> case field of
            Field path -> maybe
              do f
              do \mw -> LogsView.SelectedField{width = mw, ..}
              do Map.lookup path paths
            _ -> f
      }

  errorWidgetCallbacks =
    Error.ErrorWidgetCallbacks
      { Error.closeErrorWidget = do
          #activeWidget %= filter \case
            AS.ErrorWidgetName _ -> False
            _ -> True
      }

  activateLogsView = #activeWidget .= [AS.LogsWidgetName]
