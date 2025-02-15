module Handler (handleEvent) where

import Brick qualified as B
import Brick.BChan qualified as B
import Config
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Graphics.Vty qualified as V
import Type.AppState
import Type.AppState qualified as AS
import Type.Event
import Type.Field (Field (..))
import Type.Log (Log (..))
import Type.MaxWidth (MaxWidth (MaxWidth))
import Type.Name
import Type.Name qualified as N
import Widgets.Dialog.Handler qualified as Dialog
import Widgets.Dialog.Types qualified as Dialog
import Widgets.Fields.Handler qualified as Fields
import Widgets.Fields.Types qualified as Fields
import Widgets.LogView.Handler qualified as LogView
import Widgets.LogView.Types (LogViewWidget (LogViewWidget), newLogView)
import Widgets.LogView.Types qualified as LogView
import Widgets.LogsView.Handler qualified as LogsView
import Widgets.LogsView.Types (LogsViewWidget (..))
import Widgets.LogsView.Types qualified as LogsView
import Widgets.Query.Handler qualified as Query
import Widgets.Query.Types qualified as Query
import Widgets.Query.Types qualified as QueryWidget
import Widgets.StatusBar.Handler qualified as StatusBar
import Widgets.StatusBar.Types qualified as StatusBar
import Widgets.Types (PackedLens' (PackedLens'))

{-# SCC handleEvent #-}
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
        callLogsViewWidget (LogsView.NewLog l.log)
      FilteredLogs (Right ls) -> forM_ ls \l -> do
        callLogsViewWidget (LogsView.FilteredAndSortedLog l)
      FilteredLogs (Left ls) ->
        callLogsViewWidget (LogsView.FilteredAndSortedLogs ls)
      ResetStatus tId -> callStatusBarWidget $ StatusBar.ResetStatus tId
      SelectFields fs -> do
        forM_ fs (callLogsViewWidget . LogsView.SelectField (MaxWidth 0))
        forM_ fs (callFieldsWidget . Fields.SelectField)
    B.MouseDown (WidgetName name) V.BLeft _ loc' | ms == Up -> do
      absoluteLoc <- translateToAbsolute (WidgetName name) loc'
      let canClick = case activeWidget of
            aw : _ -> not (isActiveWidgetDialog aw) || isDialogWidget name
            _ -> False
      when canClick do
        case name of
          N.QueryWidgetName qn -> do
            #activeWidget .= [AS.QueryWidgetName]
            callQueryWidget (Query.Click qn loc' absoluteLoc)
          N.LogsViewWidgetName ln -> do
            #activeWidget .= [AS.LogsWidgetName]
            callLogsViewWidget (LogsView.Click ln)
          N.LogViewWidgetName lvn -> do
            #activeWidget .= [AS.LogWidgetName]
            callLogViewWidget (LogView.Click lvn loc' absoluteLoc)
          N.FieldsWidgetName fn -> do
            #activeWidget .= [AS.FieldWidgetName]
            callFieldsWidget (Fields.Click fn absoluteLoc)
          N.DialogWidgetName en -> do
            callDialogWidget (Dialog.Click en)
          N.StatusBarWidgetName sbn -> do
            #activeWidget .= [AS.StatusBarWidgetName]
            callStatusBarWidget (StatusBar.Click sbn)
      #mouseState .= Down (WidgetName name) absoluteLoc
    B.MouseDown name V.BLeft _ loc
      | Down clickedName prevLoc <- ms -> do
          loc' <- translateToAbsolute name loc
          case clickedName of
            (WidgetName (FieldsWidgetName n)) -> do
              callFieldsWidget (Fields.Move n prevLoc loc')
            (WidgetName (LogsViewWidgetName n)) -> do
              callLogsViewWidget (LogsView.Move n prevLoc loc')
            (WidgetName (LogViewWidgetName n)) -> do
              callLogViewWidget (LogView.Move n prevLoc loc')
            _ -> pure ()
          #mouseState .= Down clickedName loc'
    B.MouseDown (WidgetName name) V.BScrollDown mods _ -> handleScroll name mods 1
    B.MouseDown (WidgetName name) V.BScrollUp mods _ -> handleScroll name mods (-1)
    B.MouseUp{} -> do
      case ms of
        Down (WidgetName (LogViewWidgetName _)) _ -> callLogViewWidget LogView.MoveStop
        _ -> pure ()
      #mouseState .= Up
    B.VtyEvent V.EvMouseUp{} -> #mouseState .= Up
    B.VtyEvent V.EvResize{} -> B.invalidateCache
    B.VtyEvent (V.EvKey V.KEsc _) | Right LogViewWidget{settings} <- logView -> do
      B.invalidateCache
      #logView .= Left settings
    B.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> B.halt
    B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
      dialog
        "Confirm action"
        "Are you sure you want to cleanup logs?"
        [("Yes", cleanupLogs), ("No", closeDialog)]
        1
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
              B.VtyEvent (V.EvKey k mods) -> Just $ LogView.Key k mods
              _ -> Nothing
        maybe (pure ()) callLogViewWidget mbEv
      AS.StatusBarWidgetName : _ -> do
        let mbEv = case e of
              B.VtyEvent (V.EvKey k mods) -> Just $ StatusBar.Key k mods
              _ -> Nothing
        maybe (pure ()) callStatusBarWidget mbEv
      AS.FieldWidgetName : _ -> pure ()
      AS.DialogWidgetName : _ -> case e of
        B.VtyEvent (V.EvKey V.KEnter _) -> callDialogWidget Dialog.Accept
        B.VtyEvent (V.EvKey V.KLeft _) -> callDialogWidget Dialog.SelectLeft
        B.VtyEvent (V.EvKey V.KRight _) -> callDialogWidget Dialog.SelectRight
        _ -> pure ()
      _ -> error "impossible!"
 where
  translateToAbsolute name (B.Location (c, r)) = do
    B.Extent{extentUpperLeft = B.Location (ec, er)} <- fromJust <$> B.lookupExtent name
    mbViewport <- B.lookupViewport name
    let loc = B.Location (ec + c - maybe 0 B._vpLeft mbViewport, er + r - maybe 0 B._vpTop mbViewport)
    pure loc

  handleScroll name mods i = case name of
    N.QueryWidgetName _ -> pure ()
    N.LogsViewWidgetName _ -> callLogsViewWidget (logsScrollEvent i mods)
    N.LogViewWidgetName _ -> callLogViewWidget (LogView.Scroll i)
    N.FieldsWidgetName _ ->
      callFieldsWidget $
        (if V.MCtrl `elem` mods then Fields.AltScroll else Fields.Scroll) i
    N.DialogWidgetName _ -> pure ()
    N.StatusBarWidgetName _ -> pure ()

  logsScrollEvent i mods
    | V.MCtrl `elem` mods = LogsView.AltScroll i
    | otherwise = LogsView.Scroll i

  logScrollUpEvent = LogView.Scroll (-1)
  logScrollDownEvent = LogView.Scroll 1

  callQueryWidget = Query.queryWidgetHandleEvent
    do #queryView
    do queryWidgetCallbacks

  callLogViewWidget a = do
    StatusBar.StatusBarWidget{..} <- use #statusBar
    LogView.logViewWidgetHandleEvent
      logViewPosition
      (lens (either (error "illigal state") id . (.logView)) (\s v -> s{logView = Right v}))
      LogView.LogViewWidgetCallbacks
        { copied = callStatusBarWidget StatusBar.SetCopied
        , copyError = errorDialog
        , addFilter = \k v -> callQueryWidget $ QueryWidget.AddFilter k v
        }
      a

  callStatusBarWidget =
    StatusBar.statusBarWidgetHandleEvent
      ch
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
                logView <- use #logView
                bool B.invalidateCache (pure ()) (isRight logView == isJust l)
                let settings = either id (.settings) logView
                maybe
                  (#logView .= Left settings)
                  (\Log{..} -> (#logView .= Right (newLogView value settings)) >> callLogViewWidget LogView.LogSelected)
                  l
            , resetFollow = callStatusBarWidget StatusBar.ResetFollow
            , goToRequest = do
                #activeWidget .= [AS.StatusBarWidgetName]
                callStatusBarWidget StatusBar.ActivateEditor
            , addSort = callQueryWidget . Query.AddSort
            }
        ?widgetState = PackedLens' #logsView
     in LogsView.logsViewWidgetHandleEvent

  callDialogWidget = Dialog.dialogWidgetHandleEvent #dialogWidget

  isActiveWidgetDialog = \case
    AS.DialogWidgetName -> True
    _ -> False

  isDialogWidget = \case
    N.DialogWidgetName{} -> True
    _ -> False

  queryWidgetCallbacks =
    Query.QueryWidgetCallbacks
      { Query.execFilter = \q -> do
          activateLogsView
          callLogsViewWidget $ LogsView.RunQuery (B.writeBChan ch . FilteredLogs) q
      , Query.clearFilter = callLogsViewWidget LogsView.ClearFilter
      , Query.showError = errorDialog
      }

  fieldsWidgetCallbacks =
    Fields.FieldsWidgetCallbacks
      { Fields.fieldSelected = \mw f -> callLogsViewWidget $ LogsView.SelectField mw f
      , Fields.fieldsCreated = callQueryWidget . Query.NewFields
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
      , Fields.saveFieldsSet = do
          LogsViewWidget{..} <- use #logsView
          result <- liftIO $ updateLocalConfig #columns do
            const $ map (\sf -> JsonField sf.field) selectedFields
          either (errorDialog . dumpErrorToText) (const $ callStatusBarWidget StatusBar.ConfigSaved) result
      , Fields.holdMouse = \n l -> #mouseState .= Down n l
      }

  activateLogsView = #activeWidget .= [AS.LogsWidgetName]

  cleanupLogs = do
    callLogsViewWidget LogsView.CleanupLogs
    callFieldsWidget Fields.CleanupFields
    settings <- uses #logView (either id (.settings))
    #logView .= Left settings
    closeDialog

  dialog title text actions selected = do
    #activeWidget %= (AS.DialogWidgetName :)
    #dialogWidget
      .= Just
        ( Dialog.DialogWidget
            title
            text
            actions
            selected
        )

  closeDialog = do
    #activeWidget %= filter \case
      AS.DialogWidgetName -> False
      _ -> True
    #dialogWidget .= Nothing

  errorDialog txt =
    dialog
      "Error"
      txt
      [ ("Ok", closeDialog)
      ]
      0
