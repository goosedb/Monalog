module Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Control.Lens
import Data.Generics.Labels ()
import Data.List (sortBy)
import Type.AppState as AS
import Type.AvailableSpace (AvailableSpace (..))
import Type.Name
import Type.TBool
import Widgets.Error.Ui (errorWidgetDraw)
import Widgets.Fields.Ui (fieldsWidgetDraw)
import Widgets.LogView.Ui (logViewWidgetDraw)
import Widgets.LogsView.Ui (logsViewWidgetDraw)
import Widgets.Query.Ui (queryWidgetDraw)
import Widgets.StatusBar.Types
import Widgets.StatusBar.Ui

drawUi :: AppState -> [B.Widget Name]
drawUi AppState{..} =
  [ case activeWidget of
      (AS.ErrorWidgetName w : _) -> errorWidgetDraw w
      _ -> B.emptyWidget
  , B.border
      . B.vBox
      $ [ B.padLeftRight 1 (queryWidgetDraw isQueryWidgetActive queryView)
        , B.hBorder
        , sortExtents
            . B.hBox
            $ [ fieldsWidgetDraw fieldsView
              , B.vBorder
              , B.Widget
                  { B.hSize = B.Greedy
                  , B.vSize = B.Greedy
                  , B.render = do
                      w <- view B.availWidthL
                      h <- view B.availHeightL
                      B.render case statusBar.logViewPosition of
                        LogViewPositionBottom ->
                          let availableForLogsView = case logView of
                                Just _ -> h `div` 2
                                Nothing -> h
                           in drawLogsViewCluster
                                B.vBox
                                B.hBorder
                                (AvailableSpace w availableForLogsView)
                                (AvailableSpace w (h - availableForLogsView))
                        LogViewPositionRight ->
                          let availableForLogView = w - availableForLogsView
                              availableForLogsView = case logView of
                                Just _ -> (w `div` 3) * 2
                                Nothing -> w
                           in drawLogsViewCluster
                                B.hBox
                                B.vBorder
                                (AvailableSpace availableForLogsView h)
                                (AvailableSpace availableForLogView h)
                  }
              ]
        ]
  ]
 where
  isQueryWidgetActive = case activeWidget of
    AS.QueryWidgetName : _ -> Is
    _ -> Isn't

  sortExtents widget =
    let extentSort B.Extent{extentName = WidgetName nameA} B.Extent{extentName = WidgetName nameB} =
          case (nameA, nameB) of
            (FieldsWidgetName _, LogsViewWidgetName _) -> GT
            (LogsViewWidgetName _, FieldsWidgetName _) -> LT
            _ -> EQ
     in B.Widget
          { vSize = B.vSize widget
          , hSize = B.hSize widget
          , B.render = do
              B.Result{..} <- B.render widget
              pure B.Result{extents = sortBy extentSort extents, ..}
          }

  drawLogsViewCluster box sep logsViewSpace logViewSpace =
    box
      [ drawLogsViewStatusBar logsViewSpace
      , case logView of
          Just widget ->
            box
              [ sep
              , drawLogView logViewSpace widget
              ]
          Nothing -> B.emptyWidget
      ]

  drawLogsViewStatusBar logsViewSpace =
    B.vBox
      [ B.vLimit (logsViewSpace.height - 1) do
          logsViewWidgetDraw (logsViewSpace & #height %~ subtract 1) logsView
      , B.padRight (B.Pad 1) $ statusBarWidgetDraw statusBar
      ]

  drawLogView logViewSpace widget =
    B.padLeftRight 1 $ logViewWidgetDraw (logViewSpace & #width %~ subtract 2) widget
