module Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Core ((<+>))
import Control.Lens hiding (at)
import Data.Generics.Labels ()
import Data.List (sortBy)
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Query (Pos (..), Span (..))
import Type.AppState qualified as AS
import Type.AvailableSpace (AvailableSpace (..))
import Type.Field (textPath)
import Type.LogViewPosition (LogViewPosition (..))
import Type.Name
import Type.TBool
import Type.WidgetSize (WidgetSize (..))
import Widgets.Dialog.Ui (dialogWidgetDraw)
import Widgets.Fields.Types
import Widgets.Fields.Ui (fieldsWidgetDraw)
import Widgets.LogView.Types (LogViewWidget (..), LogViewWidgetSettings (..))
import Widgets.LogView.Ui (logViewWidgetDraw)
import Widgets.LogsView.Ui (logsViewWidgetDraw)
import Widgets.Query.Types
import Widgets.Query.Ui (queryWidgetDraw)
import Widgets.StatusBar.Types
import Widgets.StatusBar.Ui
import Prelude hiding (span)

drawUi :: AS.AppState -> [B.Widget Name]
drawUi AS.AppState{..} =
  [ case activeWidget of
      (AS.DialogWidgetName : _) -> maybe B.emptyWidget dialogWidgetDraw dialogWidget
      _ -> B.emptyWidget
  , case queryView.hint of
      Just Hint{..}
        | Is <- isQueryWidgetActive ->
            B.relativeTo
              (WidgetName . QueryWidgetName $ QueryWidgetEditor)
              (B.Location (span.from.column - 1, span.from.row))
              let txtHints = map textPath (Nel.toList completions)
                  highlightSelected i t = B.hBox $ map B.txt if Just i == selected then [">>", t, " "] else ["  ", t, " "]
               in B.withAttr (B.attrName "hint")
                    . B.vLimit (Nel.length completions + 1)
                    . B.hLimit (maximum (map Text.length txtHints) + 3)
                    $ B.vBox (zipWith highlightSelected [0 ..] txtHints)
      _ -> B.emptyWidget
  , sortExtents
      . B.border
      . B.vBox
      $ [ B.padLeftRight 1 (queryWidgetDraw isQueryWidgetActive queryView)
        , B.hBorder
        , B.hBox
            [ B.vBox
                [ (case fieldsView.width of Auto -> B.hLimitPercent 10; Manual w -> B.hLimit w) $
                    fieldsWidgetDraw fieldsView
                ]
            , B.clickable (WidgetName $ FieldsWidgetName FieldWidgetBorder) B.vBorder
            , B.Widget
                { B.hSize = B.Greedy
                , B.vSize = B.Greedy
                , B.render = do
                    w' <- view B.availWidthL
                    h' <- view B.availHeightL
                    B.render case statusBar.logViewPosition of
                      LogViewPositionBottom ->
                        let availableForLogsView = case logView of
                              Right LogViewWidget{settings = LogViewWidgetSettings{height}} -> case height of
                                Auto -> h' `div` 2
                                Manual i -> h' - i
                              _ -> h'
                         in drawLogsViewCluster
                              B.vBox
                              B.hBorder
                              (AvailableSpace w' availableForLogsView)
                              (AvailableSpace w' (h' - availableForLogsView))
                      LogViewPositionRight ->
                        let availableForLogView = w' - availableForLogsView
                            availableForLogsView = case logView of
                              Right LogViewWidget{settings = LogViewWidgetSettings{width}} -> case width of
                                Auto -> (w' `div` 3) * 2
                                Manual i -> w' - i
                              _ -> w'
                         in drawLogsViewCluster
                              B.hBox
                              B.vBorder
                              (AvailableSpace availableForLogsView h')
                              (AvailableSpace availableForLogView h')
                }
            ]
        ]
  ]
 where
  isQueryWidgetActive = case activeWidget of
    AS.QueryWidgetName : _ -> Is
    _ -> Isn't

  isLogWidgetActive = case activeWidget of
    AS.LogWidgetName : _ -> Is
    _ -> Isn't

  sortExtents widget =
    let extentSort B.Extent{extentName = nameA} B.Extent{extentName = nameB} =
          case (nameA, nameB) of
            (WidgetName (QueryWidgetName _), WidgetName (FieldsWidgetName _)) -> GT
            (WidgetName (QueryWidgetName _), WidgetName (LogsViewWidgetName _)) -> LT
            (WidgetName (FieldsWidgetName _), WidgetName (QueryWidgetName _)) -> LT
            (WidgetName (FieldsWidgetName _), WidgetName (LogsViewWidgetName _)) -> GT
            (WidgetName (LogsViewWidgetName _), WidgetName (FieldsWidgetName _)) -> LT
            (WidgetName (LogsViewWidgetName _), WidgetName (QueryWidgetName _)) -> LT
            (WidgetName (FieldsWidgetName FieldWidgetLayoutButton), WidgetName (FieldsWidgetName _)) -> GT
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
      , let name = WidgetName . LogViewWidgetName
         in case logView of
              Right w ->
                box
                  [ B.clickable (name LogViewWidgetBorder) sep
                  , drawLogView logViewSpace w
                  ]
              _ -> B.emptyWidget
      ]

  drawLogsViewStatusBar logsViewSpace =
    B.vBox
      [ B.vLimit (logsViewSpace.height - 1) do
          logsViewWidgetDraw (logsViewSpace & #height %~ subtract 1) logsView
      , B.padRight (B.Pad 1) $ statusBarWidgetDraw statusBar
      ]

  drawLogView logViewSpace widget =
    B.padLeftRight 1 $ logViewWidgetDraw isLogWidgetActive (logViewSpace & #width %~ subtract 2) widget
