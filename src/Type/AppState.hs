module Type.AppState where

import Brick.Widgets.Edit qualified as B
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Vector.Mutable qualified as MVec
import GHC.Generics
import Type.Field (Field (..))
import Type.Log (Logs (ImmutableLogs, MutableLogs))
import Type.MaxWidth (MaxWidth (MaxWidth))
import Type.Name qualified as N
import Widgets.Error.Types (ErrorWidget)
import Widgets.Fields.Types
import Widgets.LogView.Types (LogViewWidget)
import Widgets.LogsView.Types
import Widgets.Query.Types
import Widgets.StatusBar.Types (LogViewPosition (..), StatusBarWidget (..))

data MouseState = Up | Down
  deriving (Eq, Show)

data AppState = AppState
  { logsView :: LogsViewWidget
  , logView :: Maybe LogViewWidget
  , statusBar :: StatusBarWidget
  , fieldsView :: FieldsWidget
  , queryView :: QueryWidget
  , mouseState :: MouseState
  , activeWidget :: [ActiveWidgetName]
  }
  deriving (Generic)

data ActiveWidgetName
  = QueryWidgetName
  | LogsWidgetName
  | LogWidgetName
  | FieldWidgetName
  | ScrollBarWidgetName
  | ErrorWidgetName ErrorWidget
  deriving (Eq)

initialState :: IO AppState
initialState = do
  allLogs <- MVec.new 64
  filteredLogs <- MVec.new 1
  let initialFollowLogs = True
  let initialTopLine = 1
  pure
    AppState
      { logsView =
          LogsViewWidget
            { allLogs = MutableLogs allLogs 0
            , filteredLogs = MutableLogs filteredLogs 0
            , selectedLog = Nothing
            , selectedFields = []
            , visibleLogs = ImmutableLogs mempty
            , topLine = initialTopLine
            , activeLogs = All
            , filterQueue = Nothing
            , killFilterWorker = pure ()
            , followLogs = initialFollowLogs
            }
      , logView = Nothing
      , statusBar =
          StatusBarWidget
            { totalLines = 0
            , topLineEditor =
                B.editorText
                  (N.WidgetName $ N.StatusBarWidgetName N.StatusBarWidgetEditor)
                  (Just 1)
                  (fromString $ show initialTopLine)
            , isEditorActive = False
            , topLine = initialTopLine
            , followLogs = initialFollowLogs
            , logViewPosition = LogViewPositionRight
            }
      , fieldsView =
          FieldsWidget
            { fields =
                Map.fromList
                  [ (Timestamp, FieldState{isSelected = False, maxWidth = MaxWidth 8})
                  , (Raw, FieldState{isSelected = False, maxWidth = MaxWidth 5000})
                  ]
            }
      , queryView =
          QueryWidget
            { input = B.editorText (N.WidgetName $ N.QueryWidgetName N.QueryWidgetEditor) (Just 1) ""
            , parseError = Nothing
            }
      , mouseState = Up
      , activeWidget = [LogsWidgetName]
      }
