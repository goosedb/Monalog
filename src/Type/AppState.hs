module Type.AppState where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Consts
import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import GHC.Generics
import Type.Field (Field (..))
import Type.LogViewPosition (LogViewPosition (..))
import Type.MaxWidth (MaxWidth (MaxWidth))
import Type.Name qualified as N
import Type.WidgetSize (WidgetSize (..))
import Widgets.Dialog.Types (DialogWidget)
import Widgets.Editor (emptyEditor)
import Widgets.Fields.Types
import Widgets.LogView.Types (CopyMethod, LogViewWidget, emptyLogWidget)
import Widgets.LogsView.Types
import Widgets.Query.Types
import Widgets.StatusBar.Types (StatusBarStatus (..), StatusBarWidget (..))

data MouseState = Up | Down N.Name B.Location
  deriving (Eq, Show)

data AppState = AppState
  { logsView :: LogsViewWidget
  , logView :: LogViewWidget
  , statusBar :: StatusBarWidget
  , fieldsView :: FieldsWidget
  , queryView :: QueryWidget
  , mouseState :: MouseState
  , activeWidget :: [ActiveWidgetName]
  , dialogWidget :: Maybe (DialogWidget AppState)
  }
  deriving (Generic)

data ActiveWidgetName
  = QueryWidgetName
  | LogsWidgetName
  | LogWidgetName
  | FieldWidgetName
  | StatusBarWidgetName
  | DialogWidgetName
  deriving (Eq)

initialState :: Maybe String -> Maybe CopyMethod -> Maybe [Field] -> IO AppState
initialState copyCmd copyMethod defaultFields = do
  let defaultWidth = MaxWidth 1
  initialLogsView <- initLogsView (maybe [] (map (`SelectedField` defaultWidth)) defaultFields)
  let fields =
        flip Map.union initialFields
          . Map.fromList
          . map (,FieldState{isSelected = True, maxWidth = defaultWidth})
          . fromMaybe mempty
          $ defaultFields
  pure
    AppState
      { dialogWidget = Nothing
      , logsView = initialLogsView
      , logView =
          emptyLogWidget
            & #copyMethod %~ maybe id const copyMethod
            & #nativeCopyCmd .~ copyCmd
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
            , status = Idle
            }
      , fieldsView =
          FieldsWidget
            { fields = fields
            , defaultFields = fields
            , width = Auto
            , layout = Flatten
            }
      , queryView =
          QueryWidget
            { input = emptyEditor (N.WidgetName $ N.QueryWidgetName N.QueryWidgetEditor)
            , parseError = Nothing
            }
      , mouseState = Up
      , activeWidget = [LogsWidgetName]
      }
