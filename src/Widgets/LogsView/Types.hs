module Widgets.LogsView.Types where

import Brick qualified as B
import Brick.BChan qualified as B
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TQueue)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Query (Query)
import Type.Event qualified as E
import Type.Field (Field (..))
import Type.Log as Logs
import Type.MaxWidth
import Type.Name

data SelectedField = SelectedField
  { field :: Field
  , width :: MaxWidth
  }

data ActiveLogs = All | Filtered
  deriving (Eq)

data LogsViewWidget = LogsViewWidget
  { allLogs :: Logs Mutable
  , filteredLogs :: Logs Mutable
  , selectedLog :: Maybe (LineNumber, Log)
  , selectedFields :: [SelectedField]
  , visibleLogs :: Logs Immutable
  , topLine :: Int
  , activeLogs :: ActiveLogs
  , filterQueue :: Maybe (TQueue Log)
  , filterWorker :: Maybe ThreadId
  , followLogs :: Bool
  }
  deriving (Generic)

data LogsWidgetEvent
  = Click LogsViewWidgetName
  | Scroll Int
  | AltScroll Int
  | Key V.Key [V.Modifier]
  | NewLog Log
  | FilteredLog Log
  | RunFilter (B.BChan E.Event) Query
  | ClearFilter
  | GoToTop
  | GoToBottom
  | GoTo Int
  | FollowLogs Bool

data FieldWidth = FieldWidth
  { idx :: Int
  , width :: Int
  , field :: Field
  }

data LogsViewWidgetCallbacks s = LogsViewWidgetCallbacks
  { topLineChanged :: ~(Int -> B.EventM Name s ())
  , logAddedToView :: ~(B.EventM Name s ())
  , totalLinesChanged :: ~(Int -> B.EventM Name s ())
  , selectedLog :: ~(Maybe Log -> B.EventM Name s ())
  , selectedLogChanged :: ~(Log -> B.EventM Name s ())
  , resetFollow :: ~(B.EventM Name s ())
  }

mkName :: LogsViewWidgetName -> Name
mkName = WidgetName . LogsViewWidgetName

minimalTopLine :: Int
minimalTopLine = 1
