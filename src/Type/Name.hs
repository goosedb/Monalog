module Type.Name where

import Type.Field (Field)

newtype Name
  = WidgetName WidgetName
  deriving (Eq, Ord, Show)

data WidgetName
  = QueryWidgetName QueryWidgetName
  | FieldsWidgetName FieldsWidgetName
  | LogsViewWidgetName LogsViewWidgetName
  | LogViewWidgetName LogViewWidgetName
  | StatusBarWidgetName StatusBarWidgetName
  | ErrorWidgetName ErrorWidgetName
  deriving (Eq, Ord, Show)

data StatusBarWidgetName
  = StatusBarWidgetLogSide
  | StatusBarWidgetEditor
  | StatusBarWidgetGoToTop
  | StatusBarWidgetGoToBottom
  | StatusBarWidgetFollow
  deriving (Eq, Ord, Show)

data QueryWidgetName
  = QueryWidgetItself
  | QueryWidgetEditor
  | QueryWidgetErrorHint
  deriving (Eq, Ord, Show)

data FieldsWidgetName
  = FieldWidgetItself
  | FieldWidgetField Field
  | FieldWidgetViewport
  deriving (Eq, Ord, Show)

data LogsViewWidgetName
  = LogsViewWidgetItself
  | LogsViewWidgetLogEntry Int
  | LogsViewWidgetFieldMove (Either Int Int)
  | LogsViewWidgetFieldHeader Field
  | LogsViewWidgetNumberHeader
  | LogsViewWidgetFiller
  | LogsViewWidgetViewport
  | LogsViewWidgetLogs
  deriving (Eq, Ord, Show)

data ErrorWidgetName
  = ErrorWidgetOk
  | ErrorWidgetItself
  deriving (Eq, Ord, Show)

data LogViewWidgetName
  = LogViewWidgetViewport
  | LogViewWidgetItself
  deriving (Eq, Ord, Show)
