{-# LANGUAGE DerivingStrategies #-}

module Type.Name where

import Brick.Types qualified as B
import Data.Text (Text)
import Type.Field (Field)
import Type.Log (Idx)

newtype Name
  = WidgetName WidgetName
  deriving (Eq, Ord, Show)

data WidgetName
  = QueryWidgetName QueryWidgetName
  | FieldsWidgetName FieldsWidgetName
  | LogsViewWidgetName LogsViewWidgetName
  | LogViewWidgetName LogViewWidgetName
  | StatusBarWidgetName StatusBarWidgetName
  | DialogWidgetName DialogWidgetName
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
  | QueryWidgetErrorClear
  deriving (Eq, Ord, Show)

data FieldsWidgetName
  = FieldWidgetItself
  | FieldWidgetField Field
  | FieldWidgetLayoutButton
  | FieldWidgetSaveConfig
  | FieldWidgetViewport
  | FieldWidgetBorder
  | FieldWidgetHScrollBar B.ClickableScrollbarElement
  deriving (Eq, Ord, Show)

newtype LineNumber = MkLineNumber {rawLineNumber :: Int}
  deriving newtype (Eq, Ord, Show)

data LogsViewWidgetName
  = LogsViewWidgetItself
  | LogsViewWidgetLogEntry LineNumber Idx
  | LogsViewWidgetFieldMove (Either Int Int)
  | LogsViewWidgetFieldHeader Field
  | LogsViewWidgetNumberHeader
  | LogsViewWidgetFiller
  | LogsViewWidgetViewport
  | LogsViewWidgetLogs
  | LogsViewWidgetHScrollBar B.ClickableScrollbarElement
  deriving (Eq, Ord, Show)

data DialogWidgetName
  = DialogWidgetItself
  | DialogButton Text
  deriving (Eq, Ord, Show)

data LogViewWidgetName
  = LogViewWidgetViewport
  | LogViewWidgetCopyLog
  | LogViewWidgetCopyMethod
  | LogViewWidgetJsonpathEditor
  | LogViewWidgetJsonpathCheckbox
  | LogViewWidgetItself
  | LogViewWidgetBorder
  deriving (Eq, Ord, Show)
