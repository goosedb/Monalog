module Widgets.LogView.Types where

import GHC.Generics
import Type.Log (Log)
import Type.Name

newtype LogViewWidget = LogViewWidget
  {selectedLog :: Log}
  deriving (Generic)

data LogViewWidgetEvent
  = LogSelected Log
  | Scroll Int

mkName :: LogViewWidgetName -> Name
mkName = WidgetName . LogViewWidgetName
