module Widgets.Error.Types where

import Brick qualified as B
import Data.Text (Text)
import Type.Name

newtype ErrorWidget = ErrorWidget
  {errorMessage :: Text}
  deriving (Eq)

newtype ErrorWidgetEvent
  = Click ErrorWidgetName

newtype ErrorWidgetCallbacks s = ErrorWidgetCallbacks
  {closeErrorWidget :: B.EventM Name s ()}

mkName :: ErrorWidgetName -> Name
mkName = WidgetName . ErrorWidgetName
