module Widgets.Dialog.Types where

import Brick qualified as B
import Data.Text (Text)
import GHC.Generics (Generic)
import Type.Name

data DialogWidget s = DialogWidget
  { title :: Text
  , dialogMessage :: Text
  , actions :: [(Text, B.EventM Name s ())]
  , selected :: Int
  }
  deriving (Generic)

data DialogWidgetEvent
  = Click DialogWidgetName
  | SelectLeft
  | SelectRight
  | Accept

mkName :: DialogWidgetName -> Name
mkName = WidgetName . DialogWidgetName
