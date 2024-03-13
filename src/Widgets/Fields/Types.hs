module Widgets.Fields.Types where

import Brick qualified as B
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Type.Field (Field, Path)
import Type.Log (Log)
import Type.MaxWidth
import Type.Name

data FieldState = FieldState
  { isSelected :: Bool
  , maxWidth :: MaxWidth
  }

newtype FieldsWidget = FieldsWidget
  { fields :: Map.Map Field FieldState
  }
  deriving (Generic)

data FieldsWidgetCallbacks s = FieldsWidgetCallbacks
  { fieldSelected :: MaxWidth -> Field -> B.EventM Name s ()
  , fieldUnselected :: Field -> B.EventM Name s ()
  , fieldsChangedMaxSize :: Map.Map Path MaxWidth -> B.EventM Name s ()
  }

data FieldWidgetEvent
  = NewLog Log
  | Click FieldsWidgetName
  | Scroll Int

mkName :: FieldsWidgetName -> Name
mkName = WidgetName . FieldsWidgetName
