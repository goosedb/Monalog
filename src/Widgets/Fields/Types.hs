module Widgets.Fields.Types where

import Brick qualified as B
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Type.Event qualified as E
import Type.Field (Field (..), Path)
import Type.MaxWidth
import Type.Name
import Type.WidgetSize (WidgetSize)

data FieldState = FieldState
  { isSelected :: Bool
  , maxWidth :: MaxWidth
  }
  deriving (Generic, ToJSON)

data FieldsWidget = FieldsWidget
  { fields :: Map.Map Field FieldState
  , defaultFields :: Map.Map Field FieldState
  , width :: WidgetSize
  , layout :: FieldsViewLayout
  }
  deriving (Generic)

data FieldsViewLayout = Flatten | Nested
  deriving (Generic, FromJSON, ToJSON)

data ConfigSavingResult = SavedSuccessfully | SaveErrorHappened Text

data FieldsWidgetCallbacks s = FieldsWidgetCallbacks
  { fieldSelected :: MaxWidth -> Field -> B.EventM Name s ()
  , fieldsCreated :: [Path] -> B.EventM Name s ()
  , fieldUnselected :: Field -> B.EventM Name s ()
  , fieldsChangedMaxSize :: Map.Map Path MaxWidth -> B.EventM Name s ()
  , holdMouse :: Name -> B.Location -> B.EventM Name s ()
  , saveFieldsSet :: B.EventM Name s ()
  }

data FieldWidgetEvent
  = NewLog E.NewLog
  | Click FieldsWidgetName B.Location
  | SelectField Field
  | Move FieldsWidgetName B.Location B.Location
  | Scroll Int
  | AltScroll Int
  | CleanupFields

mkName :: FieldsWidgetName -> Name
mkName = WidgetName . FieldsWidgetName
