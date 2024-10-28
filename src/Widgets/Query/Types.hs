module Widgets.Query.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Aeson (Value)
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as Nel
import Data.Text (Text)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Query
import Type.Field (Path)
import Type.Name

data QueryWidget = QueryWidget
  { input :: B.Editor Text Name
  , parseError :: Maybe Text
  , fields :: [Path]
  , hint :: Maybe Hint
  }
  deriving (Generic)

data Hint = Hint
  { span :: Span
  , selected :: Maybe Int
  , completions :: Nel.NonEmpty Path
  }
  deriving (Show)

data QueryWidgetCallbacks s = QueryWidgetCallbacks
  { execFilter :: Query -> B.EventM Name s ()
  , clearFilter :: B.EventM Name s ()
  , showError :: Text -> B.EventM Name s ()
  }

data QueryWidgetEvent
  = Key V.Key [V.Modifier]
  | Click QueryWidgetName B.Location B.Location
  | NewFields [Path]
  | AddFilter [Text] Value
  | ActivateEditor

mkName :: QueryWidgetName -> Name
mkName = WidgetName . QueryWidgetName

queryWidgetName :: Name
queryWidgetName = mkName QueryWidgetItself
